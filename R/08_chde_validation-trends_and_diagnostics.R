# ===============================================================
# Script: 08_chde_validation-trends_and_diagnostics.R
# Purpose: 
#   - Validate EDI vs SPI (1/3/9) with density + top-disagreement points
#   - Plot CDFs of CHDEs (by duration & percentile)
#   - Plot annual CHDE trends with SD ribbons + CUSUM change-around-2000
#   - Thermopluviogram (May–Oct, July, August) with highlighted years
#   - Boxplots comparing 1980–2001 vs 2002–2023 (two formats)
# Data notes: input CSVs are precomputed tables from earlier steps
# Author: Shifa Mathbout
# ===============================================================

# ---- Libraries ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(ggpubr)
  library(dplyr)
  library(scales)
  library(MASS)       # kde2d
  library(tidyr)
})

# ---- 1) EDI vs SPI scatter/density (SPI-1, SPI-3, SPI-9) ----
# Input: Correlation.csv  with columns: EDI, SPI-1, SPI-3, SPI-9
edi_spi <- read.csv("Correlation.csv", check.names = FALSE)
names(edi_spi) <- trimws(names(edi_spi))
names(edi_spi) <- gsub("-", ".", names(edi_spi))   # -> SPI.1, SPI.3, SPI.9

indices <- c("SPI.1", "SPI.3", "SPI.9")

select_disagreement_points <- function(df, index, n = 1500) {
  df %>%
    mutate(disagreement = abs(EDI - .data[[index]])) %>%
    arrange(desc(disagreement)) %>%
    slice_head(n = n)
}
calc_mae     <- function(df, index) mean(abs(df[[index]] - df$EDI), na.rm = TRUE)
calc_pearson <- function(df, index) cor(df[[index]], df$EDI, use = "complete.obs")
calc_max_den <- function(df, index) {
  kd <- kde2d(df[[index]], df$EDI, n = 100)
  max(kd$z, na.rm = TRUE)
}

scatter_plots <- lapply(indices, function(ix) {
  hi <- select_disagreement_points(edi_spi, ix, n = 1500)
  mae <- calc_mae(edi_spi, ix)
  r   <- calc_pearson(edi_spi, ix)
  dpk <- calc_max_den(edi_spi, ix)

  ggplot(edi_spi, aes(x = .data[[ix]], y = EDI)) +
    stat_density2d(aes(fill = ..density..), geom = "tile", contour = FALSE, alpha = 0.6) +
    scale_fill_gradientn(
      colors = c("white","darkorchid1","springgreen","yellow","red","darkred"),
      values = rescale(c(0, .2, .4, .6, .8, 1)),
      limits = c(0, dpk), name = "Density"
    ) +
    geom_point(data = hi, aes(x = .data[[ix]], y = EDI),
               color = "black", alpha = 0.7, size = 1.5) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(
      title = paste("r =", round(r, 3), "| MAE =", round(mae, 3), "| d =", round(dpk, 3)),
      x = ix, y = "EDI"
    ) +
    theme_minimal(base_size = 20) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank()
    )
})

ggsave("08_scatter_edi_vs_spi_row.png",
       ggarrange(plotlist = scatter_plots, ncol = 3, nrow = 1),
       width = 24, height = 8, dpi = 600)

ggsave("08_scatter_edi_vs_spi_col.png",
       ggarrange(plotlist = scatter_plots, ncol = 1, nrow = 3),
       width = 8, height = 15, dpi = 600)

# ---- 2) CDFs of CHDEs (by duration & percentile) ----
# Input: CDF_CEs.csv with cols: var, model, Days (3/5/7 Day), Perc (P85/P90/P95)
cdf_raw <- read.csv("CDF_CEs.csv")
cdf_raw$Days <- factor(cdf_raw$Days, levels = c("3 Day","5 Day","7 Day"))

cdf_plot <- ggecdf(
  cdf_raw, x = "var", color = "model", linetype = "model", size = 1.2,
  palette = c("blue","red")
) +
  facet_grid(Days ~ Perc, labeller = labeller(
    Days = c("3 Day"="3 day","5 Day"="5 day","7 Day"="7 day"),
    Perc = c("P85"="85th Percentile","P90"="90th Percentile","P95"="95th Percentile")
  )) +
  labs(x = "Value", y = "Cumulative Probability", title = "") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50", size = 0.6) +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_rect(fill = "gray90", color = "black")
  )

ggsave("08_cdf_chde.png", plot = cdf_plot, width = 36, height = 30, units = "cm", dpi = 600)

# ---- 3) Annual CHDE trends + ribbons; top years; CUSUM ----
# Input: CE_Num_Trends.csv with cols: year, Perc (85P/90P/95P), CE
ce_trend <- read.csv("CE_Num_Trends.csv")

trend_summary <- ce_trend %>%
  group_by(year, Perc) %>%
  summarise(mean_CE = mean(CE, na.rm = TRUE),
            sd_CE   = sd(CE, na.rm = TRUE), .groups = "drop") %>%
  mutate(lower = mean_CE - sd_CE,
         upper = mean_CE + sd_CE)

p_trend <- ggplot(trend_summary, aes(x = year, group = Perc)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Perc), alpha = 0.25) +
  geom_line(aes(y = mean_CE, color = Perc), size = 1) +
  geom_point(aes(y = mean_CE, color = Perc), size = 1.2) +
  geom_smooth(aes(y = mean_CE, color = Perc), method = "lm", se = FALSE, size = 0.6, linetype = "dashed") +
  scale_color_manual(values = c("85P" = "#1f78b4", "90P" = "#33a02c", "95P" = "#e31a1c")) +
  scale_fill_manual(values  = c("85P" = "khaki",   "90P" = "orange1", "95P" = "red")) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = seq(1980, 2023, by = 5)) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom", legend.title = element_blank())

top_years <- trend_summary %>%
  group_by(year) %>%
  summarise(mean_all = mean(mean_CE, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_all)) %>% slice_head(n = 3) %>% pull(year)

p_trend <- p_trend + geom_vline(xintercept = top_years, linetype = "dashed", color = "black", size = 0.7)

# CUSUM
# Input: CUSUM Charts.csv with cols: year, Perc (85P/90P/95P), CE
cusum_df <- read.csv("CUSUM Charts.csv")
cusum_df$Perc <- gsub("P","", cusum_df$Perc)
cusum_df$Perc <- factor(cusum_df$Perc, levels = c("85","90","95"), labels = c("85P","90P","95P"))

cusum_df <- cusum_df %>%
  group_by(Perc) %>%
  mutate(target = mean(CE), CUSUM = cumsum(CE - target)) %>%
  ungroup() %>%
  mutate(ChangeGroup = ifelse(year < 2000, "Before change", "After change"))

trend_lines <- cusum_df %>%
  group_by(Perc, ChangeGroup) %>%
  summarise(
    slope = coef(lm(CUSUM ~ year))[2],
    intercept = coef(lm(CUSUM ~ year))[1],
    x_start = min(year), x_end = max(year),
    y_start = intercept + slope * x_start,
    y_end   = intercept + slope * x_end,
    .groups = "drop"
  )

mean_cusum <- cusum_df %>%
  group_by(Perc, ChangeGroup) %>%
  summarise(mean_val = mean(CUSUM), .groups = "drop")

p_cusum <- ggplot(cusum_df, aes(x = year, y = CUSUM, fill = ChangeGroup)) +
  geom_area(alpha = 0.7) +
  geom_vline(xintercept = 2000, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  geom_segment(data = mean_cusum, 
               aes(x = -Inf, xend = Inf, y = mean_val, yend = mean_val, color = ChangeGroup),
               inherit.aes = FALSE, linetype = "twodash", size = 0.8) +
  geom_segment(data = trend_lines,
               aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = ChangeGroup),
               arrow = arrow(length = unit(0.25, "inches")), size = 1, inherit.aes = FALSE) +
  scale_fill_manual(values = c("Before change" = "skyblue", "After change" = "tomato")) +
  scale_color_manual(values = c("Before change" = "blue", "After change" = "red"), guide = "none") +
  facet_wrap(~ Perc, scales = "free_y") +
  labs(x = NULL, y = "CUSUM", fill = NULL) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom")

combined_trend <- p_trend / p_cusum
ggsave("08_trends_plus_cusum.png", combined_trend, width = 18, height = 16, units = "in", dpi = 600)

# ---- 4) Thermopluviogram (May–Oct, July, August) ----
# Input: Temp_Prec.csv with cols: Date, Temperature, Precipitation
tp <- read.csv("Temp_Prec.csv")
tp$Date  <- as.Date(tp$Date)
tp$Year  <- as.integer(format(tp$Date, "%Y"))
tp$Month <- as.integer(format(tp$Date, "%m"))

thermo_fun <- function(df_month, title) {
  ag <- aggregate(cbind(Precipitation, Temperature) ~ Year, data = df_month, FUN = mean, na.rm = TRUE)
  ag$Precip_Anom_Pct <- (ag$Precipitation - mean(df_month$Precipitation, na.rm = TRUE)) /
                         mean(df_month$Precipitation, na.rm = TRUE) * 100
  ag$Temp_Anom <- ag$Temperature - mean(df_month$Temperature, na.rm = TRUE)
  ag$Period <- cut(ag$Year, breaks = c(1979, 1989, 2009, 2023, 2100),
                   labels = c("1980–1989","1990–2009","2010–2023","Later"))
  ggplot(ag, aes(x = Temp_Anom, y = Precip_Anom_Pct)) +
    geom_point(aes(color = Period, shape = Temp_Anom >= 0), size = 2) +
    geom_point(
      data = subset(ag, Year %in% c(2007, 2018, 2022, 2023)),
      aes(x = Temp_Anom, y = Precip_Anom_Pct),
      size = 6, shape = 1, color = "brown", stroke = 1.2, inherit.aes = FALSE
    ) +
    geom_hline(yintercept = 0, color = "black", size = 0.4) +
    geom_vline(xintercept = 0, color = "black", size = 0.4) +
    scale_color_manual(values = c("green","orange","red","grey40"), name = NULL,
                       breaks = c("1980–1989","1990–2009","2010–2023")) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17), guide = "none") +
    labs(x = "Temperature Anomaly (°C)", y = "Precipitation Anomaly (%)", title = title) +
    theme_bw(base_size = 16) +
    theme(legend.position = "bottom")
}

tp_may_oct <- subset(tp, Month %in% 5:10)
p_mo <- thermo_fun(tp_may_oct, "May–October")
p_jul <- thermo_fun(subset(tp, Month == 7), "July")
p_aug <- thermo_fun(subset(tp, Month == 8), "August")

thermo_all <- ggarrange(p_mo, p_jul, p_aug, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")
ggsave("08_thermopluviogram.png", thermo_all, width = 34, height = 18, units = "cm", dpi = 600)

# ---- 5) Boxplots: two periods (simple) ----
# Input: CE_Monthly_Number_Box.csv with cols: month ('(1980-2001)','(2002-2023)'), number, mean, prec
box1 <- read.csv("CE_Monthly_Number_Box.csv")
box1$month <- factor(box1$month, levels = c("(1980-2001)", "(2002-2023)"))

box_simple <- ggplot(box1, aes(x = month, y = number, fill = mean)) +
  geom_boxplot(notch = TRUE) +
  scale_fill_gradientn(
    colors = c("lightyellow","khaki","orange","darkorange3","red","red4"),
    limits = c(min(box1$mean, na.rm = TRUE), max(box1$mean, na.rm = TRUE)),
    name = "Event"
  ) +
  facet_wrap(~ prec) +
  scale_y_continuous(limits = c(0, 6)) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  labs(x = NULL, y = NULL)
ggsave("08_box_periods_basic.png", box_simple, width = 30, height = 19, units = "cm", dpi = 600)

# ---- 6) Boxplots: long-format, mean+median legend, faceted ----
# Input: CE_Monthly_Number_Box1.csv with cols: month, prec, number1..3, Duration1..3
box2 <- read.csv("CE_Monthly_Number_Box1.csv")
box2$month <- factor(box2$month, levels = c("(1980-2001)", "(2002-2023)"))

box2_long <- box2 %>%
  pivot_longer(
    cols = c(number1, number2, number3, Duration1, Duration2, Duration3),
    names_to = c(".value", "set"),
    names_pattern = "(number|Duration)(\\d)"
  )

box2_long$Duration <- factor(box2_long$Duration)

box_facet <- ggplot(box2_long, aes(x = month, y = number)) +
  geom_boxplot(fill = NA, color = "black", size = 1, outlier.colour = "red", outlier.size = 2) +
  stat_summary(aes(color = "Mean"), fun = mean, geom = "point", size = 3, show.legend = TRUE) +
  stat_summary(aes(color = "Median", linetype = "Median"), fun = median,
               geom = "crossbar", width = 0.4, fatten = 2, show.legend = TRUE) +
  scale_color_manual(values = c("Mean" = "darkgreen", "Median" = "blue")) +
  scale_linetype_manual(values = c("Median" = "solid")) +
  guides(color = guide_legend(title = NULL, override.aes = list(shape = c(16, NA))),
         linetype = "none") +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  facet_grid(Duration ~ prec, scales = "free_y") +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA),
    strip.background = element_rect(fill = "gray90", color = "black")
  ) +
  labs(x = "Time Period", y = "Number of Events",
       subtitle = "Event counts by duration and precipitation percentile")
ggsave("08_box_long_faceted.png", box_facet, width = 29, height = 28, units = "cm", dpi = 600)

# ---- 7) Optional: Change-point diagnostics (no export if not needed) ----
# Input: CE_Num_Trends.csv (same as trends). Prints CP years per percentile.
# To keep script focused, we compute and print only.
if (requireNamespace("changepoint", quietly = TRUE)) {
  library(changepoint)
  cp_df <- read.csv("CE_Num_Trends.csv")
  for (pp in unique(cp_df$Perc)) {
    sub <- subset(cp_df, Perc == pp)
    y   <- sub$CE
    yrs <- sub$year
    cpm <- try(cpts(cpt.mean(y, method = "AMOC", penalty = "MBIC")), silent = TRUE)
    cpv <- try(cpts(cpt.var(y,  method = "PELT", penalty = "MBIC")), silent = TRUE)
    cpmv<- try(cpts(cpt.meanvar(y, method = "BinSeg", Q = 5, penalty = "SIC")), silent = TRUE)
    cat("====", pp, "====\n")
    if (!inherits(cpm, "try-error") && length(cpm))  cat("Mean CP:", yrs[cpm], "\n")
    if (!inherits(cpv, "try-error") && length(cpv))  cat("Var  CP:", yrs[cpv], "\n")
    if (!inherits(cpmv,"try-error") && length(cpmv)) cat("Mean+Var CP:", yrs[cpmv], "\n\n")
  }
}
