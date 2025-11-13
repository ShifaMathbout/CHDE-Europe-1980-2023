# --- Trend tests & maps for CHDEs ------------------------------
# Requires CSVs with the following minimal schemas:
# 1) CHDEs_MMKT.csv           -> columns: value   (numeric series)
# 2) Dryspells_Trend.csv      -> columns: value   (numeric series)
# 3) yearly_means_arranged.csv-> columns: Station,x,y,<YYYY>... (years as columns)
# 4) CHDEs_Stattests.csv      -> columns: Perc,Days,model,var   (model in {"1980-2001","2002-2023"})

# ---- Libraries -------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
library(Kendall)     # MannKendall()
library(zyp)         # zyp.sen()
library(trend)       # mk.test()
library(modifiedmk)  # mmkh()
library(patchwork)   # plot_layout()
library(grid)        # unit()

# ---- 0. Helpers ------------------------------------------------
dir.create("outputs", showWarnings = FALSE)

# ---- 1) Modified Mann–Kendall on a single series ---------------
# File: CHDEs_MMKT.csv  | column: value (numeric)
dt_mk <- fread("CHDEs_MMKT.csv")
stopifnot("value" %in% names(dt_mk))

mk_res <- mk.test(dt_mk$value)                 # classical MK from 'trend'
mmk_res <- mmkh(dt_mk$value, ci = 0.95)        # modified MK (autocorr. aware)

data.frame(
  test = c("MK_trend", "MMK_trend"),
  tau_or_Z = c(unname(mk_res$estimates[["tau"]]), NA_real_),
  p_value  = c(mk_res$p.value, mmk_res$p.value)
) |>
  fwrite("outputs/CHDEs_MK_results.csv")

# ---- 2) Modified MK on Dryspells series (single column) --------
# File: Dryspells_Trend.csv | column: value (numeric)
dt_dry <- fread("Dryspells_Trend.csv")
stopifnot("value" %in% names(dt_dry))

mmk_dry <- mmkh(dt_dry$value, ci = 0.95)
data.frame(
  test = "MMK_dryspells",
  p_value = mmk_dry$p.value,
  Z = mmk_dry$Z
) |>
  fwrite("outputs/Dryspells_MMK_results.csv")

# ---- 3) Station-wise MK + Sen slope + map categories -----------
# File: yearly_means_arranged.csv
# Expected columns: Station,x,y,1980,1981,...,2023  (years as numeric column names)
df_wide <- fread("yearly_means_arranged.csv")
stopifnot(all(c("Station","x","y") %in% names(df_wide)))

# remove any 'X' prefixes from year columns like X1980 -> 1980
setnames(df_wide, old = names(df_wide), new = gsub("^X", "", names(df_wide)))
year_cols <- setdiff(names(df_wide), c("Station","x","y"))
years <- as.numeric(year_cols)
years <- years[!is.na(years)]

results <- lapply(seq_len(nrow(df_wide)), function(i){
  vals <- as.numeric(df_wide[i, ..year_cols])
  nn   <- !is.na(vals)
  vals <- vals[nn]
  yrs  <- years[nn]
  if (length(vals) < 3) {
    return(data.frame(
      Station = df_wide$Station[i], Trend = NA, P_value = NA,
      Slope = NA, Significance = NA, stringsAsFactors = FALSE))
  }
  mk <- MannKendall(vals)
  slope <- zyp.sen(vals ~ yrs)$coefficients[2]
  data.frame(
    Station = df_wide$Station[i],
    Trend = ifelse(mk$tau > 0, "increasing", "decreasing"),
    P_value = mk$sl,
    Slope = slope,
    Significance = ifelse(mk$sl < 0.05, "Significant", "Not Significant"),
    stringsAsFactors = FALSE
  )
})

res_df <- rbindlist(results, use.names = TRUE, fill = TRUE)
res_df$Trend_Category <- with(res_df,
  ifelse(P_value < 0.05 & Trend == "increasing", "Significant Increasing",
  ifelse(P_value < 0.05 & Trend == "decreasing", "Significant Decreasing",
         "Not Significant"))
)

fwrite(res_df, "outputs/Mann_Kendall_Results_by_station.csv")

# --- Quick raster-style point map (needs x,y in df_wide) --------
mapped <- merge(df_wide[, .(Station,x,y)], res_df, by = "Station")
col_map <- c("Significant Increasing" = "red",
             "Significant Decreasing" = "cornflowerblue",
             "Not Significant"       = "grey70")

p_map <- ggplot(mapped, aes(x = x, y = y)) +
  geom_point(aes(color = Trend_Category), size = 1.2) +
  scale_color_manual(values = col_map) +
  coord_equal() +
  labs(x = "", y = "", color = "") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("outputs/DEs_MMKT_Trend.png", p_map, width = 9, height = 5, dpi = 300)

# ---- 4) Period-difference tests: CvM, MWU, KS ------------------
# File: CHDEs_Stattests.csv | columns: Perc, Days, model, var
dt_stat <- fread("CHDEs_Stattests.csv")
stopifnot(all(c("Perc","Days","model","var") %in% names(dt_stat)))

alpha <- 0.05

# Cramér–von Mises
# (from 'cramer.test' in 'cramer' package; if you already ran elsewhere, skip)
if (!requireNamespace("cramer", quietly = TRUE)) {
  message("Package 'cramer' not installed; CvM section will be skipped.")
  cvm_df <- data.frame()
} else {
  cvm_df <- rbindlist(lapply(unique(dt_stat$Perc), function(perc){
    rbindlist(lapply(unique(dt_stat$Days), function(day){
      sub <- dt_stat[Perc == perc & Days == day]
      a <- sub[model == "1980-2001", var]
      b <- sub[model == "2002-2023", var]
      if (length(a) > 1 & length(b) > 1) {
        tst <- cramer::cramer.test(a, b)
        data.frame(Percentile = perc, Day = day,
                   CVM_Statistic = as.numeric(tst$statistic),
                   P_Value = as.numeric(tst$p.value),
                   Is_Significant = as.numeric(tst$p.value) <= alpha)
      } else {
        data.frame(Percentile = perc, Day = day,
                   CVM_Statistic = NA, P_Value = NA, Is_Significant = NA)
      }
    }))
  }))
  fwrite(cvm_df, "outputs/CramerVonMises_results.csv")
}

# Wilcoxon–Mann–Whitney
mwu_df <- rbindlist(lapply(unique(dt_stat$Perc), function(perc){
  rbindlist(lapply(unique(dt_stat$Days), function(day){
    sub <- dt_stat[Perc == perc & Days == day]
    if (length(unique(sub$model)) == 2) {
      tst <- wilcox.test(var ~ model, data = sub)
      data.frame(Percentile = perc, Day = day,
                 P_Value = as.numeric(tst$p.value),
                 Is_Significant = as.numeric(tst$p.value) <= alpha)
    } else {
      data.frame(Percentile = perc, Day = day, P_Value = NA, Is_Significant = NA)
    }
  }))
}))
fwrite(mwu_df, "outputs/MannWhitney_results.csv")

# Kolmogorov–Smirnov
ks_df <- rbindlist(lapply(unique(dt_stat$Perc), function(perc){
  rbindlist(lapply(unique(dt_stat$Days), function(day){
    a <- dt_stat[Perc == perc & Days == day & model == "1980-2001", var]
    b <- dt_stat[Perc == perc & Days == day & model == "2002-2023", var]
    if (length(a) > 1 & length(b) > 1) {
      tst <- ks.test(a, b)
      data.frame(Percentile = perc, Day = day,
                 P_Value = as.numeric(tst$p.value),
                 Is_Significant = as.numeric(tst$p.value) <= alpha)
    } else {
      data.frame(Percentile = perc, Day = day, P_Value = NA, Is_Significant = NA)
    }
  }))
}))
fwrite(ks_df, "outputs/KolmogorovSmirnov_results.csv")

# ---- Done -------------------------------------------------------
message("All analyses complete. Results saved in the 'outputs' folder.")
