##Mapping the Severity of CHDEs over 1980-2023

library(ggplot2)
library(ggpubr)
library(viridis)
library(grid)

# Read data
DEsev <- read.csv("All_Compound Events_Seve.csv", header = TRUE)
df1 <- as.data.frame(DEsev)
head(df1)

# Function to build styled CHDE plot
create_chde_plot <- function(data, var_column, title_label) {
ggplot(data, aes(x = x, y = y)) +
geom_raster(aes(fill = .data[[var_column]])) +
scale_fill_viridis(
option = "inferno",
name = "CHDE Severity",
breaks = c(0, 2, 4, 6, 8, 10, 12, 14),
na.value = "grey90",
direction = -1 ) +
labs(x = NULL, y = NULL, title = title_label) +
theme_minimal(base_size = 12) +
theme(
axis.title.x = element_text(family = "Times New Roman", size = 16),
axis.title.y = element_text(family = "Times New Roman", size = 16),
axis.text = element_text(family = "Times New Roman", size = 14),
axis.ticks = element_line(size = 0.4),
legend.position = "bottom",
legend.key.height = unit(0.5, "cm"),
legend.key.width = unit(14, "cm"),
legend.title = element_text(family = "Times New Roman", size = 18, face = "bold"),
legend.text = element_text(family = "Times New Roman", size = 16),
plot.title = element_text(hjust = 0.5, family = "Times New Roman", size = 18, face = "bold"),
panel.grid = element_blank(),
panel.border = element_blank(),            # <- Border removed here
plot.background = element_blank() ) +
guides(fill = guide_colorbar(
title.position = "top",
title.hjust = 0.5,
barwidth = unit(14, "cm"),
barheight = unit(0.8, "cm"),
ticks.colour = "black",
frame.colour = "black"  ))}

# Create plots
DE85 <- create_chde_plot(df1, "X85P", "85P")
DE90 <- create_chde_plot(df1, "X90P", "90P")
DE95 <- create_chde_plot(df1, "X95P", "95P")

# Arrange in a single row
grouped_plots <- ggarrange(
DE85, DE90, DE95,
ncol = 3, nrow = 1,
common.legend = TRUE,
legend = "bottom")

# Show plot
print(grouped_plots)

# Save high-resolution image at 1000 DPI using cm units
ggsave(filename = "Compound Events Maps_Severity.png",
plot = grouped_plots,
width = 35.6,       # Width in cm
height = 12.7,      # Height in cm
units = "cm",       # Specify units
dpi = 1000 )
################################################################################3

##Mapping the frequency of CHDEs over 1980-2023

library(ggplot2)
library(ggpubr)
library(scales)

# Read data
CEs <- read.csv("All_Compound Events_Freq.csv", header = TRUE)
df <- as.data.frame(CEs)
head(df)

# Color palette you requested
dry_hot_colors <- c("wheat","yellow", "orange", "red", "indianred4", "salmon4", "darkblue")

# Base theme with Times New Roman size 18 fonts and wider legend keys
base_plot_theme <- theme_minimal(base_size = 18) +
theme(
plot.title = element_text(family = "Times New Roman", size = 18, face = "bold", hjust = 0.5),
axis.title.x = element_blank(),
axis.title.y = element_blank(),
axis.text.x = element_text(family = "Times New Roman", size = 18),
axis.text.y = element_text(family = "Times New Roman", size = 18),
legend.text = element_text(family = "Times New Roman", size = 18, face = "bold"),
legend.title = element_text(family = "Times New Roman", size = 18, face = "bold"),
axis.ticks = element_line(size = 0.5),
plot.background = element_blank(),
panel.border = element_blank(),
legend.key.size = unit(1, 'cm'),
legend.key.height = unit(0.4, 'cm'),
legend.key.width = unit(6, 'cm'),  # wider legend keys
legend.position = "bottom",
legend.direction = "horizontal" )

# Function to create CE plots with titles and improved legend
create_ce_plot <- function(df, column, plot_title) {
ggplot(df, aes(x = x, y = y, fill = .data[[column]])) +
geom_raster() +
scale_fill_gradientn(
name = "Frequency %",
colors = dry_hot_colors,
limits = c(0, 70),                # fixed limits for legend
breaks = seq(0, 70, by = 10),    # breaks every 10%
labels = scales::label_number(accuracy = 1),
guide = guide_colorbar(
barwidth = 38,       # wider legend bar
barheight = 1.5,
direction = "horizontal",
title.position = "top",
title.hjust = 0.5,
label.position = "bottom",
ticks.colour = "black",
frame.colour = "black"  ),
rescaler = scales::rescale  ) +
labs(title = plot_title, x = NULL, y = NULL) +
base_plot_theme}

# Generate plots with titles
CE_3Day <- create_ce_plot(df, "Day_3_85", "3 Day_85P")
CE_5Day <- create_ce_plot(df, "Day_5_85", "5 Day_85P")
CE_7Day <- create_ce_plot(df, "Day_7_85", "7 Day_85P")

Day_3_90 <- create_ce_plot(df, "Day_3_90", "3 Day_90P")
Day_5_90 <- create_ce_plot(df, "Day_5_90", "5 Day_90P")
Day_7_90 <- create_ce_plot(df, "Day_7_90", "7 Day_90P")

Day_3_95 <- create_ce_plot(df, "Day_3_95", "3 Day_95P")
Day_5_95 <- create_ce_plot(df, "Day_5_95", "5 Day_95P")
Day_7_95 <- create_ce_plot(df, "Day_7_95", "7 Day_95P")

# Arrange the plots into a 3x3 grid
grouped_plots <- ggarrange(
CE_3Day, CE_5Day, CE_7Day,
Day_3_90, Day_5_90, Day_7_90,
Day_3_95, Day_5_95, Day_7_95,
ncol = 3, nrow = 3,
common.legend = TRUE,
legend = "bottom")

# Display the grouped plots
print(grouped_plots)

##Save

ggsave("Compound Events Maps_Frequency.png", plot=grouped_plots, height=29, width=35, units=c("cm"), dpi=1000)

###########################################

##Mapping the CHDEs number over 1980-2023

library(ggplot2)
library(ggpubr)
library(scales)

# Read data
CEs <- read.csv("All_Compound Events_number.csv", header = TRUE)
df <- as.data.frame(CEs)
head(df)

# Color palette you requested
dry_hot_colors <- c("wheat","yellow", "orange", "red", "indianred4", "salmon4", "darkblue")

# Base theme with Times New Roman size 18 fonts and wider legend keys
base_plot_theme <- theme_minimal(base_size = 18) +
theme(
plot.title = element_text(family = "Times New Roman", size = 18, face = "bold", hjust = 0.5),
axis.title.x = element_blank(),
axis.title.y = element_blank(),
axis.text.x = element_text(family = "Times New Roman", size = 18),
axis.text.y = element_text(family = "Times New Roman", size = 18),
legend.text = element_text(family = "Times New Roman", size = 18, face = "bold"),
legend.title = element_text(family = "Times New Roman", size = 18, face = "bold"),
axis.ticks = element_line(size = 0.5),
plot.background = element_blank(),
panel.border = element_blank(),
legend.key.size = unit(1, 'cm'),
legend.key.height = unit(0.4, 'cm'),
legend.key.width = unit(6, 'cm'),  # wider legend keys
legend.position = "bottom",
legend.direction = "horizontal" )

# Function to create CE plots with titles and improved legend
create_ce_plot <- function(df, column, plot_title) {
ggplot(df, aes(x = x, y = y, fill = .data[[column]])) +
geom_raster() +
scale_fill_gradientn(
name = "Event/Year",
colors = dry_hot_colors,
limits = c(0, 3),                # fixed limits for legend
breaks = seq(0, 3, by = 0.5),    # breaks every 0.25%
labels = scales::label_number(accuracy = 0.1),
guide = guide_colorbar(
barwidth = 38,       # wider legend bar
barheight = 1.5,
direction = "horizontal",
title.position = "top",
title.hjust = 0.5,
label.position = "bottom",
ticks.colour = "black",
frame.colour = "black"  ),
rescaler = scales::rescale  ) +
labs(title = plot_title, x = NULL, y = NULL) +
base_plot_theme}

# Generate plots with titles
CE_3Day <- create_ce_plot(df, "Day_3_85", "3 Day_85P")
CE_5Day <- create_ce_plot(df, "Day_5_85", "5 Day_85P")
CE_7Day <- create_ce_plot(df, "Day_7_85", "7 Day_85P")

Day_3_90 <- create_ce_plot(df, "Day_3_90", "3 Day_90P")
Day_5_90 <- create_ce_plot(df, "Day_5_90", "5 Day_90P")
Day_7_90 <- create_ce_plot(df, "Day_7_90", "7 Day_90P")

Day_3_95 <- create_ce_plot(df, "Day_3_95", "3 Day_95P")
Day_5_95 <- create_ce_plot(df, "Day_5_95", "5 Day_95P")
Day_7_95 <- create_ce_plot(df, "Day_7_95", "7 Day_95P")

# Arrange the plots into a 3x3 grid
grouped_plots1 <- ggarrange(
CE_3Day, CE_5Day, CE_7Day,
Day_3_90, Day_5_90, Day_7_90,
Day_3_95, Day_5_95, Day_7_95,
ncol = 3, nrow = 3,
common.legend = TRUE,
legend = "bottom")

# Display the grouped plots
print(grouped_plots1)

##Save

ggsave("Compound Events Maps_Numbers.png", plot=grouped_plots1, height=29, width=35, units=c("cm"), dpi=1000)

##############################################################

##Mapping the difference of percentage of CHDEs number in two periods

library(ggplot2)
library(ggpubr)
library(grid)  # for unit()

# Read CE data
CEs <- read.csv("All_Compound Events.csv", header = TRUE)
df <- as.data.frame(CEs)

# Color scale similar to HW/DE style
color_scale <- scale_fill_gradientn(
colours = c("dodgerblue3", "dodgerblue", "lightblue", "lightgreen", "lightgoldenrodyellow",
"khaki", "darkgoldenrod1", "orange", "orangered", "red", "red4"),
limits = c(-100, 100),
breaks = seq(-100, 100, by = 20),
na.value = "red",
name = "% Change" ) # Legend title

# Elegant base theme
base_theme <- theme_minimal(base_size = 12) +
theme(
axis.title.x = element_text(family = "Times New Roman", size = 18),
axis.title.y = element_text(family = "Times New Roman", size = 18),
axis.text = element_text(family = "Times New Roman", size = 18),
axis.ticks = element_line(size = 0.4),
legend.position = "bottom",
legend.key.height = unit(0.8, "cm"),
legend.key.width = unit(18, "cm"),
legend.title = element_text(family = "Times New Roman", size = 18, face = "bold"),
legend.text = element_text(family = "Times New Roman", size = 16),
plot.title = element_text(hjust = 0.5, family = "Times New Roman", size = 22, face = "bold"),
panel.grid = element_blank(),
panel.border = element_blank(),
plot.background = element_blank() )

# Function to create CE plot
create_CE_plot <- function(data, fill_col, title_text) {
ggplot(data, aes(x = x, y = y)) +
geom_raster(aes_string(fill = fill_col)) +
color_scale +
base_theme +
labs(x = NULL, y = NULL, title = title_text) +
guides(fill = guide_colorbar(
title.position = "top",
title.hjust = 0.5,
barwidth = unit(18, "cm"),
barheight = unit(0.8, "cm"),
ticks.colour = "black",
frame.colour = "black"  ))}

# Create plots
CE_3Day <- create_CE_plot(df, "Day_3_85", "3 Day_85P")
CE_5Day <- create_CE_plot(df, "Day_5_85", "5 Day_85P")
CE_7Day <- create_CE_plot(df, "Day_7_85", "7 Day_85P")

Day_3_90 <- create_CE_plot(df, "Day_3_90", "3 Day_90P")
Day_5_90 <- create_CE_plot(df, "Day_5_90", "5 Day_90P")
Day_7_90 <- create_CE_plot(df, "Day_7_90", "7 Day_90P")

Day_3_95 <- create_CE_plot(df, "Day_3_95", "3 Day_95P")
Day_5_95 <- create_CE_plot(df, "Day_5_95", "5 Day_95P")
Day_7_95 <- create_CE_plot(df, "Day_7_95", "7 Day_95P")

# Arrange all plots in a 3x3 grid with a common legend
grouped_plots <- ggarrange(
CE_3Day, CE_5Day, CE_7Day,
Day_3_90, Day_5_90, Day_7_90,
Day_3_95, Day_5_95, Day_7_95,
ncol = 3, nrow = 3,
common.legend = TRUE,
legend = "bottom")

# Save final plot
ggsave("Compound Events Maps1.png", plot = grouped_plots, height = 29, width = 35, units = "cm", dpi = 1000)


