# --- Load required packages ---
library(pracma)
library(ggplot2)
library(grid)  # for arrow units

# --- Define a function to process each file ---
process_vector_data <- function(filename, output_filename) {
# --- Read and inspect data ---
stations <- read.csv(filename, header = TRUE)
df <- as.data.frame(stations)
  
# --- Extract and rename columns ---
x <- df$x       # Longitude
y <- df$y       # Latitude
hot_events_50_85 <- df$WS1
hot_events_86_22 <- df$WS2
dry_spells_50_85 <- df$Dr1
dry_spells_86_22 <- df$Dr2
  
# --- Initialize result vectors ---
angles_degrees <- numeric(length(x))
magnitudes_hot_events <- numeric(length(x))
magnitudes_dry_spells <- numeric(length(x))
percentage_change_hot_events <- numeric(length(x))
percentage_change_dry_spells <- numeric(length(x))
  
# --- Loop to calculate vector angles and changes ---
for (i in seq_along(x)) {
vector_50_85 <- c(hot_events_50_85[i], dry_spells_50_85[i])
vector_86_22 <- c(hot_events_86_22[i], dry_spells_86_22[i])
    
dot_prod <- sum(vector_50_85 * vector_86_22)
mag_50_85 <- sqrt(sum(vector_50_85^2))
mag_86_22 <- sqrt(sum(vector_86_22^2))
    
if (mag_50_85 == 0 | mag_86_22 == 0) {
angle_degrees <- NA  } else {
cos_theta <- dot_prod / (mag_50_85 * mag_86_22)
cos_theta <- max(min(cos_theta, 1), -1)  # clamp for precision
angle_radians <- acos(cos_theta)
angle_degrees <- angle_radians * (180 / pi) }
    
magnitude_hot_events <- abs(hot_events_86_22[i] - hot_events_50_85[i])
magnitude_dry_spells <- abs(dry_spells_86_22[i] - dry_spells_50_85[i])
    
percentage_change_hot_events[i] <- ifelse(hot_events_50_85[i] == 0, NA,
(magnitude_hot_events / abs(hot_events_50_85[i])) * 100)
percentage_change_dry_spells[i] <- ifelse(dry_spells_50_85[i] == 0, NA,
(magnitude_dry_spells / abs(dry_spells_50_85[i])) * 100)
    
angles_degrees[i] <- angle_degrees
magnitudes_hot_events[i] <- magnitude_hot_events
magnitudes_dry_spells[i] <- magnitude_dry_spells  }
  
# --- Build results dataframe ---
results_df <- data.frame(
x = x,
y = y,
Angle_Degrees = angles_degrees,
Magnitude_Hot_Events = magnitudes_hot_events,
Magnitude_Dry_Spells = magnitudes_dry_spells,
Percentage_Change_Hot_Events = percentage_change_hot_events,
Percentage_Change_Dry_Spells = percentage_change_dry_spells)
  
# --- Add vector components ---
results_df$delta_hot <- hot_events_86_22 - hot_events_50_85
results_df$delta_dry <- dry_spells_86_22 - dry_spells_50_85
  
# Optional: scale arrows for better visualization
scale_factor <- 0.1
results_df$delta_hot_scaled <- results_df$delta_hot * scale_factor
results_df$delta_dry_scaled <- results_df$delta_dry * scale_factor
  
# --- Write to CSV ---
write.csv(results_df, output_filename, row.names = FALSE)
  
return(results_df)}

# --- Process all three files ---
df1 <- process_vector_data("Vector_pracma_85.csv", "results_85.csv")
df2 <- process_vector_data("Vector_pracma_90.csv", "results_90.csv")
df3 <- process_vector_data("Vector_pracma_95.csv", "results_95.csv")

####Plot the results 

# --- Add source labels to each data frame ---
df1$source <- "P85"
df2$source <- "P90"
df3$source <- "P95"

# --- Combine all into one data frame ---
combined_df <- rbind(df1, df2, df3)

# --- Sample vectors for clarity in plotting ---
sample_size <- 20000
n_combined <- nrow(combined_df)
sample_indices <- sample(n_combined, min(sample_size, n_combined))
sampled_df <- combined_df[sample_indices, ]

# --- Calculate mean angle for reference line ---
mean_angle <- mean(sampled_df$Angle_Degrees, na.rm = TRUE)

# --- Plot with facet_wrap ---

anglesplot_facet1 <- ggplot() +
# Red dashed mean angle line
geom_abline(intercept = 0, slope = tan(mean_angle * pi / 180),
linetype = "dashed", color = "red4", alpha = 0.5 ) +

# Vector arrows for ΔHWs and ΔDEs
geom_segment(data = sampled_df, aes(x = 0, y = 0,xend = delta_hot_scaled,
yend = delta_dry_scaled,color = Angle_Degrees ),
arrow = arrow(length = unit(0.3, "cm")), alpha = 0.7 ) +

# Gradient color scale
scale_color_gradientn(colors = c("blue", "green", "orange1", "red4"), name = "Angle (°)" ) +

# Axis labels
labs(x = "ΔHWs", y = "ΔDEs", title = "") +

# Faceting by source
facet_wrap(~ source) +

# Set axis limits
xlim(-4, 4) +  ylim(-4, 4) +

# Use theme_bw() to get borders
theme_bw(base_size = 70) +
theme(
# Axis and label styling
axis.title.x = element_text(family = "Times New Roman", size = 70),
axis.title.y = element_text(family = "Times New Roman", size = 70),
axis.text = element_text(family = "Times New Roman", size = 70),
axis.ticks = element_line(size = 0.4),

# Legend styling
legend.position = "bottom",
legend.key.height = unit(3, "cm"),
legend.key.width = unit(30, "cm"),
legend.title = element_text(family = "Times New Roman", size = 40, face = "bold"),
legend.text = element_text(family = "Times New Roman", size = 40),

# Facet strip titles
strip.text = element_text(family = "Times New Roman",size = 40, face = "bold",
margin = margin(t = 12, b = 12) ),

# Keep background clean
plot.background = element_blank() ) +

# Elegant horizontal legend
guides(color = guide_colorbar(
title.position = "top",
title.hjust = 0.5,
barwidth = unit(28, "cm"),
barheight = unit(1.1, "cm"),
ticks.colour = "black",
frame.colour = "black" ))

# Display the plot
print(anglesplot_facet1)

##Save
ggsave("Vectorgraphs.png", plot=anglesplot_facet1, height=37, width=65, units=c("cm"), dpi=100)

##Add a new legend
library(cowplot)  # Needed for ggdraw and draw_label

legend_strip <- ggdraw() +
draw_label(
"Warm only up        Both up (Warm >)\n0°────────────22.5°────────────45°",
fontfamily = "Times New Roman",
fontface = "italic", size = 35,x = 0.5, hjust = 0.5 )


combined_plot1 <- plot_grid(
anglesplot_facet1,legend_strip, ncol = 1,
rel_heights = c(1, 0.12) )

##Save
ggsave("Vectorgraphs1.png", plot=combined_plot1, height=37, width=65, units=c("cm"), dpi=1000)


####################################
##Calculating the magnitude as a total####

# Read the files
df_85 <- read.csv("results_85.csv")
df_90 <- read.csv("results_90.csv")
df_95 <- read.csv("results_95.csv")

# Calculate magnitude (Euclidean distance) for each
df_85$magnitude_total <- sqrt(df_85$delta_hot^2 + df_85$delta_dry^2)
df_90$magnitude_total <- sqrt(df_90$delta_hot^2 + df_90$delta_dry^2)
df_95$magnitude_total <- sqrt(df_95$delta_hot^2 + df_95$delta_dry^2)

# Calculate magnitude (Euclidean distance) for each
df_85$magnitude_total <- sqrt(df_85$delta_hot^2 + df_85$delta_dry^2)
df_90$magnitude_total <- sqrt(df_90$delta_hot^2 + df_90$delta_dry^2)
df_95$magnitude_total <- sqrt(df_95$delta_hot^2 + df_95$delta_dry^2)

write.csv(df_85, "results_85_updated.csv", row.names = FALSE)
write.csv(df_90, "results_90_updated.csv", row.names = FALSE)
write.csv(df_95, "results_95_updated.csv", row.names = FALSE)

##Plotting the vector with Magnitude Total


# Read the processed CSVs ##After adding the coloumn o of New Magnitude to each file
df1 <- read.csv("results_85.csv")
df2 <- read.csv("results_90.csv")
df3 <- read.csv("results_95.csv")

# Ensure all have the same columns by calculating magnitude_total (if missing)
calculate_magnitude <- function(df) {
if (!"magnitude_total" %in% names(df)) {
df$magnitude_total <- sqrt(df$delta_hot^2 + df$delta_dry^2) }
return(df)}

df1 <- calculate_magnitude(df1)
df2 <- calculate_magnitude(df2)
df3 <- calculate_magnitude(df3)

# Add source labels
df1$source <- "P85"
df2$source <- "P90"
df3$source <- "P95"

# Double-check column names are the same
stopifnot(identical(names(df1), names(df2)), identical(names(df2), names(df3)))

# Now safely combine them
combined_df <- rbind(df1, df2, df3)

# --- Sample vectors for clarity in plotting ---
sample_size <- 20000
n_combined <- nrow(combined_df)
sample_indices <- sample(n_combined, min(sample_size, n_combined))
sampled_df <- combined_df[sample_indices, ]

# --- Calculate mean angle for reference line ---
mean_angle <- mean(sampled_df$Angle_Degrees, na.rm = TRUE)

# --- Plot with facet_wrap ---
anglesplot_facet2 <- ggplot() +
# Red dashed mean angle line
geom_abline(intercept = 0, slope = tan(mean_angle * pi / 180),
linetype = "dashed", color = "red4", alpha = 0.5 ) +

# Vector arrows for ΔHWs and ΔDEs, colored by magnitude_total
geom_segment(data = sampled_df, aes(x = 0, y = 0, xend = delta_hot_scaled,
yend = delta_dry_scaled, color = magnitude_total),
arrow = arrow(length = unit(0.3, "cm")), alpha = 0.7 ) +

# Custom gradient color scale
scale_color_gradientn(colors = c("blue","green","red"),
name = "Magnitude") +

# Axis labels
labs(x = expression(Delta * "HWs"), y = expression(Delta * "DEs"), title = "") +
# Faceting by source
facet_wrap(~ source) +
# Set axis limits
xlim(-4, 4) + ylim(-4, 4) +
# Use theme_bw() to get borders
theme_bw(base_size = 30) +
theme(
axis.title.x = element_text(family = "Times New Roman", size = 40),
axis.title.y = element_text(family = "Times New Roman", size = 40),
axis.text = element_text(family = "Times New Roman", size = 40),
axis.ticks = element_line(size = 0.4),
legend.position = "bottom",
legend.key.height = unit(3, "cm"),
legend.key.width = unit(30, "cm"),
legend.title = element_text(family = "Times New Roman", size = 40, face = "bold"),
legend.text = element_text(family = "Times New Roman", size = 40),
strip.text = element_text(family = "Times New Roman", size = 40, face = "bold",
margin = margin(t = 12, b = 12)),
plot.background = element_blank()) +

# Elegant horizontal legend
guides(color = guide_colorbar(
title.position = "top",
title.hjust = 0.5,
barwidth = unit(28, "cm"),
barheight = unit(1.1, "cm"),
ticks.colour = "black",
frame.colour = "black" ))

# Display the plot
print(anglesplot_facet2)

##Save
ggsave("Vectorgraphs_Magnitude.png", plot=anglesplot_facet2, height=37, width=65, units=c("cm"), dpi=1000)

########################################
##Pie charts for vector Analysis Angeles

## --- Vector Angle with X-axis (0–360°) ---

# --- Load required packages ---
library(ggplot2)
library(grid)

# --- Function to process each vector file ---
process_vector_data <- function(filename, output_filename) {
# Read CSV
df <- read.csv(filename, header = TRUE)

# Extract vectors
x <- df$x
y <- df$y
WS1 <- df$WS1  # HWs 1950–1985
WS2 <- df$WS2  # HWs 1986–2022
Dr1 <- df$Dr1  # DEs 1950–1985
Dr2 <- df$Dr2  # DEs 1986–2022

# Compute vector changes
delta_hot <- WS2 - WS1
delta_dry <- Dr2 - Dr1

# Compute angle w.r.t. X-axis in [0°, 360°]
angle_rad <- atan2(delta_dry, delta_hot)
angle_deg <- angle_rad * (180 / pi)
angle_deg[angle_deg < 0] <- angle_deg[angle_deg < 0] + 360

# Scale arrows (optional)
scale_factor <- 0.1
delta_hot_scaled <- delta_hot * scale_factor
delta_dry_scaled <- delta_dry * scale_factor

# Build results dataframe
results_df <- data.frame(
x = x,
y = y,
delta_hot = delta_hot,
delta_dry = delta_dry,
angle_deg = angle_deg,
delta_hot_scaled = delta_hot_scaled,
delta_dry_scaled = delta_dry_scaled )

# Write CSV
write.csv(results_df, output_filename, row.names = FALSE)

return(results_df)}

# --- Process vector datasets ---
df85 <- process_vector_data("Vector_pracma_85.csv", "results_85.csv")
df90 <- process_vector_data("Vector_pracma_90.csv", "results_90.csv")
df95 <- process_vector_data("Vector_pracma_95.csv", "results_95.csv")

# --- Add percentile source labels ---
df85$source <- "P85"
df90$source <- "P90"
df95$source <- "P95"

# --- Combine data ---
combined_df <- rbind(df85, df90, df95)

# --- Sample for clarity ---
sampled_df <- combined_df[sample(nrow(combined_df), min(20000, nrow(combined_df))), ]

# --- Calculate mean angle ---
mean_angle <- mean(sampled_df$angle_deg, na.rm = TRUE)

# --- Create the plot ---
plot <- ggplot() +
# Mean direction line
geom_abline(intercept = 0, slope = tan(mean_angle * pi / 180),
linetype = "dashed", color = "red4", alpha = 1) +

# Vector arrows
geom_segment(data = sampled_df,
aes(x = 0, y = 0,
xend = delta_hot_scaled,
yend = delta_dry_scaled,
color = angle_deg),
arrow = arrow(length = unit(0.3, "cm")), alpha = 0.7) +

# Color scale
scale_color_gradientn(colors = c("blue", "green", "orange", "red4"),
name = "Angle (°)") +

# Labels
labs(x = "ΔHWs", y = "ΔDEs") +

# Facet by percentile
facet_wrap(~ source) +

# Coordinate limits
xlim(-4, 4) + ylim(-4, 4) +

# Theme and font
theme_bw(base_size = 30) +
theme(
axis.title.x = element_text(family = "Times New Roman", size = 40),
axis.title.y = element_text(family = "Times New Roman", size = 40),
axis.text = element_text(family = "Times New Roman", size = 40),
legend.position = "bottom",
legend.key.height = unit(3, "cm"),
legend.key.width = unit(30, "cm"),
legend.title = element_text(family = "Times New Roman", size = 40, face = "bold"),
legend.text = element_text(family = "Times New Roman", size = 40),
strip.text = element_text(family = "Times New Roman", size = 40, face = "bold"),
plot.background = element_blank() ) +
guides(color = guide_colorbar(
title.position = "top",
title.hjust = 0.5,
barwidth = unit(28, "cm"),
barheight = unit(1.1, "cm"),
ticks.colour = "black",
frame.colour = "black" ))

# --- Show plot ---
print(plot)

##################################

##Vector Analysis (Angel with X axis)##Not shown

# --- Load required packages ---
library(ggplot2)
library(grid)

# --- Function to process vector data ---
process_vector_data <- function(filename, output_filename) {
df <- read.csv(filename, header = TRUE)

# Extract columns
x <- df$x
y <- df$y
WS1 <- df$WS1  # HWs 1980–2001
WS2 <- df$WS2  # HWs 2002–2023
Dr1 <- df$Dr1  # DEs 1980–2001
Dr2 <- df$Dr2  # DEs 2002–2023

# Compute deltas
zfdvcdelta_hot <- WS2 - WS1
  delta_dry <- Dr2 - Dr1

# Angle relative to X-axis in [0°, 360°]
angle_rad <- atan2(delta_dry, delta_hot)
angle_deg <- angle_rad * (180 / pi)
angle_deg[angle_deg < 0] <- angle_deg[angle_deg < 0] + 360

# Build results
results_df <- data.frame(
y = y,
delta_hot = delta_hot,
delta_dry = delta_dry,
angle_deg = angle_deg  )

# Optional: scale arrows for visualization
scale_factor <- 0.1
results_df$delta_hot_scaled <- results_df$delta_hot * scale_factor
results_df$delta_dry_scaled <- results_df$delta_dry * scale_factor

write.csv(results_df, output_filename, row.names = FALSE)
  

# --- Process the three CSV files ---
df85 <- process_vector_data("Vector_pracma_85.csv", "results_85.csv")
df90 <- process_vector_data("Vector_pracma_90.csv", "results_90.csv")
df95 <- process_vector_data("Vector_pracma_95.csv", "results_95.csv")

# --- Add percentile labels ---
df85$source <- "P85"
df90$source <- "P90"
df95$source <- "P95"

# --- Combine all data ---
combined_df <- rbind(df85, df90, df95)

# --- Sample for clearer plot if needed ---
sampled_df <- combined_df[sample(nrow(combined_df), min(20000, nrow(combined_df))), ]

# --- Plotting ---
mean_angle <- mean(sampled_df$angle_deg, na.rm = TRUE)

plot <- ggplot() +
# Mean angle line
geom_abline(intercept = 0, slope = tan(mean_angle * pi / 180),
linetype = "dashed", color = "red4", alpha = 1) +

# Vector arrows
geom_segment(data = sampled_df,
aes(x = 0, y = 0,
xend = delta_hot_scaled,
yend = delta_dry_scaled,
color = angle_deg),
arrow = arrow(length = unit(0.3, "cm")), alpha = 0.7) +
scale_color_gradientn(colors = c("blue", "green", "orange", "red4"),
name = "Angle (°)") +

labs(x = "ΔHWs", y = "ΔDEs ") +
facet_wrap(~ source) +
theme_bw(base_size = 30) +
xlim(-4, 4) + ylim(-4, 4) +
theme(axis.title.x = element_text(family = "Times New Roman", size = 40),
axis.title.y = element_text(family = "Times New Roman", size = 40),
axis.text = element_text(family = "Times New Roman", size = 40),
legend.position = "bottom",
legend.key.height = unit(3, "cm"),
legend.key.width = unit(30, "cm"),
legend.title = element_text(family = "Times New Roman", size = 40, face = "bold"),
legend.text = element_text(family = "Times New Roman", size = 40),
strip.text = element_text(family = "Times New Roman", size = 40, face = "bold"),
plot.background = element_blank() ) +
guides(color = guide_colorbar(
title.position = "top",
title.hjust = 0.5,
barwidth = unit(28, "cm"),
barheight = unit(1.1, "cm"),
ticks.colour = "black",
frame.colour = "black" ))

# Display the plot
print(plot)

# Save to file
ggsave("Angle_Xaxis_VectorPlot.png", plot = plot, height = 37, width = 65, units = "cm", dpi = 1000)



