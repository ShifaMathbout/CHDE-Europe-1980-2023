library(data.table)
library(geosphere)
library(ggplot2)
library(grid)
library(patchwork)

# Your files
files <- c("CE_85_Urban.csv", "CE_90_Urban.csv", "CE_95_Urban.csv")
scenarios <- c("CE_85", "CE_90", "CE_95")  # scenario labels

# European capitals table
capitals <- data.table(
country = c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina",
"Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Georgia",
"Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kazakhstan", "Kosovo", "Latvia", "Liechtenstein",
"Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia",
"Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia",
"Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom", "Vatican City"),
capital = c("Tirana", "Andorra la Vella", "Yerevan", "Vienna", "Baku", "Minsk", "Brussels", "Sarajevo",
"Sofia", "Zagreb", "Nicosia", "Prague", "Copenhagen", "Tallinn", "Helsinki", "Paris", "Tbilisi",
"Berlin", "Athens", "Budapest", "Reykjavik", "Dublin", "Rome", "Nur-Sultan", "Pristina", "Riga", "Vaduz",
"Vilnius", "Luxembourg", "Valletta", "Chișinău", "Monaco", "Podgorica", "Amsterdam", "Skopje",
"Oslo", "Warsaw", "Lisbon", "Bucharest", "Moscow", "San Marino", "Belgrade", "Bratislava", "Ljubljana",
"Madrid", "Stockholm", "Bern", "Ankara", "Kyiv", "London", "Vatican City"),
lon = c(19.8189, 1.5218, 44.5146, 16.3738, 49.8671, 27.5667, 4.3517, 18.4131,
23.3219, 15.9819, 33.3736, 14.4378, 12.5683, 24.7536, 24.9384, 2.3522, 44.8271,
13.4050, 23.7275, 19.0402, -21.8277, -6.2603, 12.4964, 71.4304, 21.1606, 24.1052, 9.5167,
25.2797, 6.1319, 14.5146, 28.8574, 7.4206, 19.2620, 4.9041, 21.4314,
10.7522, 21.0122, -9.1427, 26.1025, 37.6173, 12.4578, 20.4489, 17.1077, 14.5058,
-3.7038, 18.0686, 7.4474, 32.8597, 30.5234, -0.1276, 12.4534),
lat = c(41.3275, 42.5078, 40.1792, 48.2082, 40.4093, 53.9006, 50.8503, 43.8563,
42.6975, 45.8150, 35.1856, 50.0755, 55.6761, 59.4370, 60.1695, 48.8566, 41.7151,
52.5200, 37.9838, 47.4979, 64.1265, 53.3498, 41.9028, 51.1694, 42.6629, 56.9496, 47.1416,
54.6872, 49.6117, 35.8997, 47.0105, 43.7333, 42.4304, 52.3676, 41.9981,
59.9139, 52.2297, 38.7169, 44.4268, 55.7558, 43.9333, 44.7866, 48.1486, 46.0569,
40.4168, 59.3293, 46.9481, 39.9334, 50.4501, 51.5074, 41.9029))

# ---- Define your filtered urban cities here ----
filtered_cities <- c("Berlin", "Paris", "Rome", "Madrid", "London")  # Replace with your filtered cities

filtered_capitals <- capitals[capital %in% filtered_cities]

# Combine data for all files and scenarios
combined_long <- rbindlist(lapply(seq_along(files), function(i) {
f <- files[i]
scenario <- scenarios[i]

chde <- fread(f)
coords <- chde[, .(x, y)]
cap_coords <- filtered_capitals[, .(lon, lat)]

# Calculate min distance to filtered urban capitals
chde[, min_dist_km := apply(distm(coords, cap_coords, fun = distHaversine) / 1000, 1, min)]
chde[, urban := as.integer(min_dist_km <= 25)]
chde_long <- melt(chde, id.vars = c("x", "y", "urban", "min_dist_km"),
variable.name = "year", value.name = "CHDE")
chde_long[, year := as.integer(as.character(year))]
chde_long[, decade := fifelse(year >= 2010, "2010s", paste0(floor(year / 10) * 10, "s"))]
chde_long[, decade := factor(decade, levels = c("1980s", "1990s", "2000s", "2010s"))]
chde_long[, scenario := scenario]
return(chde_long)}))

# Calculate median stats
median_stats <- combined_long[, .(median_CHDE = median(CHDE, na.rm = TRUE)), by = .(decade, urban, scenario)]

# Update scenario labels for facet titles
combined_long[, scenario := factor(scenario, 
levels = c("CE_85", "CE_90", "CE_95"),
labels = c("85P", "90P", "95P"))]
median_stats[, scenario := factor(scenario,
levels = c("CE_85", "CE_90", "CE_95"),
labels = c("85P", "90P", "95P"))]

# Boxplot of CHDE by decade and urban/rural
p <- ggplot(combined_long, aes(x = decade, y = CHDE, fill = factor(urban))) +
geom_boxplot(
outlier.shape = 21, outlier.size = 1.5, alpha = 0.8, width = 0.7, color = "black",
position = position_dodge(width = 0.75)) +
geom_point(
data = median_stats,
aes(x = decade, y = median_CHDE, group = urban),
shape = 21, size = 4, color = "black",
fill = c("darkblue", "yellow")[median_stats$urban + 1],
position = position_dodge(width = 0.75),
inherit.aes = FALSE) +
scale_fill_manual(
values = c("0" = "darkgreen", "1" = "red"),
labels = c("Rural", "Urban")) +
labs(title = "", x = "", y = "") +
facet_wrap(~ scenario, ncol = 3) +
theme_minimal(base_size = 30, base_family = "Times") +
theme(
legend.position = "bottom",
legend.title = element_blank(),
text = element_text(family = "Times", size = 30),
axis.title = element_text(family = "Times", size = 30),
axis.text = element_text(family = "Times", size = 30),
legend.text = element_text(family = "Times", size = 30),
plot.title = element_text(family = "Times", size = 30, face = "bold", hjust = 0.5),
strip.text = element_text(family = "Times", size = 30, face = "bold") )

print(p)


# ====== PLOT P2: SLOPE PLOT WITH Δ AS NUMBER ======
combined_long[, period := fifelse(year <= 2001, "1980–2001", "2002–2023")]
period_means <- combined_long[, .(mean_CHDE = mean(CHDE, na.rm = TRUE)), by = .(urban, scenario, period)]
period_wide <- dcast(period_means, scenario + urban ~ period, value.var = "mean_CHDE")
period_wide[, delta_abs := `2002–2023` - `1980–2001`]

# Set factor for faceting
period_means[, scenario := factor(scenario, levels = c("85P", "90P", "95P"))]
period_wide[, scenario := factor(scenario, levels = c("85P", "90P", "95P"))]

p2 <- ggplot() +
geom_segment(data = period_wide,
aes(x = 1980, xend = 2023, y = `1980–2001`, yend = `2002–2023`,
color = factor(urban)),
size = 1.5, arrow = arrow(length = unit(0.25, "cm"))) +
geom_point(data = period_means,
aes(x = ifelse(period == "1980–2001", 1980, 2023), y = mean_CHDE, color = factor(urban)),
size = 4, shape = 21, stroke = 1.2, fill = "white") +
geom_text(data = period_wide,
aes(x = 2001.5, y = (`1980–2001` + `2002–2023`) / 2,
label = paste0("Δ = ", round(delta_abs, 3)),
color = factor(urban)),
position = position_nudge(y = ifelse(period_wide$urban == 1, 0.4, -0.2)),
size = 9, fontface = "bold", family = "Times") +
scale_x_continuous(breaks = seq(1980, 2023, by = 10), limits = c(1980, 2023)) +
expand_limits(y = 0) +
scale_color_manual(values = c("0" = "darkgreen", "1" = "firebrick"),
labels = c("Rural", "Urban")) +
labs(title = "", x = "", y = "") +
facet_wrap(~ scenario, ncol = 3) +
theme_minimal(base_size = 30, base_family = "Times") +
theme(
legend.position = "bottom",
legend.title = element_blank(),
legend.text = element_text(family = "Times", size = 30),
axis.title = element_text(family = "Times", size = 30),
axis.text = element_text(family = "Times", size = 30),
plot.title = element_text(family = "Times", size = 30, face = "bold", hjust = 0.5),
strip.text = element_text(family = "Times", size = 30, face = "bold") )

# Combine plots and export
combined_plot <- p / p2 + plot_layout(ncol = 1, heights = c(1, 0.6))
ggsave("C:/Users/shifa/Desktop/Urban_Plots_Numeric_Diff.png", combined_plot,
width = 22, height = 20, dpi = 1000)
########################

###Maps of trends in urban cities
# Load required libraries
library(data.table)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(geosphere)
library(scales)

# --- Function to process each CE CSV file ---
process_chde <- function(file_path, decade_label) {
chde <- fread(file_path)
  
years <- 1980:2023
year_cols <- as.character(years)
year_cols <- year_cols[year_cols %in% colnames(chde)]  # Only keep available years
  
chde[, trend := {
y_vals <- as.numeric(.SD)
if (all(is.na(y_vals))) NA_real_ else coef(lm(y_vals ~ years))[2]
}, by = .(x, y), .SDcols = year_cols]
  
chde_sf <- st_as_sf(chde, coords = c("x", "y"), crs = 4326)
chde_sf$decade <- decade_label
return(chde_sf)}

# --- Load and process CE files by decade ---
ce_85_sf <- process_chde("CE_85_Urban.csv", "85P")
ce_90_sf <- process_chde("CE_90_Urban.csv", "90P")
ce_95_sf <- process_chde("CE_95_Urban.csv", "95P")

# Combine into one sf object
all_ce_sf <- rbind(ce_85_sf, ce_90_sf, ce_95_sf)

# --- Load countries and cities ---
europe <- ne_countries(continent = "Europe", returnclass = "sf") %>% st_transform(4326)
turkey <- ne_countries(country = "Turkey", returnclass = "sf") %>% st_transform(4326)

cities <- ne_download(scale = 10, type = "populated_places", category = "cultural", returnclass = "sf") %>%
st_transform(4326)

# --- Filter urban cities ---
# Europe cities with pop > 300,000
europe_urban_cities <- cities %>%
filter(ADM0NAME %in% europe$name & POP_MAX > 300000)

# Turkey cities with pop > 200,000
turkish_urban_cities <- cities %>%
filter(ADM0NAME == "Turkey" & POP_MAX > 200000)

# Combine both
urban_cities <- rbind(europe_urban_cities, turkish_urban_cities)

# --- Calculate min distance to urban city & flag urban points ---
coords_all <- st_coordinates(all_ce_sf)
urban_coords <- st_coordinates(urban_cities)

all_ce_sf$min_city_dist_km <- apply(distm(coords_all, urban_coords), 1, min) / 1000
all_ce_sf$urban <- as.integer(all_ce_sf$min_city_dist_km <= 25)

# --- Rescale slope for plotting ---
all_ce_sf$slope10 <- all_ce_sf$trend * 10

# --- Plot ---
plot_chde <- ggplot() +
geom_sf(data = europe, fill = "gray95", color = "gray70") +
  
# CHDE points
geom_sf(data = all_ce_sf, aes(color = slope10), size = 1.1) +
  
# Urban marker
geom_sf(data = all_ce_sf[all_ce_sf$urban == 1, ],
shape = 21, fill = "white", color = "black",
size = 0.2, stroke = 0.6, alpha = 0.6) +
  
scale_color_gradient2(
low = "blue",
mid = "white",
high = "red3",
midpoint = 0,
limits = c(-1, 1),
name = expression("Slope per decade"),
na.value = "grey80") +
labs(
title = "",
subtitle = "",
x = NULL, y = NULL ) +
coord_sf(xlim = c(-25, 45), ylim = c(30, 75), expand = FALSE) +
  
# Axis labels as plain numbers
scale_x_continuous(
breaks = seq(-20, 40, by = 10),
labels = scales::number_format() ) +
scale_y_continuous(
breaks = seq(30, 75, by = 5),
labels = scales::number_format() ) +
theme_minimal(base_size = 16, base_family = "Times New Roman") +
theme(
plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
plot.subtitle = element_text(size = 14, hjust = 0.5),
axis.text = element_text(size = 14),
axis.ticks = element_line(size = 0.4),
legend.position = "bottom",
legend.key.height = unit(0.5, "cm"),
legend.key.width = unit(14, "cm"),
legend.title = element_text(size = 18, face = "bold"),
legend.text = element_text(size = 16),
strip.text = element_text(size = 16, face = "bold"),
panel.grid = element_blank(),
plot.background = element_blank() ) +
  
guides(color = guide_colorbar(
title.position = "top",
title.hjust = 0.5,
barwidth = unit(14, "cm"),
barheight = unit(0.8, "cm"),
ticks.colour = "black",
frame.colour = "black")) +
facet_wrap(~decade)

# --- Save plot to file ---
ggsave(filename = "CHDE_Europe_Trends_Project3035_Centered.png", plot = plot_chde,width = 24, height = 14, dpi = 1000, units = "cm", bg = "white")

#################################################################
##Plot using different symbols colors

# Define significance threshold
# Set significance threshold
sig_threshold <- 0.05

# Assign urban border colors based on significance and trend direction
all_ce_sf$urban_border_color <- "gold4"  # default for non-significant
all_ce_sf$urban_border_color[all_ce_sf$urban == 1 &
all_ce_sf$p_value < sig_threshold &
all_ce_sf$slope10 > 0] <- "darkred"

all_ce_sf$urban_border_color[all_ce_sf$urban == 1 &
all_ce_sf$p_value < sig_threshold &
all_ce_sf$slope10 < 0] <- "darkblue"

# --- Plot ---
plot_chde <- ggplot() +
geom_sf(data = europe, fill = "snow", color = "gray70") +

# CHDE grid points colored by slope
geom_sf(data = all_ce_sf, aes(color = slope10), size = 1.1) +

# Urban markers with manual border colors
geom_sf(data = all_ce_sf %>% filter(urban == 1),
aes(color = NULL),  # prevent double color scale
shape = 21,fill = "gray71", color = all_ce_sf$urban_border_color[all_ce_sf$urban == 1],
size = 0.8, stroke = 0.6, alpha = 0.6) +

# Slope gradient scale
scale_color_gradient2(
low = "blue", mid = "white", high = "red3",
midpoint = 0,limits = c(-1, 1),
name = expression("Slope per decade"),
na.value = "grey80") +

# Layout and theme
labs(title = "", subtitle = "", x = NULL, y = NULL) +
coord_sf(xlim = c(-25, 45), ylim = c(30, 75), expand = FALSE) +
scale_x_continuous(breaks = seq(-20, 40, by = 10), labels = scales::number_format()) +
scale_y_continuous(breaks = seq(30, 75, by = 5), labels = scales::number_format()) +
theme_minimal(base_size = 16, base_family = "Times New Roman") +
theme(
plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
plot.subtitle = element_text(size = 14, hjust = 0.5),
axis.text = element_text(size = 14),
axis.ticks = element_line(size = 0.4),
legend.position = "bottom",
legend.key.height = unit(0.5, "cm"),
legend.key.width = unit(14, "cm"),
legend.title = element_text(size = 18, face = "bold"),
legend.text = element_text(size = 16),
strip.text = element_text(size = 16, face = "bold"),
panel.grid = element_blank(),
plot.background = element_blank() ) +
guides(color = guide_colorbar(
title.position = "top",
title.hjust = 0.5,
barwidth = unit(14, "cm"),
barheight = unit(0.8, "cm"),
ticks.colour = "black",
frame.colour = "black" )) +

facet_wrap(~decade)

# Display the plot
print(plot_chde)

# --- Save plot to file ---
ggsave(filename = "CHDE_Europe_Trends_Project3035_Centered.png", plot = plot_chde,width = 24, height = 14, dpi = 1000, units = "cm", bg = "white")

####Calculate the number of urban points and trend change
library(geosphere)

# Make sure urban_cities is loaded and transformed to same CRS
# urban_cities <- ... (your urban cities data)

coords_all <- st_coordinates(all_ce_sf)
urban_coords <- st_coordinates(urban_cities)

# Calculate distance matrix (meters), convert to km
dist_matrix <- distm(coords_all, urban_coords) / 1000

# Find minimum distance to any urban city for each point
all_ce_sf$min_city_dist_km <- apply(dist_matrix, 1, min)

# Flag as urban if distance <= 25 km
all_ce_sf$urban <- as.integer(all_ce_sf$min_city_dist_km <= 25)

# Now check urban counts
table(all_ce_sf$urban)

urban_points <- all_ce_sf[all_ce_sf$urban == 1, ]
write.csv(urban_points, file = "urban_points.csv", row.names = FALSE)

sig_pos <- sum(urban_points$p_value < 0.05 & urban_points$trend > 0, na.rm = TRUE)
sig_neg <- sum(urban_points$p_value < 0.05 & urban_points$trend < 0, na.rm = TRUE)
total_urban <- nrow(urban_points)
perc_pos <- round(sig_pos / total_urban * 100, 2)
perc_neg <- round(sig_neg / total_urban * 100, 2)

cat("✅ Total urban points:", total_urban, "\n")
cat("🔴 Significant Increase:", sig_pos, "(", perc_pos, "% )\n")
cat("🔵 Significant Decrease:", sig_neg, "(", perc_neg, "% )\n")

###For each decade
library(dplyr)

# For each decade, calculate counts and percentages of significant increase/decrease in urban points
urban_summary_by_decade <- all_ce_sf %>%
filter(urban == 1) %>%
group_by(decade) %>%
summarise(
total_points = n(),
sig_increase = sum(p_value < 0.05 & trend > 0, na.rm = TRUE),
sig_decrease = sum(p_value < 0.05 & trend < 0, na.rm = TRUE),
perc_increase = round(sig_increase / total_points * 100, 2),
perc_decrease = round(sig_decrease / total_points * 100, 2) )

print(urban_summary_by_decade)
