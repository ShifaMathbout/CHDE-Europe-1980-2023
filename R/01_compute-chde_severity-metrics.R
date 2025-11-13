##Calculate the CHDEs Severities

# === Step 1: Load Data ===
temp <- read.csv("Temp_2020-2021.csv", check.names = FALSE)
edi <- read.csv("EDI_2020-2021.csv", check.names = FALSE)
perc <- read.csv("percentiles.csv")

# === Step 2: Clean Dates (remove 'X' and fix format) ===
clean_dates <- function(df) {
names(df)[-1] <- gsub("X", "", names(df)[-1])
names(df)[-1] <- gsub("\\.", "-", names(df)[-1])
return(df)}

temp <- clean_dates(temp)
edi <- clean_dates(edi)

# === Step 3: Convert percentile columns to numeric ===
perc[,-1] <- lapply(perc[,-1], function(x) as.numeric(as.character(x)))
perc[,1] <- as.character(perc[,1])

# === Step 4: Prepare matrices and metadata ===
temp_mat <- as.matrix(temp[,-1])
edi_mat <- as.matrix(edi[,-1])
grids <- temp[, 1]
dates <- colnames(temp)[-1]

# === Step 5: Function to calculate CHDI severity for a percentile pair ===
calculate_chde <- function(thresh_col_low, thresh_col_high, threshold_label) {
chdi_sev <- numeric(length(grids))
names(chdi_sev) <- grids

for (g in 1:length(grids)) {
tmax <- as.numeric(temp_mat[g, ])
edi_vals <- as.numeric(edi_mat[g, ])
    
# Extract percentiles for this grid
t_low <- perc[g, thresh_col_low]
t_high <- perc[g, thresh_col_high]
    
# Standardized temperature
t_std <- (tmax - t_low) / (t_high - t_low)
    
# Identify CHDE candidate days (TMAX > threshold)
exceed <- tmax > t_high
rle_events <- rle(exceed)
    
idx <- 1
sev_list <- list()
    
for (i in seq_along(rle_events$lengths)) {
len <- rle_events$lengths[i]
val <- rle_events$values[i]
      
if (val && len >= 3) {
days_idx <- idx:(idx + len - 1)
        
# Compute daily severities only for EDI < 0
daily_sev <- sapply(days_idx, function(d) {
if (!is.na(edi_vals[d]) && edi_vals[d] < 0) {
sev <- (-1 * edi_vals[d]) * t_std[d]
return(max(sev, 0))     } else {
return(0)  }    })
        
sev_list[[length(sev_list) + 1]] <- sum(daily_sev, na.rm = TRUE) }
idx <- idx + len    }

 N <- length(sev_list)
chdi_sev[g] <- if (N > 0) sum(unlist(sev_list)) / N else 0  }  return(chdi_sev)}

# === Step 6: Run CHDI for 85th, 90th, 95th thresholds ===
chdi_85 <- calculate_chde("T15p", "T85p", "85th")
chdi_90 <- calculate_chde("T10p", "T90p", "90th")
chdi_95 <- calculate_chde("T5p",  "T95p", "95th")

# === Step 7: Combine and Save Results ===
result <- data.frame(
Grid = grids,
CHDI_85th = chdi_85,
CHDI_90th = chdi_90,
CHDI_95th = chdi_95)

# === Step 8: Write to Desktop ===
write.csv(result, "C:/Users/shifa/Desktop/CHDI_Severity.csv", row.names = FALSE)

# Optional: view output
print(result)
