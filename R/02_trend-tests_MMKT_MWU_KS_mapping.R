# 02_trend-and-distribution_tests_CHDEs.R
# Purpose: Run trend tests (Mann–Kendall, Modified MK, Sen's slope)
#          and distribution-shift tests (CvM, MWU, KS) for CHDE datasets.
# Notes:
# - Do NOT install packages inside the script. List these in your README:
#   required packages: trend, modifiedmk, Kendall, zyp, data.table, dplyr, cramer
# - All inputs are CSVs in the working directory. All outputs are CSVs.

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(trend)       # mk.test
  library(modifiedmk)  # mmkh
  library(Kendall)     # MannKendall
  library(zyp)         # zyp.sen
  library(cramer)      # cramer.test
})

# =========================
# A) Single-series MK tests
# =========================
# Input: CHDEs_MMKT.csv — must contain a numeric column named `value` (or `value2`)
# Output: mk_single_series_results.csv

run_single_series_mk <- function(infile = "CHDEs_MMKT.csv",
                                 value_cols = c("value", "value2"),
                                 alpha = 0.05) {
  stopifnot(file.exists(infile))
  dt <- fread(infile)

  # choose the first available value column
  vcol <- value_cols[value_cols %in% names(dt)][1]
  if (is.na(vcol)) stop("No numeric 'value' column found (tried: ", paste(value_cols, collapse=", "), ").")

  x <- as.numeric(dt[[vcol]])
  x <- x[is.finite(x)]
  if (length(x) < 3) stop("Series too short for MK test.")

  mk_base <- trend::mk.test(x)
  p_base <- unname(mk_base$p.value)

  # Modified MK (variance corrected)
  mmk <- modifiedmk::mmkh(x, ci = 0.95)

  out <- data.frame(
    test                  = c("MK", "Modified_MK"),
    p_value               = c(p_base, mmk$P_VALUE),
    tau_or_Z              = c(unname(mk_base$estimates[["tau"]]), mmk$STATISTIC),
    significant_alpha_0.05= c(p_base < alpha, mmk$P_VALUE < alpha)
  )
  fwrite(out, "mk_single_series_results.csv")
}

# ======================================
# B) Modified MK on another single series
# ======================================
# Input: Dryspells_Trend.csv with numeric column `value`
# Output: modified_mk_single_series_results.csv

run_modified_mk_single <- function(infile = "Dryspells_Trend.csv", alpha = 0.05) {
  stopifnot(file.exists(infile))
  dt <- fread(infile)
  if (!"value" %in% names(dt)) stop("Expected a 'value' column in ", infile)
  x <- as.numeric(dt$value)
  x <- x[is.finite(x)]
  if (length(x) < 3) stop("Series too short for Modified MK test.")

  mmk <- modifiedmk::mmkh(x, ci = 0.95)
  out <- data.frame(
    test                  = "Modified_MK",
    statistic             = mmk$STATISTIC,
    p_value               = mmk$P_VALUE,
    significant_alpha_0.05= mmk$P_VALUE < alpha
  )
  fwrite(out, "modified_mk_single_series_results.csv")
}

# =====================================================
# C) Mann–Kendall + Sen's slope per station (wide table)
# =====================================================
# Input: yearly_means_arranged.csv
#   - first column = Station id (or name)
#   - remaining columns = years (e.g., X1980, X1981, ... or 1980, 1981, ...)
# Output: mann_kendall_per_station_results.csv

run_station_mk <- function(infile = "yearly_means_arranged.csv", alpha = 0.05) {
  stopifnot(file.exists(infile))
  df <- fread(infile)

  # Clean year column names (drop leading "X")
  setnames(df, old = names(df), new = gsub("^X", "", names(df)))

  station_col <- names(df)[1]
  year_cols <- names(df)[-1]
  # keep columns that are numeric years
  keep <- suppressWarnings(!is.na(as.numeric(year_cols)))
  year_cols <- year_cols[keep]
  years <- as.numeric(year_cols)

  if (length(years) < 3) stop("Not enough yearly columns for trend analysis.")

  res <- lapply(seq_len(nrow(df)), function(i) {
    vals <- as.numeric(df[i, ..year_cols])
    # handle NAs by pairwise filtering with years
    ok <- is.finite(vals)
    vals <- vals[ok]
    yrs  <- years[ok]
    if (length(vals) < 3) {
      return(data.frame(
        Station     = df[[station_col]][i],
        Tau         = NA_real_,
        P_value     = NA_real_,
        Sen_slope   = NA_real_,
        Trend       = NA_character_,
        Significant = NA_character_
      ))
    }
    mk <- Kendall::MannKendall(vals)
    slope <- tryCatch(zyp::zyp.sen(vals ~ yrs)$coefficients[2], error = function(e) NA_real_)
    trend_dir <- if (!is.na(mk$tau) && mk$tau > 0) "increasing" else "decreasing"
    sig <- if (!is.na(mk$sl) && mk$sl < alpha) "Significant" else "Not Significant"

    data.frame(
      Station     = df[[station_col]][i],
      Tau         = unname(mk$tau),
      P_value     = unname(mk$sl),
      Sen_slope   = slope,
      Trend       = trend_dir,
      Significant = sig
    )
  })

  res_dt <- rbindlist(res, use.names = TRUE, fill = TRUE)
  fwrite(res_dt, "mann_kendall_per_station_results.csv")
}

# =========================================================
# D) Distribution shift tests (CvM, MWU, KS) per Perc × Days
# =========================================================
# Input: CHDEs_Stattests.csv with columns:
#   Perc, Days, model (values: "1980-2001" / "2002-2023"), var (numeric)
# Outputs:
#   disttest_cvm_results.csv
#   disttest_mwu_results.csv
#   disttest_ks_results.csv

run_distribution_tests <- function(infile = "CHDEs_Stattests.csv", alpha = 0.05) {
  stopifnot(file.exists(infile))
  df1 <- fread(infile)
  need <- c("Perc", "Days", "model", "var")
  if (!all(need %in% names(df1))) stop("Input must contain columns: ", paste(need, collapse=", "))

  perc_levels <- sort(unique(df1$Perc))
  day_levels  <- sort(unique(df1$Days))

  # CvM
  cvm_list <- list()
  for (p in perc_levels) {
    for (d in day_levels) {
      sub <- df1 %>% filter(Perc == p, Days == d)
      a <- sub$var[sub$model == "1980-2001"]
      b <- sub$var[sub$model == "2002-2023"]
      a <- a[is.finite(a)]; b <- b[is.finite(b)]
      if (length(a) > 1 && length(b) > 1) {
        ct <- cramer::cramer.test(a, b)
        cvm_list[[length(cvm_list)+1]] <- data.frame(
          Percentile   = p,
          Days         = d,
          CVM_stat     = unname(ct$statistic),
          P_value      = unname(ct$p.value),
          Significant  = ct$p.value <= alpha
        )
      }
    }
  }
  cvm_out <- rbindlist(cvm_list)
  fwrite(cvm_out, "disttest_cvm_results.csv")

  # MWU
  mwu_list <- list()
  for (p in perc_levels) {
    for (d in day_levels) {
      sub <- df1 %>% filter(Perc == p, Days == d)
      if (nrow(sub) > 0) {
        wt <- tryCatch(
          wilcox.test(var ~ model, data = sub),
          error = function(e) NULL
        )
        if (!is.null(wt)) {
          mwu_list[[length(mwu_list)+1]] <- data.frame(
            Percentile  = p,
            Days        = d,
            P_value     = unname(wt$p.value),
            Significant = (wt$p.value <= alpha)
          )
        }
      }
    }
  }
  mwu_out <- rbindlist(mwu_list)
  fwrite(mwu_out, "disttest_mwu_results.csv")

  # KS
  ks_list <- list()
  for (p in perc_levels) {
    for (d in day_levels) {
      a <- df1$var[df1$Perc == p & df1$Days == d & df1$model == "1980-2001"]
      b <- df1$var[df1$Perc == p & df1$Days == d & df1$model == "2002-2023"]
      a <- a[is.finite(a)]; b <- b[is.finite(b)]
      if (length(a) > 1 && length(b) > 1) {
        kt <- ks.test(a, b)
        ks_list[[length(ks_list)+1]] <- data.frame(
          Percentile  = p,
          Days        = d,
          P_value     = unname(kt$p.value),
          Significant = (kt$p.value <= alpha)
        )
      }
    }
  }
  ks_out <- rbindlist(ks_list)
  fwrite(ks_out, "disttest_ks_results.csv")
}

# =====================
# Execute all sections:
# =====================
if (file.exists("CHDEs_MMKT.csv"))            run_single_series_mk()
if (file.exists("Dryspells_Trend.csv"))       run_modified_mk_single()
if (file.exists("yearly_means_arranged.csv")) run_station_mk()
if (file.exists("CHDEs_Stattests.csv"))       run_distribution_tests()

cat("Done. Results written to CSV files in the working directory.\n")
