# =============================================================================
# 00 — SETUP: packages, paths, analysis parameters, small helpers
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(lme4)
  library(nnet)
  library(broom)
  library(broom.mixed)
  library(cluster)
  library(scales)
})

# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------
# The four raw CSV files are proprietary and not part of this repository.
# Point DATA_DIR to the folder that contains them (or set the environment
# variable PRICE_DATA_DIR before running).
DATA_DIR <- Sys.getenv("PRICE_DATA_DIR", unset = "data")
OUT_DIR  <- Sys.getenv("PRICE_OUT_DIR",  unset = "output")

dir.create(file.path(OUT_DIR, "tables"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT_DIR, "figures"), recursive = TRUE, showWarnings = FALSE)

RAW_FILES <- tribble(
  ~file,                                            ~country, ~product_category,
  "competitor_price_data_mobile_phones_be.csv",     "BE",     "mobile_phones",
  "competitor_price_data_mobile_phones_nl.csv",     "NL",     "mobile_phones",
  "competitor_price_data_vacuum_cleaners_be.csv",   "BE",     "vacuum_cleaners",
  "competitor_price_data_vacuum_cleaners_nl.csv",   "NL",     "vacuum_cleaners"
)

# Website names of the focal retailer ("Company X") as they appear in the
# competitor_name column. Set the environment variable FOCAL_NAMES to a
# comma-separated list, e.g. FOCAL_NAMES="companyx.nl,companyx.be".
FOCAL_NAMES <- strsplit(Sys.getenv("FOCAL_NAMES", unset = "company_x.nl,company_x.be"), ",")[[1]]

# -----------------------------------------------------------------------------
# Analysis parameters (one place, used by every script)
# -----------------------------------------------------------------------------
RESPONSE_WINDOW_H  <- 24                  # follower: competitor moves within 24h after the event
LEAD_WINDOW_H      <- 6                   # leader:   competitor moves within 6h before the event
ACTIVE_LOOKBACK_H  <- 72                  # competitor is "in the market" for the product if it
                                          # posted a price in the 72h before the event
ROBUSTNESS_WINDOWS <- c(6, 24, 48, 72, 168)
GAP_THRESHOLDS     <- c(0.01, 0.02, 0.03, 0.05, 0.07, 0.10, 0.12, 0.15, 0.20, 0.25)
MIN_EXPOSURE_PAIRS <- 200                 # minimum event-competitor pairs to enter the clustering
N_CLUSTERS         <- 3
SEED               <- 123

set.seed(SEED)
theme_set(theme_minimal(base_size = 12))

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
to_binary01 <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  case_when(
    x_chr %in% c("1", "true", "yes", "y")  ~ 1L,
    x_chr %in% c("0", "false", "no", "n")  ~ 0L,
    TRUE                                    ~ NA_integer_
  )
}

sig_stars <- function(p) {
  case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p < 0.10 ~ ".", TRUE ~ "")
}

save_table <- function(x, name) {
  write_csv(x, file.path(OUT_DIR, "tables", paste0(name, ".csv")))
  invisible(x)
}

save_plot <- function(p, name, width = 10, height = 6) {
  ggsave(file.path(OUT_DIR, "figures", paste0(name, ".png")), p,
         width = width, height = height, dpi = 300, bg = "white")
  invisible(p)
}

nice_cat <- function(x) recode(x, mobile_phones = "Mobile Phones", vacuum_cleaners = "Vacuum Cleaners")

zscore <- function(x) as.numeric(scale(x))

# Area under the ROC curve via the rank (Mann-Whitney) formula
auc_rank <- function(y, p) {
  r <- rank(p)
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

log_step <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
