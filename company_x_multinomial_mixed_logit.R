# Company X Case — Multinomial Mixed Logit (Event‑level competitor reactions)
#
# This script prepares an event–level dataset from the Company X competitor price data
# and fits a multinomial mixed–effects logit model.  Each Company X price change is
# treated as an event, and each considered competitor is observed for their first
# response (no change, price decrease, or price increase) within a 24‑hour window.
# Random intercepts for competitors and products capture unobserved heterogeneity.
#
# Author: ChatGPT (generated)
# Last updated: Feb 2026

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# ---------------------------------------------------------
# 1. Load and clean data
# ---------------------------------------------------------
# Four CSV files live in the working directory.  Adjust the paths below if needed.
phones_BE <- read_csv("competitor_price_data_mobile_phones_be.csv", show_col_types = FALSE) %>%
  mutate(country = "BE", category = "mobile_phones")
phones_NL <- read_csv("competitor_price_data_mobile_phones_nl.csv", show_col_types = FALSE) %>%
  mutate(country = "NL", category = "mobile_phones")
vac_BE <- read_csv("competitor_price_data_vacuum_cleaners_be.csv", show_col_types = FALSE) %>%
  mutate(country = "BE", category = "vacuum_cleaners")
vac_NL <- read_csv("competitor_price_data_vacuum_cleaners_nl.csv", show_col_types = FALSE) %>%
  mutate(country = "NL", category = "vacuum_cleaners")

df <- bind_rows(phones_BE, phones_NL, vac_BE, vac_NL) %>%
  mutate(
    # Parse date/time columns
    scrape_datetime = ymd_hms(scrape_datetime, quiet = TRUE),
    run_datetime    = ymd_hms(run_datetime, quiet = TRUE),
    run_date        = ymd(run_date, quiet = TRUE),
    # Identify Company X rows
    is_company_x     = competitor_name %in% c("company_x.nl", "company_x.be"),
    # Price age sometimes negative: truncate at 0
    price_age       = pmax(coalesce(price_age, 0), 0)
  ) %>%
  arrange(product_id, country, category, competitor_id, scrape_datetime, run_datetime)

# Remove duplicate scrape observations by keeping the last run for each (product, competitor, timestamp)
df_u <- df %>%
  group_by(product_id, country, category, competitor_id, scrape_datetime) %>%
  slice_tail(n = 1) %>%
  ungroup()

# ---------------------------------------------------------
# 2. Construct Company X price‑change events
# ---------------------------------------------------------
cb_events <- df_u %>%
  filter(is_company_x) %>%
  arrange(product_id, country, category, scrape_datetime) %>%
  group_by(product_id, country, category) %>%
  mutate(
    cb_price_before   = lag(price),
    cb_price_change   = price - cb_price_before,
    cb_price_change_pct = cb_price_change / cb_price_before
  ) %>%
  ungroup() %>%
  # Keep only actual price changes (non‑zero change with valid lag)
  filter(!is.na(cb_price_before), cb_price_change != 0) %>%
  transmute(
    product_id, country, category,
    event_time         = scrape_datetime,
    cb_price_before,
    cb_price_after     = price,
    cb_price_change_pct,
    abs_cb_change_pct  = abs(cb_price_change_pct),
    cb_direction       = if_else(cb_price_change > 0, "increase", "decrease"),
    promo              = as.integer(coalesce(is_company_x_promotion, 0))
  )

# ---------------------------------------------------------
# 3. Identify competitor price changes and select top competitors
# ---------------------------------------------------------
comp <- df_u %>%
  filter(!is_company_x, is_considered == TRUE) %>%
  arrange(product_id, country, category, competitor_id, scrape_datetime) %>%
  group_by(product_id, country, category, competitor_id) %>%
  mutate(
    comp_price_before = lag(price),
    comp_change       = price - comp_price_before,
    is_comp_change    = !is.na(comp_price_before) & comp_change != 0,
    comp_change_pct   = comp_change / comp_price_before
  ) %>%
  ungroup()

comp_changes <- comp %>%
  filter(is_comp_change) %>%
  select(product_id, country, category, competitor_id,
         chg_time = scrape_datetime, comp_change_pct)

# Keep at most K competitors per (product, country, category) based on mean relevance
K <- 8
top_comp <- comp %>%
  group_by(product_id, country, category, competitor_id) %>%
  summarise(mean_rel = mean(coalesce(competitor_relevance_score, 0), na.rm = TRUE), .groups = "drop") %>%
  arrange(product_id, country, category, desc(mean_rel)) %>%
  group_by(product_id, country, category) %>%
  slice_head(n = K) %>%
  ungroup()

comp_top       <- comp %>% semi_join(top_comp, by = c("product_id", "country", "category", "competitor_id"))
comp_changes_top <- comp_changes %>% semi_join(top_comp, by = c("product_id", "country", "category", "competitor_id"))

# ---------------------------------------------------------
# 4. Build event × competitor panel
# ---------------------------------------------------------
# For each Company X event and each top competitor, derive baseline features at the moment of the event
# and identify the competitor’s first price change within 24h.  If no change occurs, the outcome is
# "no_change"; otherwise, classify as "increase" or "decrease" based on the sign of the price change.

WINDOW_H <- 24

# Helper: baseline at event for competitor
baseline_at_event <- function(evs, comp_series) {
  comp_series <- comp_series %>%
    filter(!is.na(scrape_datetime)) %>%
    arrange(scrape_datetime)
  
  evs <- evs %>% arrange(event_time)
  
  idx <- findInterval(as.numeric(evs$event_time),
                      as.numeric(comp_series$scrape_datetime))
  
  idx[idx == 0] <- NA_integer_
  out <- comp_series[idx, , drop = FALSE]
  out$event_time <- evs$event_time
  out
}



# Helper: first competitor change after event
first_change_after <- function(events, chg_series, window_h = 24) {
  
  events <- events %>%
    filter(!is.na(event_time)) %>%     # <- EKLE
    arrange(event_time)
  
  chg_series <- chg_series %>%
    filter(!is.na(chg_time)) %>%       # <- EKLE
    arrange(chg_time)
  
  if (nrow(chg_series) == 0) {
    return(tibble(
      resp_time_hours = rep(NA_real_, nrow(events)),
      comp_change_pct = rep(0, nrow(events)),
      resp_class = rep("no_change", nrow(events))
    ))
  }
  
  idx <- findInterval(as.numeric(events$event_time),
                      as.numeric(chg_series$chg_time)) + 1L
  idx[idx > nrow(chg_series)] <- NA_integer_
  
  chg <- chg_series[idx, , drop = FALSE]
  dt_h <- as.numeric(difftime(chg$chg_time, events$event_time, units = "hours"))
  within <- !is.na(dt_h) & dt_h <= window_h
  
  tibble(
    resp_time_hours = if_else(within, dt_h, NA_real_),
    comp_change_pct = if_else(within, chg$comp_change_pct, 0),
    resp_class = case_when(
      within & chg$comp_change_pct < 0 ~ "decrease",
      within & chg$comp_change_pct > 0 ~ "increase",
      TRUE ~ "no_change"
    )
  )
}


event_comp_list <- list()
group_keys <- cb_events %>% distinct(product_id, country, category)

for (i in seq_len(nrow(group_keys))) {
  pid  <- group_keys$product_id[i]
  ctry <- group_keys$country[i]
  catg <- group_keys$category[i]
  evs  <- cb_events %>% filter(product_id == pid, country == ctry, category == catg)
  cands <- top_comp %>% filter(product_id == pid, country == ctry, category == catg)
  if (nrow(cands) == 0) next
  for (cid in unique(cands$competitor_id)) {
    comp_series <- comp_top %>%
      filter(product_id == pid, country == ctry, category == catg, competitor_id == cid) %>%
      filter(!is.na(scrape_datetime)) %>%      # <- BU
      arrange(scrape_datetime)
    base <- baseline_at_event(evs, comp_series %>% select(
      scrape_datetime, competitor_id, competitor_name, price, delivery_cost, is_in_stock,
      price_age, is_next_day_delivery, review_bucket, competitor_relevance_score,
      assortment_overlap, search_impression_share, shopping_impression_share)) %>%
      select(-scrape_datetime)
    chg_series <- comp_changes_top %>%
      filter(product_id == pid, country == ctry, category == catg, competitor_id == cid) %>%
      arrange(chg_time)
    resp <- first_change_after(evs, chg_series)
    event_comp_list[[length(event_comp_list) + 1L]] <- bind_cols(
      evs,
      base %>% select(competitor_id, competitor_name, price, delivery_cost, is_in_stock,
                       price_age, is_next_day_delivery, review_bucket,
                       competitor_relevance_score, assortment_overlap,
                       search_impression_share, shopping_impression_share),
      resp
    )
  }
}

ec <- bind_rows(event_comp_list) %>%
  mutate(
    resp_class    = factor(resp_class, levels = c("no_change", "decrease", "increase")),
    cb_direction  = factor(cb_direction),
    # engineered baseline features
    price_gap_pct     = (price - cb_price_before) / cb_price_before,
    abs_price_gap_pct = abs(price_gap_pct),
    rel_delivery_cost = coalesce(delivery_cost, 0) / cb_price_before,
    log_price_age     = log1p(price_age),
    rel_relevance     = coalesce(competitor_relevance_score, 0),
    in_stock          = as.integer(coalesce(is_in_stock, FALSE)),
    next_day          = as.integer(coalesce(is_next_day_delivery, FALSE))
  ) %>%
  drop_na(resp_class)

# ---------------------------------------------------------
# 5. Fit a multinomial mixed logit
# ---------------------------------------------------------
# We recommend using the brms package for a true mixed multinomial model.  The
# following skeleton shows how to call brms.  Uncomment and run if you have
# brms and cmdstanr installed.  You may need to increase memory/time for the
# full dataset.

install.packages("brms")
install.packages("rstan")
library(brms)
install.packages("rstan", dependencies = TRUE)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



m1 <- brm(
resp_class ~ abs_cb_change_pct + cb_direction + promo +
rel_relevance + assortment_overlap + log_price_age +
in_stock + next_day + abs_price_gap_pct + rel_delivery_cost +
country + category +
     (1 | competitor_id) + (1 | product_id),
   data   = ec,
  family = categorical(link = "logit"),
  chains = 4, cores = 4, iter = 2000,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)
  summary(m1)

# As a lightweight alternative for quick exploration, you can treat competitor_id
# and product_id as fixed effects (categorical variables) and use the nnet package.
# Example:

# library(nnet)
# m_fixed <- multinom(
#   resp_class ~ abs_cb_change_pct + cb_direction + promo +
#     rel_relevance + assortment_overlap + log_price_age +
#     in_stock + next_day + abs_price_gap_pct + rel_delivery_cost +
#     country + category + as.factor(competitor_id) + as.factor(product_id),
#   data = ec,
#   MaxNWts = 100000,
#   trace = FALSE
# )
# summary(m_fixed)

# ---------------------------------------------------------
# End of script