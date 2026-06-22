# Company X Leader–Follower Multinomial Model Script

# This script reads four CSV datasets of competitor pricing data, constructs
# a leader/follower/inactive classification for competitor price changes
# relative to Company X’s price changes, and estimates a multinomial
# logistic model (with competitor and product fixed effects) to explain
# leadership versus followership.  The code is designed for the Company X
# case study and produces a summary table that can be formatted for
# inclusion in a report.

## Load required libraries
library(data.table)
library(dplyr)
library(lubridate)
library(nnet)    # for multinomial logistic regression

## 1. Read and combine datasets
# Adjust the file paths if necessary; all files should be located in
# the working directory or specify absolute paths.
files <- list(
  phones_BE  = "competitor_price_data_mobile_phones_be.csv",
  phones_NL  = "competitor_price_data_mobile_phones_nl.csv",
  vac_BE     = "competitor_price_data_vacuum_cleaners_be.csv",
  vac_NL     = "competitor_price_data_vacuum_cleaners_nl.csv"
)
dat_list <- lapply(names(files), function(name) {
  dt <- fread(files[[name]])
  dt$dataset <- name
  dt$country <- ifelse(grepl("_BE", name), "BE", "NL")
  dt$category <- ifelse(grepl("phones", name), "mobile_phones", "vacuum_cleaners")
  return(dt)
})
dt_all <- rbindlist(dat_list, use.names = TRUE, fill = TRUE)

## 2. Parse timestamps
dt_all[, scrape_datetime := as.POSIXct(scrape_datetime, tz = "Europe/Amsterdam")]

## 3. Separate Company X and competitor rows
cb_rows   <- dt_all[competitor_name == "Company X"]
comp_rows <- dt_all[competitor_name != "Company X"]

# Identify price changes for each seller–product pair
cb_rows <- cb_rows[order(product_id, scrape_datetime)]
cb_rows[, cb_price_change := c(FALSE, diff(price) != 0), by = .(product_id)]
comp_rows <- comp_rows[order(product_id, competitor_id, scrape_datetime)]
comp_rows[, comp_price_change := c(FALSE, diff(price) != 0), by = .(product_id, competitor_id)]

# Extract change events
cb_changes   <- cb_rows[cb_price_change == TRUE]
comp_changes <- comp_rows[comp_price_change == TRUE]

## 4. Classify competitor events as leader, follower, or inactive

# Define classification function
classify_role <- function(comp_event, cb_events) {
  t_comp <- comp_event$scrape_datetime
  product <- comp_event$product_id
  # filter Company X events for this product
  cb_prod <- cb_events[product_id == product]
  # define 24h window
  window_start <- t_comp - hours(24)
  window_end   <- t_comp + hours(24)
  cb_window <- cb_prod[scrape_datetime >= window_start & scrape_datetime <= window_end]
  if (nrow(cb_window) == 0) return("inactive")
  # find nearest Company X change
  cb_window[, time_diff := as.numeric(difftime(scrape_datetime, t_comp, units = "hours"))]
  nearest <- cb_window[which.min(abs(time_diff))]
  if (nearest$scrape_datetime < t_comp) {
    return("follower")  # Company X moved first
  } else {
    return("leader")    # competitor moved first
  }
}

# Apply classification per competitor change event
comp_changes[, role := classify_role(.SD, cb_changes), by = .(product_id, scrape_datetime, competitor_id)]

# Remove events with NA role (should not occur)
comp_changes <- comp_changes[!is.na(role)]

## 5. Construct modelling data with selected predictors
model_dt <- comp_changes %>%
  mutate(
    abs_cb_change_pct = abs(cb_change_pct),    # absolute Company X price change
    cb_direction      = factor(sign(cb_change_pct), levels = c(-1,0,1), labels = c("decrease","none","increase")),
    competitor_relevance = competitor_relevance_score,
    assortment_overlap   = assortment_overlap,
    price_age_hours      = pmax(price_age, 0),
    price_gap            = price - cb_price_at_scrape, # difference between competitor price and Company X price at event time
    promo_cb            = is_company_x_promotion,
    in_stock_comp       = is_in_stock,
    next_day_delivery   = is_next_day_delivery,
    role                = factor(role, levels = c("inactive","follower","leader"))
  ) %>%
  select(role, competitor_id, product_id, abs_cb_change_pct, cb_direction,
         competitor_relevance, assortment_overlap, price_age_hours,
         price_gap, promo_cb, in_stock_comp, next_day_delivery,
         country, category)

# Convert necessary variables to factors
model_dt$competitor_id <- factor(model_dt$competitor_id)
model_dt$product_id    <- factor(model_dt$product_id)
model_dt$country       <- factor(model_dt$country)
model_dt$category      <- factor(model_dt$category)

## 6. Fit multinomial logit model
# Use competitor_id and product_id as fixed effects (no random intercept)
m_formula <- as.formula(
  role ~ abs_cb_change_pct + cb_direction + competitor_relevance +
    assortment_overlap + price_age_hours + price_gap + promo_cb +
    in_stock_comp + next_day_delivery + country + category +
    competitor_id + product_id
)

# Fit the model using nnet::multinom (baseline category = 'inactive')
mult_model <- multinom(m_formula, data = model_dt, Hess = TRUE, trace = FALSE)

## 7. Summarise coefficients and compute standard errors
coef_table <- summary(mult_model)$coefficients
se_table   <- summary(mult_model)$standard.errors
z_table    <- coef_table / se_table
p_values   <- 2 * (1 - pnorm(abs(z_table)))

# Combine into a data.frame for export
results_df <- as.data.frame(coef_table)
results_df$parameter <- rownames(results_df)
results_df$std_error <- as.vector(se_table)
results_df$z_value   <- as.vector(z_table)
results_df$p_value   <- as.vector(p_values)

# Save results to CSV for convenience
write.csv(results_df, file = "multinomial_results_table.csv", row.names = FALSE)

## 8. Print summary to console
print("Multinomial model fitted. Coefficient table saved to multinomial_results_table.csv")

## Notes
# - This script requires the variables cb_change_pct and cb_price_at_scrape to exist.
#   If your dataset uses different names for Company X price change percentage and
#   Company X price at the event time, adjust the code accordingly.
# - nnet::multinom uses maximum likelihood without random effects.  To
#   approximate random intercepts, we include competitor_id and product_id
#   as fixed effects.  For a true mixed model, one could use packages like
#   brms or glmmTMB, though these require more computational resources.