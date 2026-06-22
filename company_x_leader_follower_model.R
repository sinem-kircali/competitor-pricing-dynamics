# Company X Leader-Follower Model Script

# This R script constructs an event‑level dataset of price changes in
# Company X and its competitors, classifies each competitor as a
# leader or follower relative to Company X, and fits a logistic mixed
# effects model to identify factors associated with leadership or
# follower behaviour.  The code is designed for the datasets
# provided in the case study: competitor_price_data_mobile_phones_be.csv,
# competitor_price_data_mobile_phones_nl.csv,
# competitor_price_data_vacuum_cleaners_be.csv, and
# competitor_price_data_vacuum_cleaners_nl.csv.

## Load required packages
library(data.table)
library(dplyr)
library(lubridate)
library(lme4)  # for logistic mixed model

## 1. Read and combine datasets
# Define file paths – adjust if your data are stored elsewhere
files <- list(
  "mobile_phones_be"  = "competitor_price_data_mobile_phones_be.csv",
  "mobile_phones_nl"  = "competitor_price_data_mobile_phones_nl.csv",
  "vacuum_cleaners_be"= "competitor_price_data_vacuum_cleaners_be.csv",
  "vacuum_cleaners_nl"= "competitor_price_data_vacuum_cleaners_nl.csv"
)

# Load each dataset into a list of data.tables
dat_list <- lapply(names(files), function(name) {
  dt <- fread(files[[name]])
  dt$category_country <- name  # keep track of category and country
  return(dt)
})
names(dat_list) <- names(files)

# Combine into one data.table
dt_all <- rbindlist(dat_list, use.names = TRUE, fill = TRUE)

## 2. Prepare data: parse timestamps and ensure appropriate ordering
# Ensure that the price_time column is in POSIXct format; adjust the name
# if your timestamp column differs.
dt_all[, price_time := as.POSIXct(price_time, tz = "Europe/Amsterdam")]

## 3. Create event‑level dataset
# We are interested in price change events where either Company X or a
# competitor changed price.  We will create a unified table of
# price change events with indicator variables showing who moved and
# when.

# First, separate Company X records and competitor records
cb_dt <- dt_all[competitor_name == "Company X"]
comp_dt <- dt_all[competitor_name != "Company X"]

# Identify price changes for each product–retailer pair by comparing
# consecutive rows.  We'll flag rows where the price differs from the
# previous observation for the same product and retailer.
cb_dt <- cb_dt[order(product_id, price_time)]
cb_dt[, cb_price_change := c(FALSE, diff(price) != 0), by = .(product_id, competitor_name)]
comp_dt <- comp_dt[order(product_id, competitor_name, price_time)]
comp_dt[, comp_price_change := c(FALSE, diff(price) != 0), by = .(product_id, competitor_name)]

# Filter to only rows with price changes
cb_changes <- cb_dt[cb_price_change == TRUE]
comp_changes <- comp_dt[comp_price_change == TRUE]

# For each competitor price change event, identify whether the competitor
# leads or follows relative to Company X.  A competitor leads if it
# changes price before Company X changes the price within a 24‑hour
# window.  A competitor follows if Company X changes price first and
# the competitor changes within the same 24‑hour window.

# We'll loop over competitor price change events and check the nearest
# Company X price change event in a ±24‑hour window.

## Function to classify an event as leader or follower
classify_event <- function(comp_event, cb_events) {
  # Inputs:
  # comp_event: single row of competitor change (data.table)
  # cb_events: data.table of Company X price change events for the same product
  # Returns: factor with levels 'leader', 'follower', or NA
  t_comp <- comp_event$price_time
  product <- comp_event$product_id
  # Filter Company X events for this product
  cb_prod <- cb_events[product_id == product]
  # Find any Company X changes within ±24h
  window_start <- t_comp - hours(24)
  window_end   <- t_comp + hours(24)
  cb_window <- cb_prod[price_time >= window_start & price_time <= window_end]
  if (nrow(cb_window) == 0) {
    return(NA_character_)  # no Company X change within window; unclassified
  }
  # Determine if Company X changed before or after the competitor
  # The event with the smaller absolute time difference defines the ordering
  # If Company X's change time < comp time -> competitor is follower (Company X leads)
  # If Company X's change time > comp time -> competitor is leader
  # In case of ties, treat competitor as follower
  cb_window[, time_diff := as.numeric(difftime(price_time, t_comp, units = "hours"))]
  # Select the nearest Company X event
  nearest_cb <- cb_window[which.min(abs(time_diff))]
  if (nearest_cb$price_time < t_comp) {
    return("follower")
  } else {
    return("leader")
  }
}

# Apply classification to competitor events per product
comp_changes[, role := classify_event(.SD, cb_changes), by = .(product_id, price_time, competitor_name)]

# Remove unclassified events
comp_changes <- comp_changes[!is.na(role)]

## 4. Construct modelling dataset
# Select features relevant for modelling.  The dataset includes many
# variables; here we select a subset that may influence leadership vs
# follower status.  You can expand this list based on business
# understanding.
model_dt <- comp_changes %>%
  mutate(
    abs_cb_change_pct = abs(cb_change_pct),  # absolute Company X price change percentage (if available)
    competitor_relevance = rel_relevance,    # competitor relevance score
    assortment_overlap  = overlap,          # overlap score
    price_age_hours     = price_age,        # age of competitor's price in hours
    price_gap           = price_gap,        # difference between competitor and Company X price
    promo               = promo_flag,       # whether competitor price is a promotion
    next_day_delivery   = next_day_delivery,# competitor offers next day delivery
    in_stock            = in_stock,         # competitor has stock
    country_category    = category_country  # country and category identifier
  ) %>%
  select(role, competitor_id, product_id, abs_cb_change_pct, competitor_relevance,
         assortment_overlap, price_age_hours, price_gap, promo, next_day_delivery,
         in_stock, country_category)

# Convert categorical variables to factors
model_dt$role <- factor(model_dt$role, levels = c("follower", "leader"))
model_dt$competitor_id <- factor(model_dt$competitor_id)
model_dt$product_id    <- factor(model_dt$product_id)
model_dt$country_category <- factor(model_dt$country_category)

## 5. Fit logistic mixed effects model
# We use a logistic mixed model with random intercepts for competitor and product.
formula <- role ~ abs_cb_change_pct + competitor_relevance + assortment_overlap +
  price_age_hours + price_gap + promo + next_day_delivery + in_stock +
  country_category + (1 | competitor_id) + (1 | product_id)

# Fit the model using glmer (binomial family)
leader_model <- glmer(formula, data = model_dt, family = binomial(link = "logit"),
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

## 6. Summarise results
summary(leader_model)

## 7. Save the model object for further analysis (optional)
# saveRDS(leader_model, file = "leader_model.rds")

## Notes:
# - This script assumes that the dataset contains columns: price_time, product_id,
#   competitor_name, price, cb_change_pct (Company X's price change percentage),
#   rel_relevance, overlap, price_age, price_gap, promo_flag,
#   next_day_delivery, in_stock, and category_country.  Adjust variable names
#   according to your dataset.
# - The classification of leader vs follower uses a symmetric ±24h window.
#   You can modify the window size (e.g., 6h, 12h, 24h) by changing the
#   hours() argument in classify_event.
# - The model can be extended with additional predictors or random slopes.