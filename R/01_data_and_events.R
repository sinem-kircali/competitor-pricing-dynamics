# =============================================================================
# 01 — DATA PREPARATION AND EVENT CONSTRUCTION            (Report §3.1–3.2)
# -----------------------------------------------------------------------------
# Output objects used by all later scripts:
#   df          cleaned product × retailer × timestamp panel
#   cx_events   one row per Company X price change (the "event")
#   event_level one row per event × competitor that is active on the product
#               (the backbone of every model)
# =============================================================================

log_step("01  Loading raw data")

read_market <- function(file, country, product_category) {
  read_csv(file.path(DATA_DIR, file), show_col_types = FALSE, progress = FALSE,
           col_types = cols(.default = col_guess(), review_bucket = col_character())) %>%
    mutate(country = country, product_category = product_category)
}

raw <- pmap_dfr(RAW_FILES, read_market) %>%
  # the promotion / "sold by" flags carry the focal retailer's name in the raw
  # column names; give them neutral names
  rename(cx_promo_flag = ends_with("_promotion"), sold_by_cx = starts_with("sold_by_"))

n_raw_rows <- nrow(raw)

# -----------------------------------------------------------------------------
# 1. Clean panel
# -----------------------------------------------------------------------------
# The raw files repeat the same scrape once per hourly run (same scrape time,
# same price). Keep one row per product × country × retailer × scrape time
# (the first run), so that every observation is a distinct price observation.
df <- raw %>%
  arrange(product_id, country, competitor_id, scrape_datetime, run_datetime) %>%
  distinct(product_id, country, competitor_id, scrape_datetime, .keep_all = TRUE) %>%
  mutate(
    is_cx                = competitor_name %in% FOCAL_NAMES,
    is_in_stock          = to_binary01(is_in_stock),
    is_next_day_delivery = to_binary01(is_next_day_delivery),
    cx_promo_flag        = to_binary01(cx_promo_flag),
    price_age            = na_if(price_age, -1),        # -1 = unknown price age
    hour_of_day          = hour(scrape_datetime),
    day_of_week          = wday(scrape_datetime, label = TRUE, week_start = 1),
    is_weekend           = as.integer(day_of_week %in% c("Sat", "Sun"))
  ) %>%
  group_by(product_id, country, competitor_id) %>%
  mutate(
    price_lag        = lag(price),
    price_change_pct = (price - price_lag) / price_lag,
    is_price_change  = as.integer(!is.na(price_lag) & price != price_lag),
    change_direction = case_when(
      is_price_change == 1 & price > price_lag ~ "increase",
      is_price_change == 1 & price < price_lag ~ "decrease",
      TRUE                                     ~ "no_change"
    )
  ) %>%
  ungroup()

if (!any(df$is_cx)) {
  stop("No rows match FOCAL_NAMES. Set FOCAL_NAMES to the focal retailer's ",
       "website names as they appear in competitor_name.")
}

# -----------------------------------------------------------------------------
# 2. Company X price-change events
# -----------------------------------------------------------------------------
cx_events <- df %>%
  filter(is_cx, is_price_change == 1) %>%
  transmute(
    product_id, country, product_category,
    cx_event_time      = scrape_datetime,
    cx_price           = price,                 # Company X's NEW price
    cx_price_change_pct = price_change_pct,
    cx_abs_change      = abs(price_change_pct),
    cx_direction       = change_direction,      # increase / decrease
    cx_promo           = cx_promo_flag,
    cx_in_stock        = is_in_stock,
    cx_next_day        = is_next_day_delivery,
    cx_delivery_cost   = delivery_cost,
    cx_price_age       = price_age,
    cx_hour            = hour_of_day,
    cx_is_weekend      = is_weekend
  )

# -----------------------------------------------------------------------------
# 3. Competitor-level attributes
# -----------------------------------------------------------------------------
comp_rows <- df %>% filter(!is_cx)

competitor_attributes <- comp_rows %>%
  group_by(country, competitor_id, competitor_name) %>%
  summarise(
    competitor_relevance = mean(competitor_relevance_score, na.rm = TRUE),
    assortment_overlap   = mean(assortment_overlap, na.rm = TRUE),
    n_changes            = sum(is_price_change),
    n_days               = as.numeric(difftime(max(scrape_datetime), min(scrape_datetime), units = "days")),
    .groups = "drop"
  ) %>%
  mutate(
    across(c(competitor_relevance, assortment_overlap), ~ ifelse(is.nan(.x), NA_real_, .x)),
    # structural repricing speed: price changes per day across all products
    reprice_freq = n_changes / pmax(n_days, 1)
  ) %>%
  select(country, competitor_id, competitor_name, competitor_relevance,
         assortment_overlap, reprice_freq)

# -----------------------------------------------------------------------------
# 4. Event × competitor panel (risk set)
# -----------------------------------------------------------------------------
# risk_set = "listing": a competitor is part of an event's risk set only if it
#   offers the same product in the same country and posted a price for it in
#   the `lookback_h` hours before Company X's price change (§3.1: competition is
#   defined at the product–market level). Retailers that do not sell the
#   product are therefore not counted as "non-responders".
# risk_set = "country": every competitor active in the country with assortment
#   overlap > 0, paired with every event (the grid behind the report's figures).
#   Competitors without a price for the product have a missing price gap.
log_step("01  Building event x competitor panel (risk set: ", RISK_SET, ")")

comp_obs <- comp_rows %>%
  select(product_id, country, competitor_id, obs_time = scrape_datetime, comp_price_before = price)

comp_changes <- comp_rows %>%
  filter(is_price_change == 1) %>%
  select(product_id, country, competitor_id, change_time = scrape_datetime,
         comp_change_pct = price_change_pct, comp_change_direction = change_direction,
         comp_price_after = price)

build_event_level <- function(risk_set = RISK_SET, lookback_h = ACTIVE_LOOKBACK_H) {

  candidates <- if (risk_set == "listing") {
    comp_rows %>% distinct(product_id, country, competitor_id)
  } else {
    competitor_attributes %>%
      filter(!is.na(assortment_overlap), assortment_overlap > 0) %>%
      select(country, competitor_id)
  }

  event_grid <- cx_events %>%
    select(product_id, country, cx_event_time) %>%
    inner_join(candidates, by = intersect(c("product_id", "country"), names(candidates)),
               relationship = "many-to-many") %>%
    # last competitor price for the product strictly before the event
    left_join(comp_obs, by = join_by(product_id, country, competitor_id,
                                     closest(cx_event_time > obs_time))) %>%
    mutate(hours_since_last_price = as.numeric(difftime(cx_event_time, obs_time, units = "hours")))

  if (risk_set == "listing") {
    event_grid <- event_grid %>%
      filter(!is.na(obs_time), hours_since_last_price <= lookback_h)
  }
  event_grid <- event_grid %>% select(-obs_time, -hours_since_last_price)

  # first competitor price change AFTER the event (response) ...
  first_after <- event_grid %>%
    select(product_id, country, competitor_id, cx_event_time) %>%
    left_join(comp_changes, by = join_by(product_id, country, competitor_id,
                                         closest(cx_event_time < change_time))) %>%
    transmute(product_id, country, competitor_id, cx_event_time,
              response_delay_h = as.numeric(difftime(change_time, cx_event_time, units = "hours")),
              response_change_pct = comp_change_pct,
              response_direction  = comp_change_direction)

  # ... and last competitor price change BEFORE the event (leadership)
  last_before <- event_grid %>%
    select(product_id, country, competitor_id, cx_event_time) %>%
    left_join(comp_changes %>% select(product_id, country, competitor_id, change_time),
              by = join_by(product_id, country, competitor_id, closest(cx_event_time > change_time))) %>%
    transmute(product_id, country, competitor_id, cx_event_time,
              lead_time_h = as.numeric(difftime(cx_event_time, change_time, units = "hours")))

  event_grid %>%
    left_join(first_after, by = c("product_id", "country", "competitor_id", "cx_event_time")) %>%
    left_join(last_before, by = c("product_id", "country", "competitor_id", "cx_event_time")) %>%
    left_join(cx_events,   by = c("product_id", "country", "cx_event_time")) %>%
    left_join(competitor_attributes, by = c("country", "competitor_id")) %>%
    mutate(
      # outcomes
      responded  = as.integer(!is.na(response_delay_h) & response_delay_h <= RESPONSE_WINDOW_H),
      led        = as.integer(!is.na(lead_time_h) & lead_time_h <= LEAD_WINDOW_H),
      same_direction = if_else(responded == 1, response_direction == cx_direction, NA),
      response_abs_change = if_else(responded == 1, abs(response_change_pct), NA_real_),
      # role (§3.2): leader if the competitor moved before Company X (this
      # includes competitors that also moved again afterwards), follower if it
      # only moved after, otherwise no action
      role3 = factor(case_when(led == 1 ~ "leader", responded == 1 ~ "follower", TRUE ~ "no_action"),
                     levels = c("no_action", "follower", "leader")),
      # price proximity at the event: competitor's pre-event price vs Company X's new price
      price_gap_pct = (comp_price_before - cx_price) / cx_price,
      abs_gap       = abs(price_gap_pct)
    ) %>%
    relocate(product_id, product_category, country, cx_event_time,
             competitor_id, competitor_name, responded, led, role3)
}

event_level <- build_event_level()

log_step(sprintf("01  Done: %s raw rows, %s distinct observations, %s events, %s event-competitor pairs",
                 comma(n_raw_rows), comma(nrow(df)), comma(nrow(cx_events)), comma(nrow(event_level))))
