# =========================================================
# COMPANY X — WEEK 3 (DATA WRANGLING + SUMMARY STATS + 5 SIMPLE ANALYSES)
# Built by extending your uploaded script while keeping your structure.
# =========================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  # For average marginal effects (used later)

})

# ---------------------------------------------------------
# 0) LOAD + TAG 4 DATASETS
# ---------------------------------------------------------
# ---------------------------------------------------------
# 0) PROJECT PATHS
# ---------------------------------------------------------
# Use an explicit data directory instead of setwd().
# Change this once, and the script will work anywhere.
DATA_DIR <- "/Users/sinemkircali/Desktop/master/case/data/competitor_price_data_2x2"

be_phones <- read.csv(file.path(DATA_DIR, "competitor_price_data_mobile_phones_be.csv"))
nl_phones <- read.csv(file.path(DATA_DIR, "competitor_price_data_mobile_phones_nl.csv"))
be_vacuum <- read.csv(file.path(DATA_DIR, "competitor_price_data_vacuum_cleaners_be.csv"))
nl_vacuum <- read.csv(file.path(DATA_DIR, "competitor_price_data_vacuum_cleaners_nl.csv"))

# =========================================================
# REPORT TABLES / CHECKS — PART 0 (RAW FILE CHECKS)
# =========================================================
raw_na_counts <- tibble(
  dataset = c("be_phones", "nl_phones", "be_vacuum", "nl_vacuum"),
  n_rows  = c(nrow(be_phones), nrow(nl_phones), nrow(be_vacuum), nrow(nl_vacuum)),
  n_cols  = c(ncol(be_phones), ncol(nl_phones), ncol(be_vacuum), ncol(nl_vacuum)),
  na_cells = c(sum(is.na(be_phones)), sum(is.na(nl_phones)), sum(is.na(be_vacuum)), sum(is.na(nl_vacuum)))
)
raw_na_counts

# ---- 1) Add dataset identifiers ----
be_phones <- be_phones %>% mutate(dataset = "phones_BE", product_category = "mobile_phones",   country = "BE")
nl_phones <- nl_phones %>% mutate(dataset = "phones_NL", product_category = "mobile_phones",   country = "NL")
be_vacuum <- be_vacuum %>% mutate(dataset = "vacuum_BE", product_category = "vacuum_cleaners", country = "BE")
nl_vacuum <- nl_vacuum %>% mutate(dataset = "vacuum_NL", product_category = "vacuum_cleaners", country = "NL")

# ---- 2) Combine datasets ----
df <- bind_rows(be_phones, nl_phones, be_vacuum, nl_vacuum)

# =========================================================
# STEP A — CLEAN IDENTIFICATION + TIME FEATURES
# =========================================================

df <- df %>%
  mutate(
    is_company_x = competitor_name %in% c("company_x.nl", "company_x.be"),
    
    # Parse time
    scrape_datetime = ymd_hms(scrape_datetime, quiet = TRUE),
    scrape_date = as_date(scrape_datetime),
    hour_of_day = hour(scrape_datetime),
    day_of_week = wday(scrape_datetime, label = TRUE, week_start = 1),
    is_weekend = day_of_week %in% c("Sat", "Sun")
  ) %>%
  arrange(product_id, country, scrape_datetime)

# =========================================================
# REPORT TABLES / CHECKS — PART 1 (DATA DESCRIPTION AFTER PARSING)
# =========================================================

dataset_overview <- df %>%
  group_by(dataset, country, product_category) %>%
  summarise(
    n_rows = n(),
    n_products = n_distinct(product_id),
    n_competitors = n_distinct(competitor_id),
    n_competitor_names = n_distinct(competitor_name),
    start = min(scrape_datetime, na.rm = TRUE),
    end   = max(scrape_datetime, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country, product_category)

dataset_overview

missingness <- df %>%
  summarise(across(everything(), ~ mean(is.na(.)) )) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
  arrange(desc(pct_missing))

missingness

var_types <- tibble(
  variable = names(df),
  class = map_chr(df, ~ paste(class(.x), collapse = ", "))
)
var_types

competitor_counts <- df %>%
  count(competitor_name, is_company_x, sort = TRUE) %>%
  arrange(desc(is_company_x), desc(n)) %>%
  slice_head(n = 20)

competitor_counts

counts_by_segment <- df %>%
  group_by(country, product_category) %>%
  summarise(
    n_rows = n(),
    n_products = n_distinct(product_id),
    n_competitors = n_distinct(competitor_id),
    .groups = "drop"
  ) %>%
  arrange(country, product_category)

counts_by_segment

# Competitive intensity proxy: competitors active per product-day
comp_intensity_product_day <- df %>%
  group_by(dataset, country, product_category, product_id, scrape_date) %>%
  summarise(
    competitors_active = n_distinct(competitor_id),
    .groups = "drop"
  )

comp_intensity_summary <- comp_intensity_product_day %>%
  group_by(dataset, country, product_category) %>%
  summarise(
    avg_competitors_active = mean(competitors_active, na.rm = TRUE),
    p25_competitors_active = quantile(competitors_active, 0.25, na.rm = TRUE),
    med_competitors_active = median(competitors_active, na.rm = TRUE),
    p75_competitors_active = quantile(competitors_active, 0.75, na.rm = TRUE),
    max_competitors_active = max(competitors_active, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country, product_category)

comp_intensity_summary

# =========================================================
# STEP B — PRICE CHANGE VARIABLES (needed for everything)
# =========================================================

df <- df %>%
  group_by(product_id, country, competitor_id) %>%
  arrange(scrape_datetime) %>%
  mutate(
    price_lag = lag(price),
    price_change_value = price - price_lag,
    price_change_pct = (price - price_lag) / price_lag,
    price_change_direction = case_when(
      price_change_value > 0 ~ "increase",
      price_change_value < 0 ~ "decrease",
      TRUE ~ "no_change"
    ),
    is_price_change = if_else(!is.na(price_lag) & price != price_lag, 1L, 0L)
  ) %>%
  ungroup()

# =========================================================
# REPORT TABLES / CHECKS — PART 2 (PRICE CHANGE / COMPANY X CHECKS)
# =========================================================

price_change_rate_by_segment <- df %>%
  group_by(country, product_category, is_company_x) %>%
  summarise(
    n_rows = n(),
    n_price_changes = sum(is_price_change == 1, na.rm = TRUE),
    share_price_change = mean(is_price_change == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(is_company_x), country, product_category)

price_change_rate_by_segment

# =========================================================
# STEP C — DEFINE COMPANY X PRICE CHANGE EVENTS (event backbone)
# =========================================================

company_x_events <- df %>%
  filter(is_company_x == TRUE, is_price_change == 1) %>%
  select(product_id, product_category, country,
         cb_event_time = scrape_datetime,
         cb_price_change_value = price_change_value,
         cb_price_change_pct = price_change_pct,
         cb_price_change_direction = price_change_direction,
         cb_price = price,
         cb_is_in_stock = is_in_stock,
         cb_promo = is_company_x_promotion,
         cb_delivery_cost = delivery_cost,
         cb_next_day = is_next_day_delivery,
         cb_price_age = price_age,
         cb_day_of_week = day_of_week,
         cb_is_weekend = is_weekend,
         cb_hour = hour_of_day)

company_x_rows_events_summary <- df %>%
  filter(is_company_x) %>%
  summarise(
    rows_cb = n(),
    cb_price_change_events = sum(is_price_change == 1, na.rm = TRUE),
    share_price_change = mean(is_price_change == 1, na.rm = TRUE),
    start = min(scrape_datetime, na.rm = TRUE),
    end   = max(scrape_datetime, na.rm = TRUE)
  )

company_x_rows_events_summary

# =========================================================
# STEP D — CONSTRUCT COMPETITOR RESPONSES (SQ2)
# =========================================================

# Response window definition (sensitivity choice)
response_window_hours <- 24  # you can later compare 24 vs 168 for robustness

responses <- df %>%
  filter(is_company_x == FALSE, is_price_change == 1) %>%
  inner_join(company_x_events, by = c("product_id", "country")) %>%
  mutate(
    response_delay_hours = as.numeric(difftime(scrape_datetime, cb_event_time, units = "hours"))
  ) %>%
  filter(response_delay_hours > 0, response_delay_hours <= response_window_hours) %>%
  mutate(
    responded = 1L,
    response_magnitude_abs = abs(price_change_value),
    response_magnitude_pct = abs(price_change_pct),
    same_direction = (price_change_direction == cb_price_change_direction),
    undercut_cb_after_event = (price < cb_price) # proxy: is competitor cheaper than CB in that response observation
  )

response_delay_distribution <- responses %>%
  summarise(
    n_responses = n(),
    min_delay = min(response_delay_hours, na.rm = TRUE),
    p25_delay = quantile(response_delay_hours, 0.25, na.rm = TRUE),
    med_delay = median(response_delay_hours, na.rm = TRUE),
    p75_delay = quantile(response_delay_hours, 0.75, na.rm = TRUE),
    max_delay = max(response_delay_hours, na.rm = TRUE)
  )

response_delay_distribution

response_summary <- responses %>%
  group_by(product_id, country, competitor_id, cb_event_time) %>%
  summarise(
    responded = 1L,
    min_response_delay = min(response_delay_hours, na.rm = TRUE),
    avg_response_magnitude_pct = mean(response_magnitude_pct, na.rm = TRUE),
    same_direction_any = any(same_direction, na.rm = TRUE),
    undercut_any = any(undercut_cb_after_event, na.rm = TRUE),
    .groups = "drop"
  )

# ---------------- FIX (A): bring competitor-level relevance into event_level ----------------
# Compute competitor-level relevance (and overlap) so we can use it in event-level analyses.
# We aggregate at competitor x country to keep joins small and interpretation clean.
competitor_attributes <- df %>%
  filter(is_company_x == FALSE) %>%
  group_by(country, competitor_id, competitor_name) %>%
  summarise(
    competitor_relevance_score = ifelse(all(is.na(competitor_relevance_score)),
                                        NA_real_,
                                        mean(competitor_relevance_score, na.rm = TRUE)),
    assortment_overlap = ifelse(all(is.na(assortment_overlap)),
                                NA_real_,
                                mean(assortment_overlap, na.rm = TRUE)),
    .groups = "drop"
  )

# Build full competitor list (with attributes)
competitors_only <- df %>%
  filter(is_company_x == FALSE) %>%
  distinct(country, competitor_id, competitor_name) %>%
  left_join(competitor_attributes, by = c("country", "competitor_id", "competitor_name"))

# Build full competitor-event grid so non-responses appear explicitly
event_competitor_grid <- company_x_events %>%
  select(product_id, country, cb_event_time) %>%
  distinct() %>%
  inner_join(competitors_only, by = "country")

event_level <- event_competitor_grid %>%
  left_join(response_summary, by = c("product_id", "country", "competitor_id", "cb_event_time")) %>%
  mutate(
    responded = ifelse(is.na(responded), 0L, responded)
  ) %>%
  left_join(company_x_events, by = c("product_id", "country", "cb_event_time"))

# =========================================================
# STEP E — LEADERSHIP / FOLLOWERSHIP (SQ1)
# =========================================================

lead_window_hours <- 24
follow_window_hours <- response_window_hours

leads <- df %>%
  filter(is_company_x == FALSE, is_price_change == 1) %>%
  inner_join(company_x_events, by = c("product_id", "country")) %>%
  mutate(
    lead_time_hours = as.numeric(difftime(cb_event_time, scrape_datetime, units = "hours"))
  ) %>%
  filter(lead_time_hours > 0, lead_time_hours <= lead_window_hours) %>%
  group_by(product_id, country, competitor_id, cb_event_time) %>%
  summarise(
    led = 1L,
    min_lead_time = min(lead_time_hours, na.rm = TRUE),
    .groups = "drop"
  )

event_level <- event_level %>%
  left_join(leads, by = c("product_id", "country", "competitor_id", "cb_event_time")) %>%
  mutate(
    led = ifelse(is.na(led), 0L, led)
  )

event_level <- event_level %>%
  mutate(
    role_event = case_when(
      led == 1 & responded == 0 ~ "leader_only",
      led == 0 & responded == 1 ~ "follower_only",
      led == 1 & responded == 1 ~ "both_sides",
      TRUE ~ "no_action"
    )
  )

competitor_role_summary <- event_level %>%
  group_by(country, competitor_id, competitor_name) %>%
  summarise(
    leader_share = mean(led == 1, na.rm = TRUE),
    follower_share = mean(responded == 1, na.rm = TRUE),
    both_share = mean(role_event == "both_sides", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country, desc(follower_share))

competitor_role_summary

# =========================================================
# STEP F — PERSISTENCE & BEHAVIORAL INTENSITY (time/persistence context)
# =========================================================

df <- df %>%
  mutate(scrape_day = as_date(scrape_datetime)) %>%
  group_by(product_id, country, competitor_id, scrape_day) %>%
  mutate(changes_that_day = sum(is_price_change == 1, na.rm = TRUE)) %>%
  ungroup()

df <- df %>%
  mutate(
    price_age_tertile = ntile(price_age, 3),
    persistence_level = case_when(
      price_age_tertile == 1 ~ "low_persistence",
      price_age_tertile == 2 ~ "medium_persistence",
      price_age_tertile == 3 ~ "high_persistence"
    )
  )

# =========================================================
# STEP G — WEEK 3 REQUIRED OUTPUTS
#  (1) CURATED SUMMARY STATISTICS (<= 10 key constructs)
#  (2) 5 SIMPLE ANALYSES (tables + plots)
# =========================================================

# ---------------------------------------------------------
# G1) Curated Summary Statistics (safe for all-NA variables)
# ---------------------------------------------------------

# ---------------------------------------------------------
# FIX: Convert binary raw columns safely (prevents all-NA summaries)
# Put this BEFORE you build df_constructs
# ---------------------------------------------------------

to_binary01 <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  case_when(
    is.na(x_chr) ~ NA_integer_,
    x_chr %in% c("1", "true", "yes", "y") ~ 1L,
    x_chr %in% c("0", "false", "no", "n") ~ 0L,
    TRUE ~ NA_integer_
  )
}

df <- df %>%
  mutate(
    is_in_stock = to_binary01(is_in_stock),
    is_next_day_delivery = to_binary01(is_next_day_delivery)
  )

df_constructs <- df %>%
  transmute(
    dataset, country, product_category,
    is_company_x,
    price,
    price_change_pct,
    price_age,
    is_in_stock,
    is_next_day_delivery,
    is_company_x_promotion = as.integer(is_company_x_promotion),
    competitor_relevance_score,
    assortment_overlap,
    search_impression_share,
    shopping_impression_share
  )

construct_vars <- c(
  "price",
  "price_change_pct",
  "price_age",
  "is_in_stock",
  "is_next_day_delivery",
  "is_company_x_promotion",
  "competitor_relevance_score",
  "assortment_overlap"
)

# Helper: safe summary that returns NA when a variable has 0 non-missing values
safe_summary <- function(x) {
  n_non_missing <- sum(!is.na(x))
  if (n_non_missing == 0) {
    return(tibble(
      n_non_missing = 0L,
      mean = NA_real_, sd = NA_real_,
      p25 = NA_real_, median = NA_real_, p75 = NA_real_,
      min = NA_real_, max = NA_real_
    ))
  } else {
    return(tibble(
      n_non_missing = n_non_missing,
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      p25 = quantile(x, 0.25, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      p75 = quantile(x, 0.75, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    ))
  }
}

# =========================================================
# UPDATED SUMMARY STATS TABLE (up to 10 constructs, including event-based + intensity)
# =========================================================

# 0) Clean price_age (-1 is a sentinel for "unknown" persistence)
df <- df %>%
  mutate(price_age = na_if(price_age, -1))

# 1) Competitive intensity vector (competitors active per product-day)
# (you already created comp_intensity_product_day earlier)
competitors_active_vec <- comp_intensity_product_day$competitors_active

# 2) Event-level scalar (share_same_direction)
# (you already created event_level_summary earlier)
share_same_direction_value <- event_level_summary$share_same_direction[1]

# 3) Build ONE LONG construct table where each construct has a "value" vector
construct_long <- bind_rows(
  tibble(construct = "price_age", value = df$price_age),
  tibble(construct = "price_change_pct", value = df$price_change_pct),
  
  # response_delay_hours is response-based (exists only in responses dataset)
  tibble(construct = "response_delay_hours", value = responses$response_delay_hours),
  
  # share_same_direction is event-based scalar (single informative value)
  tibble(construct = "share_same_direction", value = share_same_direction_value),
  
  # competitors active is product-day based
  tibble(construct = "competitors_active", value = competitors_active_vec),
  
  # competitive relevance / overlap (raw constructs)
  tibble(construct = "assortment_overlap", value = df$assortment_overlap),
  tibble(construct = "competitor_relevance_score", value = df$competitor_relevance_score),
  
  # context moderators (binary)
  tibble(construct = "is_in_stock", value = df$is_in_stock),
  tibble(construct = "is_company_x_promotion", value = as.integer(df$is_company_x_promotion)),
  tibble(construct = "is_next_day_delivery", value = df$is_next_day_delivery)
)

# 4) Summary statistics table (same stats as your original code)
summary_stats_constructs <- construct_long %>%
  group_by(construct) %>%
  summarise(
    n_non_missing = sum(!is.na(value)),
    mean   = mean(value, na.rm = TRUE),
    sd     = sd(value, na.rm = TRUE),
    p25    = quantile(value, 0.25, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    p75    = quantile(value, 0.75, na.rm = TRUE),
    min    = min(value, na.rm = TRUE),
    max    = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(construct)

summary_stats_constructs

response_delay_summary <- responses %>%
  summarise(
    n_responses = n(),
    mean_delay_hours = mean(response_delay_hours, na.rm = TRUE),
    p25_delay_hours = quantile(response_delay_hours, 0.25, na.rm = TRUE),
    median_delay_hours = median(response_delay_hours, na.rm = TRUE),
    p75_delay_hours = quantile(response_delay_hours, 0.75, na.rm = TRUE),
    max_delay_hours = max(response_delay_hours, na.rm = TRUE)
  )

response_delay_summary

# ---------------------------------------------------------
# G2) SIMPLE ANALYSIS 0 — Event-level response summary (overall + segment)
# ---------------------------------------------------------
event_level_summary <- event_level %>%
  summarise(
    n_events = n_distinct(cb_event_time),
    n_event_competitor_pairs = n(),
    response_rate = mean(responded, na.rm = TRUE),
    avg_delay_hours = mean(min_response_delay, na.rm = TRUE),
    median_delay_hours = median(min_response_delay, na.rm = TRUE),
    avg_response_magnitude_pct = mean(avg_response_magnitude_pct, na.rm = TRUE),
    share_same_direction = mean(same_direction_any, na.rm = TRUE),
    .groups = "drop"
  )

event_level_summary

event_level_by_segment <- event_level %>%
  group_by(country, product_category) %>%
  summarise(
    response_rate = mean(responded, na.rm = TRUE),
    avg_delay_hours = mean(min_response_delay, na.rm = TRUE),
    median_delay_hours = median(min_response_delay, na.rm = TRUE),
    avg_response_magnitude_pct = mean(avg_response_magnitude_pct, na.rm = TRUE),
    share_same_direction = mean(same_direction_any, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country, product_category)

event_level_by_segment

# ---------------------------------------------------------
# G3) SIMPLE ANALYSIS 1 — Response probability by competitor relevance
# ---------------------------------------------------------

# FIX (B): competitor_relevance_score now exists in event_level via competitor_attributes join.
# To avoid ntile() issues, we drop rows with missing relevance for this specific analysis.
event_level_with_relevance <- event_level %>%
  filter(!is.na(competitor_relevance_score))

event_level_with_relevance <- event_level_with_relevance %>%
  group_by(country) %>%
  mutate(relevance_q = ntile(competitor_relevance_score, 4)) %>%
  ungroup()

sa1_response_by_relevance <- event_level_with_relevance %>%
  group_by(country, product_category, relevance_q) %>%
  summarise(
    n_pairs = n(),
    response_rate = mean(responded, na.rm = TRUE),
    median_delay = median(min_response_delay, na.rm = TRUE),
    .groups = "drop"
  )

sa1_response_by_relevance

ggplot(sa1_response_by_relevance, aes(x = factor(relevance_q), y = response_rate)) +
  geom_point(size = 2) +
  geom_line(aes(group = interaction(country, product_category))) +
  facet_grid(country ~ product_category) +
  labs(
    title = "Simple Analysis 1: Response rate by competitor relevance quartile",
    x = "Competitor relevance (quartiles within country)",
    y = "Response rate (within response window)"
  )

# ---------------------------------------------------------
# G4) SIMPLE ANALYSIS 2 — Reaction speed differs by category/country
# ---------------------------------------------------------

sa2_delay_dist <- event_level %>%
  filter(responded == 1) %>%   # only those with a response
  group_by(country, product_category) %>%
  summarise(
    n_responses = n(),
    p25 = quantile(min_response_delay, 0.25, na.rm = TRUE),
    median = median(min_response_delay, na.rm = TRUE),
    p75 = quantile(min_response_delay, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country, product_category)
ggplot(event_level %>% filter(responded == 1), aes(x = min_response_delay)) +
  geom_histogram(bins = 40) +
  facet_grid(country ~ product_category) +
  labs(
    title = "Simple Analysis 2: Distribution of response delays (hours)",
    x = "Minimum response delay (hours)",
    y = "Count"
  )
# ---------------------------------------------------------
# G5) SIMPLE ANALYSIS 3 — Stock availability and response likelihood
# (conditional means + plot)
# ---------------------------------------------------------

sa3_stock_response <- event_level %>%
  group_by(country, product_category, cb_is_in_stock) %>%
  summarise(
    n_pairs = n(),
    response_rate = mean(responded, na.rm = TRUE),
    median_delay = median(min_response_delay, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(cb_is_in_stock = ifelse(cb_is_in_stock, "CB in stock", "CB out of stock"))

sa3_stock_response

ggplot(sa3_stock_response, aes(x = cb_is_in_stock, y = response_rate)) +
  geom_col() +
  facet_grid(country ~ product_category) +
  labs(
    title = "Simple Analysis 3: Response rate by Company X stock status at event time",
    x = "",
    y = "Response rate"
  )

# ---------------------------------------------------------
# G6) SIMPLE ANALYSIS 4 — “Match vs Oppose” direction behavior
# (same-direction share + plot)
# ---------------------------------------------------------

sa4_direction <- event_level %>%
  filter(responded == 1) %>%
  group_by(country, product_category) %>%
  summarise(
    n_responses = n(),
    share_same_direction = mean(same_direction_any, na.rm = TRUE),
    .groups = "drop"
  )

sa4_direction

ggplot(sa4_direction, aes(x = product_category, y = share_same_direction)) +
  geom_col() +
  facet_wrap(~ country) +
  labs(
    title = "Simple Analysis 4: Share of responses in the same direction as Company X",
    x = "Product category",
    y = "Share same-direction"
  )

# ---------------------------------------------------------
# G7) SIMPLE ANALYSIS 5 — Leader vs follower asymmetry by competitor
# (role shares + scatter plot)
# ---------------------------------------------------------

sa5_roles <- competitor_role_summary %>%
  mutate(country = as.factor(country))

sa5_roles

ggplot(sa5_roles, aes(x = leader_share, y = follower_share)) +
  geom_point(alpha = 0.8) +
  facet_wrap(~ country) +
  labs(
    title = "Simple Analysis 5: Competitor role asymmetry (leader vs follower shares)",
    x = "Leader share (moved before Company X within lead window)",
    y = "Follower share (responded after Company X within response window)"
  )

# Optional: list “top followers” and “top leaders” per country (easy to paste in report)
top_followers <- sa5_roles %>%
  group_by(country) %>%
  slice_max(order_by = follower_share, n = 10, with_ties = FALSE) %>%
  ungroup()

top_leaders <- sa5_roles %>%
  group_by(country) %>%
  slice_max(order_by = leader_share, n = 10, with_ties = FALSE) %>%
  ungroup()

top_followers
top_leaders

# ---------------------------------------------------------
# OPTIONAL: Quick robustness check (24h vs 168h) for SA1 response rate
# (This is for narrative support; not required but great to mention.)
# ---------------------------------------------------------

response_window_hours_alt <- 168

responses_alt <- df %>%
  filter(is_company_x == FALSE, is_price_change == 1) %>%
  inner_join(
    company_x_events %>%
      select(product_id, country, product_category, cb_event_time),
    by = c("product_id", "country")
  ) %>%
  mutate(
    response_delay_hours = as.numeric(difftime(scrape_datetime, cb_event_time, units = "hours"))
  ) %>%
  filter(response_delay_hours > 0, response_delay_hours <= response_window_hours_alt)

response_summary_alt <- responses_alt %>%
  group_by(product_id, country, competitor_id, cb_event_time) %>%
  summarise(
    responded_alt = 1L,
    .groups = "drop"
  )

event_level_alt <- event_level %>%
  select(product_id, country, competitor_id, cb_event_time, product_category) %>%
  distinct() %>%
  left_join(
    response_summary_alt,
    by = c("product_id", "country", "competitor_id", "cb_event_time")
  ) %>%
  mutate(
    responded_alt = ifelse(is.na(responded_alt), 0L, responded_alt)
  )
response_rate_alt_by_segment <- event_level_alt %>%
  group_by(country, product_category) %>%
  summarise(
    response_rate_168h = mean(responded_alt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country, product_category)

response_rate_alt_by_segment

# =========================================================
# DONE — You now have:
# - data description tables (Week 3)
# - operationalization via event construction (Week 3)
# - curated summary stats (<=10 key constructs) (Week 3)
# - 5 simple analyses with tables + plots (Week 3)
# =========================================================

#Regressions
#REGRESSION 1 — When do price changes occur?
#cool blue 
m_when_cb <- lm(
  is_price_change ~ 
    hour_of_day +
    is_weekend +
    is_company_x_promotion +
    product_category +
    country,
  data = df %>% filter(is_company_x == TRUE)
)

summary(m_when_cb)

#competitor 

m_when_comp <- lm(
  is_price_change ~ 
    hour_of_day +
    is_weekend +
    product_category + competitor_relevance_score + 
    country,
  data = df %>% filter(is_company_x == FALSE)
)

summary(m_when_comp)

#REGRESSION 2 — Who changes prices most?
m_freq <- lm(
  is_price_change ~ 
    factor(competitor_id) +
    product_category + factor(competitor_id) * competitor_relevance_score +
    country,
  data = df %>% filter(is_company_x == FALSE)
)

summary(m_freq)

#irrelevant from the regressions, this is descriptive analysis 

aggressiveness_by_competitor_product <- df %>%
  filter(is_company_x == FALSE) %>%
  group_by(competitor_name, country, product_category) %>%
  summarise(
    price_change_rate = mean(is_price_change),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(price_change_rate))

#REGRESSION 3 — Who makes the biggest price changes?
m_magnitude <- lm(
  abs(price_change_pct) ~
    factor(competitor_id) +
    is_company_x_promotion +
    product_category +
    country,
  data = df %>% 
    filter(is_price_change == 1, is_company_x == FALSE)
)

summary(m_magnitude)

# ============================================================
# Regression: price change magnitude (abs pct) — run separately
# by product category (competitors only, only when they changed)
# ============================================================

# Make sure factors are set (optional but recommended)
df2 <- df %>%
  mutate(
    product_category = factor(product_category),
    country = factor(country),
    competitor_id = factor(competitor_id)
  )

# --- 1) Mobile phones only ---
reg_mag_phones <- lm(
  abs(price_change_pct) ~ competitor_id + is_company_x_promotion + country,
  data = df2 %>% filter(is_price_change == 1, is_company_x == FALSE, product_category == "mobile_phones")
)

summary(reg_mag_phones)

# --- 2) Vacuum cleaners only ---
reg_mag_vacuums <- lm(
  abs(price_change_pct) ~ competitor_id + is_company_x_promotion + country,
  data = df2 %>% filter(is_price_change == 1, is_company_x == FALSE, product_category == "vacuum_cleaners")
)

summary(reg_mag_vacuums)

# ============================================================
# OPTIONAL: Turn coefficients into a clean "ranking" table
# (Average magnitude implied by intercept + competitor effect)
# ============================================================

coef_to_rank <- function(model, category_label) {
  co <- coef(model)
  intercept <- unname(co["(Intercept)"])
  
  tibble(term = names(co), estimate = unname(co)) %>%
    filter(grepl("^competitor_id", term)) %>%
    mutate(
      competitor_id = gsub("^competitor_id", "", term),
      implied_abs_change_pct = intercept + estimate,     # baseline + competitor FE
      product_category = category_label
    ) %>%
    select(product_category, competitor_id, estimate, implied_abs_change_pct) %>%
    arrange(desc(implied_abs_change_pct))
}

rank_phones  <- coef_to_rank(reg_mag_phones,  "mobile_phones")
rank_vacuums <- coef_to_rank(reg_mag_vacuums, "vacuum_cleaners")

rank_phones
rank_vacuums

# If you want competitor names in the ranking table:
# (works if df has competitor_name)
id_to_name <- df2 %>%
  filter(is_company_x == FALSE) %>%
  distinct(competitor_id, competitor_name)

rank_phones_named <- rank_phones %>% left_join(id_to_name, by = "competitor_id")
rank_vacuums_named <- rank_vacuums %>% left_join(id_to_name, by = "competitor_id")

rank_phones_named
rank_vacuums_named

#🔹 REGRESSION 4 — Who is fastest?
responses_with_cat <- responses %>%
  left_join(
    df %>%
      select(product_id, product_category) %>%
      distinct(),
    by = "product_id"
  )
m_speed_log <- lm(
  log(response_delay_hours + 0.01) ~ factor(competitor_id) + product_category + country,
  data = responses_with_cat
)

library(broom)

speed_table <- tidy(m_speed_log) %>%
  filter(grepl("factor\\(competitor_id\\)", term)) %>%
  mutate(
    competitor_id = gsub("factor\\(competitor_id\\)", "", term),
    implied_multiplier = exp(estimate),
    implied_pct_faster_slower = (implied_multiplier - 1) * 100
  ) %>%
  arrange(implied_multiplier)

speed_table

library(lme4)

m_speed_mixed <- lmer(
  log(response_delay_hours + 0.01) ~
    product_category + country +
    (1 | competitor_id),
  data = responses_with_cat
)

summary(m_speed_mixed)

library(broom.mixed)
library(dplyr)

speed_re <- ranef(m_speed_mixed)$competitor_id %>%
  as.data.frame() %>%
  rownames_to_column("competitor_id") %>%
  rename(speed_effect = `(Intercept)`) %>%
  mutate(
    implied_multiplier = exp(speed_effect),
    implied_pct_faster_slower = (implied_multiplier - 1) * 100
  ) %>%
  arrange(implied_pct_faster_slower)

summary(speed_re)

library(lme4)
library(tibble)

# --- Extract random effects ---
speed_table <- ranef(m_speed_mixed)$competitor_id %>%
  as.data.frame() %>%
  rownames_to_column("competitor_id") %>%
  rename(speed_effect_log = `(Intercept)`) %>%
  mutate(
    competitor_id = as.character(competitor_id),   # 🔹 FIX
    implied_multiplier = exp(speed_effect_log),
    implied_pct_faster_slower = (implied_multiplier - 1) * 100
  )

# --- Competitor name lookup (also force character) ---
comp_map <- responses_with_cat %>%
  select(competitor_id, competitor_name) %>%
  distinct() %>%
  mutate(competitor_id = as.character(competitor_id))  # 🔹 FIX

# --- Join + rank ---
speed_table_final <- speed_table %>%
  left_join(comp_map, by = "competitor_id") %>%
  arrange(implied_pct_faster_slower) %>%
  mutate(
    rank_speed = row_number(),
    speed_label = case_when(
      implied_pct_faster_slower < 0 ~ paste0(round(abs(implied_pct_faster_slower), 1), "% faster"),
      TRUE ~ paste0(round(implied_pct_faster_slower, 1), "% slower")
    )
  ) %>%
  select(
    rank_speed,
    competitor_name,
    implied_pct_faster_slower,
    speed_label
  )

# View full ranking
speed_table_final

top5_fastest <- speed_table_final %>% slice_head(n = 5)
top5_slowest <- speed_table_final %>% slice_tail(n = 5)

top5_fastest
top5_slowest

#regression 6 
event_level <- event_level %>%
  mutate(
    relevance_c = competitor_relevance_score - mean(competitor_relevance_score, na.rm = TRUE)
  )

m_market_logit <- glm(
  responded ~
    relevance_c *
    product_category *
    country,
  data = event_level,
  family = binomial(link = "logit")
)

summary(m_market_logit)

#average marginal effects 
# NOTE: Package installation is intentionally NOT run inside the script.
# If you do not have marginaleffects installed, run this once in the console:
# install.packages("marginaleffects")

# =========================================================
# OPTIONAL: AVERAGE MARGINAL EFFECTS (AME)
# =========================================================

ame_market <- avg_slopes(
  m_market_logit,
  variables = "relevance_c",
  by = c("product_category", "country")
)

ame_market

#🔹 FINAL EXECUTIVE SCORECARD (non-regression, but essential)
competitor_scorecard <- df %>%
  filter(is_company_x == FALSE) %>%
  group_by(competitor_name, country) %>%
  summarise(
    price_change_rate = mean(is_price_change),
    avg_magnitude_pct = mean(abs(price_change_pct), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    responses %>%
      group_by(competitor_name, country) %>%
      summarise(
        median_delay = median(response_delay_hours),
        .groups = "drop"
      ),
    by = c("competitor_name", "country")
  ) %>%
  arrange(country, median_delay)

#regression 5
m_follow_ext <- lm(
  responded ~
    competitor_relevance_score +
    assortment_overlap +
    product_category +
    country,
  data = event_level
)

summary(m_follow_ext)

#Final results 
# =========================================================
# WEEK 4 — VISUAL DIAGNOSTICS (PLOTS)
# Add this section at the END of your current script.
# =========================================================

library(scales)

# ---------------------------------------------------------
# 0) SAFETY: ensure datetime + identifiers exist
# ---------------------------------------------------------
# (Your script already does this, but keeping it safe)
df <- df %>%
  mutate(scrape_datetime = ymd_hms(scrape_datetime, quiet = TRUE))

# ---------------------------------------------------------
# 1) PRODUCT SELECTION RULES (so we don’t pick randomly)
# ---------------------------------------------------------
# Goal: pick products that are (i) well observed, (ii) have many CB events,
#       (iii) have many competitors, and (iv) have many responses (optional).
#
# We create a "plot_priority_score" for each product-market:
#   score = (CB_events) + 0.5*(avg_competitors_active_per_day) + 0.5*(responses_to_CB_events)
# Then pick top N.

# 1A) Competitors active per product-day (your earlier metric)
comp_intensity_product_day <- df %>%
  mutate(scrape_date = as_date(scrape_datetime)) %>%
  group_by(country, product_category, product_id, scrape_date) %>%
  summarise(competitors_active = n_distinct(competitor_id), .groups = "drop")

comp_intensity_product <- comp_intensity_product_day %>%
  group_by(country, product_category, product_id) %>%
  summarise(
    avg_competitors_active = mean(competitors_active, na.rm = TRUE),
    .groups = "drop"
  )

# 1B) Company X event count per product-market
cb_event_counts <- company_x_events %>%
  group_by(country, product_category, product_id) %>%
  summarise(
    cb_events = n(),
    .groups = "drop"
  )

# 1C) Response count per product-market (how many competitor reactions we observe in `responses`)
# NOTE: this counts response observations, not unique competitor-event pairs
response_counts <- responses %>%
  left_join(
    company_x_events %>% distinct(product_id, country, product_category, cb_event_time),
    by = c("product_id", "country", "cb_event_time")
  ) %>%
  group_by(country, product_category, product_id) %>%
  summarise(
    response_obs = n(),
    .groups = "drop"
  )

# 1D) Build the selection table
product_selection_table <- comp_intensity_product %>%
  left_join(cb_event_counts, by = c("country", "product_category", "product_id")) %>%
  left_join(response_counts, by = c("country", "product_category", "product_id")) %>%
  mutate(
    cb_events = replace_na(cb_events, 0),
    response_obs = replace_na(response_obs, 0),
    plot_priority_score = cb_events + 0.5 * avg_competitors_active + 0.5 * log1p(response_obs)
  ) %>%
  arrange(desc(plot_priority_score))

# Helper: pick top N products per (country, category)
pick_products <- function(country_sel, category_sel, n = 2) {
  product_selection_table %>%
    filter(country == country_sel, product_category == category_sel) %>%
    slice_head(n = n) %>%
    pull(product_id)
}

# You can inspect the selection table (good to paste in report appendix)
product_selection_table %>%
  group_by(country, product_category) %>%
  slice_head(n = 5) %>%
  ungroup()

# ---------------------------------------------------------
# 2) PLOT TYPE 1 — CORE PRICE TRAJECTORIES (must-have)
# ---------------------------------------------------------
# For each selected product:
# x = scrape_datetime, y = price, one line per competitor
# Company X highlighted

# Settings
OUT_DIR <- "week4_plots"
dir.create(OUT_DIR, showWarnings = FALSE)

# function to keep plots readable (limit to top competitors by #obs)
top_competitors_for_product <- function(df_sub, top_n = 8) {
  df_sub %>%
    count(competitor_name, sort = TRUE) %>%
    slice_head(n = top_n) %>%
    pull(competitor_name)
}

plot_core_trajectory <- function(country_sel, category_sel, product_id_sel, top_n_comp = 8) {
  
  df_sub <- df %>%
    filter(country == country_sel,
           product_category == category_sel,
           product_id == product_id_sel) %>%
    mutate(
      competitor_label = ifelse(is_company_x, "Company X", competitor_name)
    )
  
  # limit competitor lines to avoid spaghetti
  keep_names <- top_competitors_for_product(df_sub %>% filter(!is_company_x), top_n = top_n_comp)
  
  df_plot <- df_sub %>%
    filter(is_company_x | competitor_name %in% keep_names)
  
  p <- ggplot(df_plot, aes(x = scrape_datetime, y = price, group = competitor_label)) +
    geom_line(aes(color = competitor_label, size = competitor_label == "Company X"), alpha = 0.9) +
    scale_size_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.4), guide = "none") +
    labs(
      title = paste0("Core price trajectories | ", country_sel, " | ", category_sel, " | product_id=", product_id_sel),
      x = "Time (scrape_datetime)",
      y = "Price",
      color = "Competitor"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
  
  p
}

# Produce exactly 4 plots: 2 products per category but 1 country at a time is your rule.
# Here: we do 2 products per category for each country (BE and NL) -> you can choose to keep only 4 total if you want.
# If you truly want "total 4 plots", set one country only (e.g., BE) and run the loop for that country.

countries_to_plot <- c("BE", "NL")
categories_to_plot <- c("mobile_phones", "vacuum_cleaners")

for (cc in countries_to_plot) {
  for (cat in categories_to_plot) {
    prod_ids <- pick_products(cc, cat, n = 2)
    
    for (pid in prod_ids) {
      p <- plot_core_trajectory(cc, cat, pid, top_n_comp = 8)
      ggsave(
        filename = file.path(OUT_DIR, paste0("P1_core_", cc, "_", cat, "_pid", pid, ".png")),
        plot = p,
        width = 12, height = 6, dpi = 200
      )
      print(p)
    }
  }
}

# What you say in report:
# "These plots show asymmetric reactions: some firms track Company X closely, others move in steps or remain rigid."

# ---------------------------------------------------------
# 3) PLOT TYPE 2 — EVENT-ALIGNED REACTION PLOTS (critical)
# ---------------------------------------------------------
# Center time around a Company X price change event.
# Window: -24h to +72h
# Show Company X + top 4–5 competitors (by response activity for that product)

# Helper: pick one "representative" event for a product: use median event time
pick_representative_event <- function(country_sel, category_sel, product_id_sel) {
  company_x_events %>%
    filter(country == country_sel,
           product_category == category_sel,
           product_id == product_id_sel) %>%
    arrange(cb_event_time) %>%
    slice(round(n()/2)) %>%
    pull(cb_event_time)
}

# Helper: top responders for that product (counts in responses)
top_responders_for_product <- function(country_sel, product_id_sel, top_n = 5) {
  responses %>%
    filter(country == country_sel, product_id == product_id_sel) %>%
    count(competitor_name, sort = TRUE) %>%
    slice_head(n = top_n) %>%
    pull(competitor_name)
}

plot_event_aligned <- function(country_sel, category_sel, product_id_sel,
                               cb_event_time_sel = NULL,
                               pre_hours = 24, post_hours = 72, top_n_comp = 5) {
  
  if (is.null(cb_event_time_sel)) {
    cb_event_time_sel <- pick_representative_event(country_sel, category_sel, product_id_sel)
  }
  
  # competitor subset = most active responders around that product
  keep_names <- top_responders_for_product(country_sel, product_id_sel, top_n = top_n_comp)
  
  df_window <- df %>%
    filter(country == country_sel,
           product_category == category_sel,
           product_id == product_id_sel) %>%
    filter(scrape_datetime >= cb_event_time_sel - hours(pre_hours),
           scrape_datetime <= cb_event_time_sel + hours(post_hours)) %>%
    mutate(
      competitor_label = ifelse(is_company_x, "Company X", competitor_name),
      t_rel_hours = as.numeric(difftime(scrape_datetime, cb_event_time_sel, units = "hours"))
    ) %>%
    filter(is_company_x | competitor_name %in% keep_names)
  
  p <- ggplot(df_window, aes(x = t_rel_hours, y = price, group = competitor_label)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_line(aes(color = competitor_label, size = competitor_label == "Company X"), alpha = 0.9) +
    scale_size_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.5), guide = "none") +
    labs(
      title = paste0("Event-aligned reactions (t=0 is Company X change) | ", country_sel, " | ", category_sel,
                     " | pid=", product_id_sel),
      subtitle = paste0("Window: -", pre_hours, "h to +", post_hours, "h around cb_event_time = ", cb_event_time_sel),
      x = "Hours relative to Company X price change (t=0)",
      y = "Price",
      color = "Competitor"
    ) +
    theme_minimal()
  
  p
}

# Make 2 plots total (1 phone + 1 vacuum) — pick the top-priority product in each category for one chosen country.
# If you want both countries, run twice.
country_for_eventplots <- "NL"

pid_phone  <- pick_products(country_for_eventplots, "mobile_phones", n = 1)[1]
pid_vacuum <- pick_products(country_for_eventplots, "vacuum_cleaners", n = 1)[1]

p2_phone <- plot_event_aligned(country_for_eventplots, "mobile_phones", pid_phone,
                               pre_hours = 24, post_hours = 72, top_n_comp = 5)
ggsave(file.path(OUT_DIR, paste0("P2_eventaligned_", country_for_eventplots, "_phones_pid", pid_phone, ".png")),
       p2_phone, width = 12, height = 6, dpi = 200)
print(p2_phone)

p2_vac <- plot_event_aligned(country_for_eventplots, "vacuum_cleaners", pid_vacuum,
                             pre_hours = 24, post_hours = 72, top_n_comp = 5)
ggsave(file.path(OUT_DIR, paste0("P2_eventaligned_", country_for_eventplots, "_vacuum_pid", pid_vacuum, ".png")),
       p2_vac, width = 12, height = 6, dpi = 200)
print(p2_vac)

# What you say:
# "Reactions occur with heterogeneous delays; fixed windows miss slower responders and motivate timing models."

# ---------------------------------------------------------
# 4) PLOT TYPE 3 — COMPETITOR “STYLE” COMPARISON (case studies)
# ---------------------------------------------------------
# Pick 4 archetypes from data-driven metrics:
# - fast follower: low median response delay + high follower share
# - slow strategic: high median response delay + non-zero follower share
# - aggressive jumper: high mean abs(price_change_pct) conditional on change
# - rigid: low change rate + high avg price_age

# 4A) Build competitor-level metrics (competitors only)
comp_change_rate <- df %>%
  filter(!is_company_x) %>%
  group_by(country, product_category, competitor_name) %>%
  summarise(
    change_rate = mean(is_price_change == 1, na.rm = TRUE),
    avg_price_age = mean(price_age, na.rm = TRUE),
    .groups = "drop"
  )

comp_jump_size <- df %>%
  filter(!is_company_x, is_price_change == 1) %>%
  group_by(country, product_category, competitor_name) %>%
  summarise(
    mean_abs_change_pct = mean(abs(price_change_pct), na.rm = TRUE),
    .groups = "drop"
  )

# Ensure responses has product_category for plotting/metrics
responses_with_cat <- responses %>%
  left_join(
    df %>% distinct(product_id, country, product_category),
    by = c("product_id", "country")
  )

comp_speed <- responses_with_cat %>%
  group_by(country, product_category, competitor_name) %>%
  summarise(
    median_delay = median(response_delay_hours, na.rm = TRUE),
    .groups = "drop"
  )

# follower share from event_level (your event grid)
comp_follow <- event_level %>%
  group_by(country, product_category, competitor_name) %>%
  summarise(
    follower_share = mean(responded == 1, na.rm = TRUE),
    .groups = "drop"
  )

comp_metrics <- comp_change_rate %>%
  left_join(comp_jump_size, by = c("country", "product_category", "competitor_name")) %>%
  left_join(comp_speed, by = c("country", "product_category", "competitor_name")) %>%
  left_join(comp_follow, by = c("country", "product_category", "competitor_name")) %>%
  mutate(
    mean_abs_change_pct = replace_na(mean_abs_change_pct, 0),
    median_delay = replace_na(median_delay, NA_real_),
    follower_share = replace_na(follower_share, 0)
  )

# 4B) Choose 4 competitors for ONE market slice (pick the slice you want to tell the story with)
slice_country <- "NL"
slice_category <- "mobile_phones"

cm <- comp_metrics %>%
  filter(country == slice_country, product_category == slice_category)

fast_follower <- cm %>%
  filter(follower_share >= quantile(follower_share, 0.75, na.rm = TRUE)) %>%
  arrange(median_delay) %>%
  slice_head(n = 1) %>%
  pull(competitor_name)

slow_strategic <- cm %>%
  filter(follower_share > 0) %>%
  arrange(desc(median_delay)) %>%
  slice_head(n = 1) %>%
  pull(competitor_name)

aggressive_jumper <- cm %>%
  arrange(desc(mean_abs_change_pct)) %>%
  slice_head(n = 1) %>%
  pull(competitor_name)

rigid_player <- cm %>%
  arrange(change_rate, desc(avg_price_age)) %>%
  slice_head(n = 1) %>%
  pull(competitor_name)

archetype_competitors <- unique(c(fast_follower, slow_strategic, aggressive_jumper, rigid_player))
archetype_competitors

# 4C) For these competitors, show price paths vs Company X for ONE representative product
pid_case <- pick_products(slice_country, slice_category, n = 1)[1]

case_df <- df %>%
  filter(country == slice_country,
         product_category == slice_category,
         product_id == pid_case) %>%
  mutate(competitor_label = ifelse(is_company_x, "Company X", competitor_name)) %>%
  filter(is_company_x | competitor_name %in% archetype_competitors)

p3_case <- ggplot(case_df, aes(x = scrape_datetime, y = price, group = competitor_label)) +
  geom_line(aes(color = competitor_label, size = competitor_label == "Company X"), alpha = 0.9) +
  scale_size_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.6), guide = "none") +
  facet_wrap(~ competitor_label, scales = "free_y") +
  labs(
    title = paste0("Competitor style case studies | ", slice_country, " | ", slice_category, " | pid=", pid_case),
    subtitle = "Each panel: Company X vs one competitor (illustrating persistent pricing styles)",
    x = "Time",
    y = "Price",
    color = "Line"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(OUT_DIR, paste0("P3_styles_", slice_country, "_", slice_category, "_pid", pid_case, ".png")),
       p3_case, width = 12, height = 7, dpi = 200)
print(p3_case)

# What you say:
# "These side-by-side panels suggest stable pricing 'styles' (fast follower vs rigid vs aggressive), motivating segmentation."
#i tried to have different graphs here 
# Identify Company X rows (adjust if your competitor_name differs)
company_x_names <- c("Company X", "company_x.nl", "company_x.be")

product_pick_tbl <- df%>%
  group_by(country, product_category, product_id) %>%
  summarise(
    # competitor activity excluding Company X
    avg_competitors_active = n_distinct(competitor_name[!competitor_name %in% company_x_names], na.rm=TRUE),
    # proxy for "Company X event count": number of Company X price changes
    cb_events = {
      cb <- df %>% filter(country==first(country), product_category==first(product_category),
                              product_id==first(product_id), competitor_name %in% company_x_names) %>%
        arrange(scrape_datetime) %>%
        mutate(cb_change = price != lag(price)) 
      sum(cb$cb_change, na.rm=TRUE)
    },
    # proxy for response observations: number of competitor price changes (excluding Company X)
    response_obs = {
      comp <- df %>% filter(country==first(country), product_category==first(product_category),
                                product_id==first(product_id), !competitor_name %in% company_x_names) %>%
        arrange(competitor_name, scrape_datetime) %>%
        group_by(competitor_name) %>%
        mutate(ch = price != lag(price)) %>%
        ungroup()
      sum(comp$ch, na.rm=TRUE)
    },
    .groups = "drop"
  ) %>%
  mutate(
    plot_priority_score = cb_events + 0.2*response_obs + 10*avg_competitors_active
  ) %>%
  group_by(country, product_category) %>%
  slice_max(plot_priority_score, n=2, with_ties = FALSE) %>%
  ungroup()

product_pick_tbl
make_style_plot <- function(data, country_sel, cat_sel, pid_sel, top_k = 6, min_obs = 30) {

  company_x_names <- c("Company X", "company_x.nl", "company_x.be")

  d <- data %>%
    filter(country == country_sel,
           product_category == cat_sel,
           product_id == pid_sel) %>%
    mutate(is_company_x = competitor_name %in% company_x_names)

  # Choose competitors WITHIN this product based on enough observations
  top_comps <- d %>%
    filter(!is_company_x) %>%
    count(competitor_name, name="n_obs") %>%
    filter(n_obs >= min_obs) %>%
    arrange(desc(n_obs)) %>%
    slice_head(n = top_k) %>%
    pull(competitor_name)

  # If too few competitors pass the threshold, relax it automatically
  if(length(top_comps) < 2) {
    top_comps <- d %>%
      filter(!is_company_x) %>%
      count(competitor_name, name="n_obs") %>%
      arrange(desc(n_obs)) %>%
      slice_head(n = max(2, min(top_k, n()))) %>%
      pull(competitor_name)
  }

  # Build a long dataset where each competitor becomes its own facet
  cb <- d %>% filter(is_company_x) %>% select(scrape_datetime, cb_price = price)

  plot_df <- d %>%
    filter(is_company_x | competitor_name %in% top_comps) %>%
    select(scrape_datetime, competitor_name, price, is_company_x) %>%
    left_join(cb, by="scrape_datetime") %>%
    mutate(panel = ifelse(is_company_x, "Company X", competitor_name)) %>%
    # we want each panel to include BOTH Company X + that competitor
    group_by(panel) %>%
    group_modify(~{
      if(.y$panel == "Company X") return(tibble()) # drop the standalone Company X panel
      comp_name <- .y$panel
      comp_part <- d %>% filter(competitor_name == comp_name) %>%
        select(scrape_datetime, competitor_name, price) %>%
        mutate(series = comp_name)
      cb_part <- d %>% filter(is_company_x) %>%
        select(scrape_datetime, competitor_name, price) %>%
        mutate(series = "Company X")
      bind_rows(comp_part, cb_part) %>%
        mutate(panel = comp_name)
    }) %>%
    ungroup()

  ggplot(plot_df, aes(x = scrape_datetime, y = price, group = series, color = series)) +
    geom_step(linewidth = 0.7, alpha = 0.9) +
    facet_wrap(~panel, scales = "free_y") +
    labs(
      title = paste0("Competitor style case studies | ", country_sel, " | ", cat_sel, " | pid=", pid_sel),
      subtitle = "Each panel: Company X vs one competitor (persistent pricing style)",
      x = "Time", y = "Price", color = "Series"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

#competitor-style plot

top_products <- product_selection_table %>%
  group_by(country, product_category) %>%
  slice_max(plot_priority_score, n = 2, with_ties = FALSE) %>%
  ungroup()

df_focus <- df %>%
  semi_join(top_products, by = c("country","product_category","product_id"))

top_competitors <- df_focus %>%
  filter(!is_company_x) %>%
  count(country, product_category, product_id, competitor_name, sort = TRUE) %>%
  group_by(country, product_category, product_id) %>%
  slice_max(n, n = 4, with_ties = FALSE) %>%
  ungroup()

df_style <- df_focus %>%
  filter(is_company_x | competitor_name %in% top_competitors$competitor_name) %>%
  semi_join(top_competitors %>% distinct(country, product_category, product_id),
            by = c("country","product_category","product_id"))

style_panels <- df_style %>%
  # sadece company_x ve ilgili competitor'lar
  group_by(country, product_category, product_id) %>%
  group_split() %>%
  purrr::map_dfr(function(d){
    
    cb <- d %>% filter(is_company_x) %>%
      distinct(product_id, country, product_category, scrape_datetime, .keep_all = TRUE)
    
    comps <- d %>% filter(!is_company_x) %>%
      distinct(product_id, competitor_name, country, product_category, scrape_datetime, .keep_all = TRUE)
    
    # her competitor için Company X ile aynı grafikte görünsün diye iki satırlık panel tipi:
    comp_list <- unique(comps$competitor_name)
    
    purrr::map_dfr(comp_list, function(cc){
      bind_rows(
        cb %>% mutate(panel = cc, series = "Company X"),
        comps %>% filter(competitor_name == cc) %>% mutate(panel = cc, series = cc)
      )
    })
  }) %>%
  ungroup()

#competitor panel
# FIXED VERSION: no duplicate panel, no many-to-many issues
# =========================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(purrr)

# -------------------------------
# 0) output folder
# -------------------------------
OUT_DIR <- "week4_plots"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

# -------------------------------
# 1) helpers
# -------------------------------
normalize_cb <- function(x) {
  ifelse(x %in% c("company_x.nl","company_x.be","Company X"), "Company X", x)
}

# Deduplicate: keep ONE row per product_id x competitor x scrape_datetime
# (choose last price observed in the data ordering)
dedup_prices <- function(df_market) {
  df_market %>%
    mutate(
      competitor_name = normalize_cb(competitor_name),
      scrape_datetime = ymd_hms(scrape_datetime, quiet = TRUE)
    ) %>%
    arrange(product_id, competitor_name, scrape_datetime) %>%
    group_by(product_id, competitor_name, scrape_datetime) %>%
    slice_tail(n = 1) %>%    # keep last row if duplicates
    ungroup()
}

pick_top_competitors <- function(df_market, pid, k = 4) {
  df_market %>%
    filter(product_id == pid, competitor_name != "Company X") %>%
    count(competitor_name, sort = TRUE) %>%
    slice_head(n = k) %>%
    pull(competitor_name)
}

# Build data for: Company X vs one competitor (facets)
build_style_df <- function(df_market, pid, competitors_keep) {
  
  d <- df_market %>%
    filter(product_id == pid,
           competitor_name %in% c("Company X", competitors_keep)) %>%
    mutate(series = competitor_name)
  
  # replicate Company X into each competitor panel
  cb <- d %>% filter(series == "Company X")
  
  comp <- d %>% filter(series != "Company X") %>%
    mutate(panel = series)
  
  cb_rep <- cb %>%
    tidyr::crossing(panel = competitors_keep)
  
  bind_rows(cb_rep, comp) %>%
    mutate(panel = factor(panel, levels = competitors_keep))
}

plot_style_case <- function(df_market, country_label, cat_label, pid, competitors_keep) {
  
  dd <- build_style_df(df_market, pid, competitors_keep)
  
  # plot as step to reflect price changes
  ggplot() +
    geom_step(
      data = dd %>% filter(series == "Company X"),
      aes(x = scrape_datetime, y = price, group = panel),
      linewidth = 1.2, alpha = 1
    ) +
    geom_step(
      data = dd %>% filter(series != "Company X"),
      aes(x = scrape_datetime, y = price, group = series, color = series),
      linewidth = 0.7, alpha = 0.9
    ) +
    facet_wrap(~panel, ncol = 2, scales = "free_y") +
    labs(
      title = paste0("Competitor style case study | ", country_label, " | ", cat_label, " | pid=", pid),
      subtitle = "Each panel compares Company X (thick line) vs one competitor",
      x = "Time", y = "Price", color = "Competitor"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
}

# -------------------------------
# 2) PREP DATA: dedup each market
# -------------------------------
be_phones_d <- dedup_prices(be_phones)
be_vacuum_d <- dedup_prices(be_vacuum)
nl_phones_d <- dedup_prices(nl_phones)
nl_vacuum_d <- dedup_prices(nl_vacuum)

# -------------------------------
# 3) Choose 2 products per market (you already have this table)
# If product_selection_table exists, we use it.
# Otherwise, define product IDs manually.
# -------------------------------
if (exists("product_selection_table")) {
  top_products <- product_selection_table %>%
    group_by(country, product_category) %>%
    slice_max(plot_priority_score, n = 2, with_ties = FALSE) %>%
    ungroup()
} else {
  stop("product_selection_table not found. Create it first or set product IDs manually.")
}

# helper to fetch chosen product IDs for each market
get_pids <- function(ctry, cat) {
  top_products %>%
    filter(country == ctry, product_category == cat) %>%
    pull(product_id)
}

pids_BE_phone <- get_pids("BE", "mobile_phones")
pids_BE_vac   <- get_pids("BE", "vacuum_cleaners")
pids_NL_phone <- get_pids("NL", "mobile_phones")
pids_NL_vac   <- get_pids("NL", "vacuum_cleaners")

# -------------------------------
# 4) Make & save style plots: 2 products per market
# -------------------------------
make_two_style_plots <- function(df_market, country_label, cat_label, pids, prefix) {
  
  plots <- map(pids, function(pid){
    comps <- pick_top_competitors(df_market, pid, k = 4)
    p <- plot_style_case(df_market, country_label, cat_label, pid, comps)
    
    fname <- file.path(OUT_DIR, paste0(prefix, "_pid_", pid, ".png"))
    ggsave(fname, p, width = 14, height = 8, dpi = 300)
    message("Saved: ", fname)
    p
  })
  
  plots
}

# BE phones (2 products)
plots_BE_phone <- make_two_style_plots(be_phones_d, "BE", "mobile_phones", pids_BE_phone, "style_BE_phones")
# BE vacuum (2 products)
plots_BE_vac   <- make_two_style_plots(be_vacuum_d, "BE", "vacuum_cleaners", pids_BE_vac, "style_BE_vacuum")
# NL phones (2 products)
plots_NL_phone <- make_two_style_plots(nl_phones_d, "NL", "mobile_phones", pids_NL_phone, "style_NL_phones")
# NL vacuum (2 products)
plots_NL_vac   <- make_two_style_plots(nl_vacuum_d, "NL", "vacuum_cleaners", pids_NL_vac, "style_NL_vacuum")

# -------------------------------
# 5) PLOT TYPE 4 — Reaction delay distribution
# Uses responses_with_cat if it exists, otherwise tells you.
# -------------------------------
if (exists("responses_with_cat")) {
  p_delay <- ggplot(responses_with_cat, aes(x = response_delay_hours)) +
    geom_histogram(bins = 50) +
    facet_grid(country ~ product_category) +
    labs(
      title = "Distribution of competitor response delays (hours)",
      subtitle = "Speed heterogeneity — motivates timing models (hazard / mixed-effects)",
      x = "Response delay (hours)",
      y = "Count"
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(file.path(OUT_DIR, "P4_delay_distribution.png"),
         p_delay, width = 12, height = 7, dpi = 300)
  
  print(p_delay)
  message("Saved: ", file.path(OUT_DIR, "P4_delay_distribution.png"))
} else {
  message("responses_with_cat not found — skip Plot Type 4 for now (or paste your response-construction code above).")
}

# -------------------------------
# 6) Where are plots saved?
# -------------------------------
message("All plots saved under: ", normalizePath(OUT_DIR))
message("Files: ")
print(list.files(OUT_DIR, pattern = "\\.png$", full.names = TRUE))

