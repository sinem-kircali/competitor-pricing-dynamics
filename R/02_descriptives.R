# =============================================================================
# 02 — DESCRIPTIVE STATISTICS                              (Report §3.3, Appendix)
# =============================================================================

log_step("02  Descriptive statistics")

# Dataset overview -------------------------------------------------------------
dataset_overview <- df %>%
  group_by(country, product_category) %>%
  summarise(
    n_observations  = n(),
    n_products      = n_distinct(product_id),
    n_competitors   = n_distinct(competitor_id[!is_cx]),
    share_price_change = mean(is_price_change),
    start = min(scrape_datetime), end = max(scrape_datetime),
    .groups = "drop"
  ) %>%
  left_join(cx_events %>% count(country, product_category, name = "n_cx_events"),
            by = c("country", "product_category")) %>%
  left_join(event_level %>% count(country, product_category, name = "n_event_competitor_pairs"),
            by = c("country", "product_category"))
save_table(dataset_overview, "data_overview")

# Price-change magnitudes (§3.3: most observations involve no repricing)
price_change_summary <- df %>%
  summarise(
    share_rows_with_change = mean(is_price_change),
    median_change_pct      = median(replace_na(price_change_pct, 0)),
    p75_change_pct         = quantile(replace_na(price_change_pct, 0), 0.75),
    median_abs_change_if_changed = median(abs(price_change_pct[is_price_change == 1]))
  )
save_table(price_change_summary, "price_change_summary")

# Overall response, leadership and role shares
overall_rates <- event_level %>%
  summarise(
    n_events = n_distinct(paste(product_id, country, cx_event_time)),
    n_pairs = n(),
    response_rate_24h = mean(responded),
    leader_rate_6h = mean(led),
    share_no_action = mean(role3 == "no_action"),
    share_follower  = mean(role3 == "follower"),
    share_leader    = mean(role3 == "leader"),
    share_same_direction_if_responded = mean(same_direction, na.rm = TRUE)
  )
save_table(overall_rates, "overall_response_rates")

responses <- event_level %>% filter(responded == 1)

# Table A1 — response delay distribution (first response per event-competitor pair)
table_A1 <- responses %>%
  summarise(n_responses = n(),
            min = min(response_delay_h), p25 = quantile(response_delay_h, 0.25),
            median = median(response_delay_h), p75 = quantile(response_delay_h, 0.75),
            max = max(response_delay_h))
save_table(table_A1, "table_A1_response_delay")

# Table A2 — response rates under alternative windows
table_A2 <- map_dfr(ROBUSTNESS_WINDOWS, function(w) {
  event_level %>%
    group_by(country, product_category) %>%
    summarise(response_rate = mean(!is.na(response_delay_h) & response_delay_h <= w),
              .groups = "drop") %>%
    mutate(window = paste0(w, "h"))
}) %>%
  pivot_wider(names_from = window, values_from = response_rate, names_prefix = "response_rate_")
save_table(table_A2, "table_A2_response_rate_by_window")

# Table A3 — response delay by segment
table_A3 <- responses %>%
  group_by(country, product_category) %>%
  summarise(n_responses = n(), p25 = quantile(response_delay_h, 0.25),
            median = median(response_delay_h), p75 = quantile(response_delay_h, 0.75),
            .groups = "drop")
save_table(table_A3, "table_A3_response_delay_by_segment")

# Table A4 — response probability by competitor relevance quartile (within country)
table_A4 <- event_level %>%
  group_by(country) %>%
  mutate(relevance_quartile = ntile(competitor_relevance, 4)) %>%
  group_by(country, product_category, relevance_quartile) %>%
  summarise(n_pairs = n(), response_rate = mean(responded),
            median_delay = median(response_delay_h[responded == 1]), .groups = "drop")
save_table(table_A4, "table_A4_response_by_relevance")

p_A4 <- ggplot(table_A4, aes(factor(relevance_quartile), response_rate, group = 1)) +
  geom_line() + geom_point(size = 2) +
  facet_grid(country ~ product_category) +
  scale_y_continuous(labels = percent) +
  labs(title = "Response rate by competitor relevance quartile",
       x = "Competitor relevance quartile (within country)", y = "24h response rate")
save_plot(p_A4, "fig_A4_response_by_relevance")

# Table A5 — leader / follower shares per competitor
table_A5 <- event_level %>%
  group_by(country, competitor_name) %>%
  summarise(n_pairs = n(),
            leader_share   = mean(led == 1),
            follower_share = mean(responded == 1),
            both_share     = mean(led == 1 & responded == 1),
            .groups = "drop") %>%
  arrange(country, desc(leader_share + follower_share))
save_table(table_A5, "table_A5_competitor_roles")

p_delay <- ggplot(responses, aes(response_delay_h)) +
  geom_histogram(binwidth = 1, boundary = 0) +
  facet_grid(country ~ product_category) +
  labs(title = "Delay of competitor responses (first response within 24h)",
       x = "Hours after Company X price change", y = "Responses")
save_plot(p_delay, "fig_response_delay_distribution")
