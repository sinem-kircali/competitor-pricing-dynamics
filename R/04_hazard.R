# =============================================================================
# 04 — MODEL 2: DISCRETE-TIME HAZARD OF RESPONSE TIMING     (Report §4.2.2, §5.2)
# -----------------------------------------------------------------------------
# Person-period (event-competitor-hour) data: one row per hour t = 1..24 that a
# competitor is still "at risk" of responding. y = 1 in the hour it responds.
# Non-responders are right-censored at 24h. Logit of the hourly hazard with a
# fully flexible baseline (hour dummies). Continuous predictors are mean-centred.
# =============================================================================

log_step("04  Discrete-time hazard model")

HORIZON_H <- RESPONSE_WINDOW_H

# Turn raw covariate values into the model's (centred, factor) scale
hz_prepare <- function(d, center = hz_center, levels = hz_levels) {
  if (!"gap_missing" %in% names(d)) d$gap_missing <- 0L
  d %>% mutate(
    relevance_c      = competitor_relevance - center$relevance,
    overlap_c        = assortment_overlap   - center$overlap,
    reprice_freq_c   = reprice_freq         - center$reprice_freq,
    abs_gap_c        = abs_gap              - center$abs_gap,
    abs_change_c     = cx_abs_change        - center$abs_change,
    country          = factor(country,          levels = levels$country),
    product_category = factor(product_category, levels = levels$product_category),
    cx_direction     = factor(cx_direction,     levels = levels$cx_direction)
  )
}

# Build the pair-level base table, centring constants and person-hour data
hazard_inputs <- function(el) {
  base <- el %>%
    drop_na(competitor_relevance, assortment_overlap, reprice_freq,
            cx_promo, cx_abs_change, cx_direction) %>%
    mutate(
      # only the "country" risk set has competitors without a price for the
      # product; their gap is set to the mean and flagged
      gap_missing = as.integer(is.na(abs_gap)),
      abs_gap     = if_else(is.na(abs_gap), mean(abs_gap, na.rm = TRUE), abs_gap),
      status      = responded,
      # hour bin in which the response falls (1 = first hour); censored at 24h
      event_hour  = if_else(status == 1, pmax(1L, as.integer(ceiling(response_delay_h))), NA_integer_),
      last_hour   = if_else(status == 1, event_hour, as.integer(HORIZON_H)),
      pair_id     = row_number()
    )
  center <- base %>%
    summarise(relevance = mean(competitor_relevance), overlap = mean(assortment_overlap),
              reprice_freq = mean(reprice_freq), abs_gap = mean(abs_gap),
              abs_change = mean(cx_abs_change)) %>%
    as.list()
  levels <- list(country          = sort(unique(base$country)),
                 product_category = sort(unique(base$product_category)),
                 cx_direction     = c("decrease", "increase"))
  long <- base %>%
    uncount(last_hour, .id = "t", .remove = FALSE) %>%
    mutate(y = as.integer(status == 1 & t == event_hour),
           t = factor(t, levels = 1:HORIZON_H)) %>%
    hz_prepare(center, levels)
  list(base = base, long = long, center = center, levels = levels)
}

hazard_formula_for <- function(long) {
  f <- y ~ t +
    relevance_c + overlap_c + cx_promo + abs_change_c + cx_direction +
    abs_gap_c + reprice_freq_c + product_category + country +
    abs_gap_c:relevance_c + abs_gap_c:cx_promo + abs_gap_c:cx_direction
  if (n_distinct(long$gap_missing) > 1) f <- update(f, . ~ . + gap_missing)
  f
}

# Coefficients with standard errors clustered by competitor: a competitor's
# person-hours are not independent observations
hazard_table <- function(model, long) {
  robust_tidy(model, cluster = long$competitor_id) %>%
    filter(term != "(Intercept)", !str_detect(term, "^t[0-9]+$")) %>%
    mutate(odds_ratio = exp(estimate),
           or_low  = exp(estimate - 1.96 * std.error),
           or_high = exp(estimate + 1.96 * std.error),
           sig = sig_stars(p.value))
}

hz <- hazard_inputs(event_level)
hazard_base <- hz$base
hazard_long <- hz$long
hz_center   <- hz$center
hz_levels   <- hz$levels

# Integrity checks: one event per responder, none for censored pairs
hazard_checks <- hazard_long %>%
  group_by(pair_id) %>%
  summarise(status = first(status), events = sum(y), .groups = "drop") %>%
  summarise(pairs = n(), responders = sum(status),
            responders_with_one_event = sum(status == 1 & events == 1),
            censored_with_no_event    = sum(status == 0 & events == 0))
stopifnot(hazard_checks$responders == hazard_checks$responders_with_one_event)
save_table(hazard_checks, "model2_risk_set_checks")

hazard_formula <- hazard_formula_for(hazard_long)
hazard_model   <- glm(hazard_formula, data = hazard_long, family = binomial)

# Table 4 — coefficients (hour dummies omitted), competitor-clustered SEs
table_4 <- hazard_table(hazard_model, hazard_long)
save_table(table_4, "table_4_hazard_coefficients")

# Model fit: AIC and likelihood-ratio tests for the interactions and the
# hour-specific baseline (§5.2.2)
hazard_no_int  <- update(hazard_model, . ~ . - abs_gap_c:relevance_c - abs_gap_c:cx_promo -
                           abs_gap_c:cx_direction)
hazard_no_time <- update(hazard_model, . ~ . - t)
lrt_int  <- anova(hazard_no_int,  hazard_model, test = "LRT")
lrt_time <- anova(hazard_no_time, hazard_model, test = "LRT")
hazard_fit <- tibble(
  risk_set = RISK_SET, pairs = nrow(hazard_base), person_hours = nrow(hazard_long),
  AIC = AIC(hazard_model),
  lrt_interactions_chisq = lrt_int$Deviance[2],  lrt_interactions_p = lrt_int$`Pr(>Chi)`[2],
  lrt_hour_dummies_chisq = lrt_time$Deviance[2], lrt_hour_dummies_p = lrt_time$`Pr(>Chi)`[2]
)
save_table(hazard_fit, "model2_fit")
rm(hazard_no_int, hazard_no_time, lrt_int, lrt_time)

# -----------------------------------------------------------------------------
# Prediction helper: cumulative response probability over hours 1..24
# `scenarios` holds raw covariate values, one row per scenario.
# -----------------------------------------------------------------------------
hz_typical <- list(
  competitor_relevance = hz_center$relevance, assortment_overlap = hz_center$overlap,
  reprice_freq = hz_center$reprice_freq,
  cx_abs_change = median(hazard_base$cx_abs_change),
  cx_promo = 0, gap_missing = 0L, country = hz_levels$country[1]
)

hz_cumulative <- function(scenarios) {
  for (v in names(hz_typical)) if (!v %in% names(scenarios)) scenarios[[v]] <- hz_typical[[v]]
  scenarios %>%
    mutate(scenario_id = row_number()) %>%
    crossing(t_hour = 1:HORIZON_H) %>%
    mutate(t = factor(t_hour, levels = 1:HORIZON_H)) %>%
    hz_prepare() %>%
    mutate(hazard = predict(hazard_model, newdata = ., type = "response")) %>%
    group_by(scenario_id) %>%
    arrange(t_hour, .by_group = TRUE) %>%
    mutate(cum_response = 1 - cumprod(1 - hazard)) %>%
    ungroup()
}

gap_grid <- seq(0, 0.25, by = 0.01)

# Figure 1 (left) — 24h response probability by gap, direction and category (no promo)
curves_no_promo <- expand_grid(abs_gap = gap_grid, cx_direction = hz_levels$cx_direction,
                               product_category = hz_levels$product_category) %>%
  hz_cumulative() %>% filter(t_hour == HORIZON_H) %>% mutate(cx_promo = 0)

# Figure 1 (right) — the same under a Company X promotion, price decreases
curves_promo <- expand_grid(abs_gap = gap_grid, cx_direction = "decrease",
                            product_category = hz_levels$product_category, cx_promo = 1) %>%
  hz_cumulative() %>% filter(t_hour == HORIZON_H)

p_fig1a <- curves_no_promo %>%
  mutate(category = nice_cat(as.character(product_category)),
         move = if_else(cx_direction == "decrease", "Price decrease", "Price increase")) %>%
  ggplot(aes(abs_gap * 100, cum_response * 100, colour = move)) +
  geom_line(linewidth = 1.3) +
  facet_wrap(~ category, scales = "free_y") +
  scale_colour_manual(values = c("Price decrease" = "#D73027", "Price increase" = "#4575B4")) +
  labs(title = "24-hour response probability by category and direction",
       subtitle = "Average competitor, no promotion",
       x = "Price gap from Company X's new price (%)", y = "P(response within 24h), %",
       colour = "Company X move") +
  theme(legend.position = "top")
save_plot(p_fig1a, "fig_1_left_hazard_gap_direction")

p_fig1b <- curves_promo %>%
  mutate(category = nice_cat(as.character(product_category))) %>%
  ggplot(aes(abs_gap * 100, cum_response * 100, colour = category)) +
  geom_line(linewidth = 1.3) +
  scale_colour_manual(values = c("Mobile Phones" = "#4575B4", "Vacuum Cleaners" = "#D73027")) +
  labs(title = "24-hour response probability under promotion (price decreases)",
       subtitle = "Average competitor",
       x = "Price gap from Company X's new price (%)", y = "P(response within 24h), %",
       colour = "Category")
save_plot(p_fig1b, "fig_1_right_hazard_promo")

# Promotion multiplier by gap (Finding 2)
promo_multiplier <- curves_no_promo %>%
  filter(cx_direction == "decrease") %>%
  select(product_category, abs_gap, no_promo = cum_response) %>%
  inner_join(curves_promo %>% select(product_category, abs_gap, promo = cum_response),
             by = c("product_category", "abs_gap")) %>%
  filter(round(abs_gap, 2) %in% c(0.01, 0.05, 0.10, 0.15, 0.20)) %>%
  mutate(multiplier = promo / no_promo, pp_difference = 100 * (promo - no_promo))
save_table(promo_multiplier, "model2_promo_multiplier")

# Directional asymmetry and category comparison (Finding 3, §5.2.1)
direction_category_table <- curves_no_promo %>%
  filter(round(abs_gap, 2) %in% c(0.01, 0.05, 0.10, 0.15, 0.20, 0.25)) %>%
  select(product_category, abs_gap, cx_direction, cum_response) %>%
  pivot_wider(names_from = cx_direction, values_from = cum_response) %>%
  mutate(increase_vs_decrease = increase / decrease - 1)
save_table(direction_category_table, "model2_direction_by_category")

category_timing <- expand_grid(abs_gap = 0.05, cx_direction = "decrease",
                               product_category = hz_levels$product_category) %>%
  hz_cumulative() %>%
  filter(t_hour %in% c(6, 12, 24)) %>%
  select(product_category, t_hour, cum_response)
save_table(category_timing, "model2_category_timing")

# Finding 1 — gap versus magnitude: log-odds contribution of a 1 SD increase
dominance <- tibble(
  term = c("abs_change_c", "abs_gap_c"),
  sd = c(sd(hazard_base$cx_abs_change), sd(hazard_base$abs_gap)),
  beta = coef(hazard_model)[c("abs_change_c", "abs_gap_c")]
) %>%
  mutate(contribution_1sd = beta * sd, odds_ratio_1sd = exp(contribution_1sd))
save_table(dominance, "model2_gap_vs_magnitude")

# -----------------------------------------------------------------------------
# Figure 2 — non-linear proximity ("threat zone") by category and direction
# Re-estimate the hazard within each category × direction subset with a
# near-parity indicator 1(|gap| < threshold) for a range of thresholds.
# Standard errors are clustered by competitor.
# -----------------------------------------------------------------------------
log_step("04  Threshold analysis")

threshold_formula <- y ~ t + relevance_c + overlap_c + cx_promo + abs_change_c + parity +
  abs_gap_c + abs_gap_c:relevance_c + abs_gap_c:cx_promo + reprice_freq_c + country
if (n_distinct(hazard_long$gap_missing) > 1) {
  threshold_formula <- update(threshold_formula, . ~ . + gap_missing)
}

threshold_results <- expand_grid(product_category = hz_levels$product_category,
                                 cx_direction = hz_levels$cx_direction,
                                 threshold = GAP_THRESHOLDS) %>%
  mutate(fit = pmap(list(product_category, cx_direction, threshold), function(cat, dir, thr) {
    d <- hazard_long %>%
      filter(product_category == cat, cx_direction == dir) %>%
      mutate(parity = as.integer(abs_gap < thr))
    if (n_distinct(d$parity) < 2) return(tibble(estimate = NA_real_, std.error = NA_real_, p.value = NA_real_))
    robust_tidy(glm(threshold_formula, data = d, family = binomial), cluster = d$competitor_id) %>%
      filter(term == "parity") %>% select(estimate, std.error, p.value)
  })) %>%
  unnest(fit) %>%
  mutate(sig = sig_stars(p.value))
save_table(threshold_results, "fig_2_threshold_results")

p_fig2 <- threshold_results %>%
  mutate(category = nice_cat(product_category),
         move = if_else(cx_direction == "decrease", "Decrease", "Increase")) %>%
  ggplot(aes(threshold * 100, estimate, colour = move)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 1.1) + geom_point(size = 2.3) +
  geom_text(aes(label = sig), vjust = -0.8, size = 3.5, show.legend = FALSE) +
  facet_wrap(~ category) +
  scale_colour_manual(values = c("Decrease" = "#D73027", "Increase" = "#4575B4")) +
  labs(title = "Threshold effect by product category and direction",
       subtitle = "Coefficient on 1(|gap| < threshold) in the hazard model",
       x = "Threshold (% price difference)", y = "Coefficient (log-odds)", colour = "Price move")
save_plot(p_fig2, "fig_2_threshold_by_category_direction")

# -----------------------------------------------------------------------------
# Finding 5 — structural speed vs strategic importance: most exposed
# competitors per category, evaluated at their own covariate profile
# -----------------------------------------------------------------------------
competitor_profiles <- hazard_base %>%
  group_by(product_category, country, competitor_name) %>%
  summarise(competitor_relevance = mean(competitor_relevance),
            assortment_overlap = mean(assortment_overlap),
            reprice_freq = mean(reprice_freq), n_pairs = n(), .groups = "drop") %>%
  group_by(product_category) %>%
  slice_max(n_pairs, n = 3, with_ties = FALSE) %>%
  ungroup()

competitor_curves <- competitor_profiles %>%
  crossing(abs_gap = c(0.05, 0.10, 0.15), cx_direction = hz_levels$cx_direction) %>%
  hz_cumulative() %>%
  filter(t_hour == HORIZON_H) %>%
  select(product_category, country, competitor_name, competitor_relevance, reprice_freq,
         abs_gap, cx_direction, cum_response)
save_table(competitor_curves, "model2_competitor_profiles")

# Empirical cumulative response curve (descriptive)
empirical_curve <- tibble(t_hour = 1:HORIZON_H) %>%
  mutate(cum_response = map_dbl(t_hour, ~ mean(hazard_base$status == 1 &
                                               hazard_base$event_hour <= .x, na.rm = TRUE)))
p_emp <- ggplot(empirical_curve, aes(t_hour, cum_response)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = percent) +
  labs(title = "Cumulative share of event-competitor pairs that responded",
       x = "Hours since Company X price change", y = "Cumulative response share")
save_plot(p_emp, "fig_empirical_cumulative_response")
