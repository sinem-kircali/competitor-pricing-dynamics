# =============================================================================
# 05 — MODEL 3: MULTINOMIAL LOGIT OF COMPETITOR ROLES       (Report §4.2.3, §5.3)
# -----------------------------------------------------------------------------
# Each event-competitor pair is one of: no action (baseline), follower (moved
# within 24h after Company X), leader (moved within 6h before Company X).
# Coefficients are reported as relative risk ratios (RRR) vs no action.
# Company X's stock and next-day-delivery flags are constant at event time
# (always in stock, always next-day) and are therefore not included.
# =============================================================================

log_step("05  Multinomial role model")

role_df <- event_level %>%
  drop_na(competitor_relevance, assortment_overlap, cx_promo, cx_abs_change,
          cx_direction, cx_price_age, cx_price, cx_hour, cx_is_weekend) %>%
  mutate(
    cx_direction     = factor(cx_direction, levels = c("decrease", "increase")),
    country          = factor(country),
    product_category = factor(product_category),
    relevance_z  = zscore(competitor_relevance),
    overlap_z    = zscore(assortment_overlap),
    abs_change_z = zscore(cx_abs_change),
    price_age_z  = zscore(cx_price_age),
    log_price_z  = zscore(log(cx_price)),
    hour_z       = zscore(cx_hour)
  )

role_formula <- role3 ~ relevance_z + overlap_z + cx_promo + abs_change_z + cx_direction +
  price_age_z + log_price_z + hour_z + cx_is_weekend +
  country + product_category

role_model <- multinom(role_formula, data = role_df, trace = FALSE, maxit = 500)

# Table 5 — RRRs with Wald CIs and p-values
role_summary <- summary(role_model)
table_5 <- map_dfr(rownames(role_summary$coefficients), function(role) {
  tibble(outcome   = paste(role, "vs no_action"),
         term      = colnames(role_summary$coefficients),
         estimate  = role_summary$coefficients[role, ],
         std.error = role_summary$standard.errors[role, ])
}) %>%
  filter(term != "(Intercept)") %>%
  mutate(z = estimate / std.error,
         p.value = 2 * pnorm(-abs(z)),
         rrr = exp(estimate),
         rrr_low = exp(estimate - 1.96 * std.error),
         rrr_high = exp(estimate + 1.96 * std.error),
         sig = sig_stars(p.value))
save_table(table_5, "table_5_multinomial_rrr")

role_fit <- tibble(
  n = nrow(role_df), AIC = AIC(role_model),
  observed_no_action = mean(role_df$role3 == "no_action"),
  observed_follower  = mean(role_df$role3 == "follower"),
  observed_leader    = mean(role_df$role3 == "leader")
)
save_table(role_fit, "model3_fit")

# -----------------------------------------------------------------------------
# Figure 3 — predicted role probabilities with and without a promotion
# (other covariates at typical values: z = 0, most common factor levels)
# -----------------------------------------------------------------------------
mode_of <- function(x) names(sort(table(x), decreasing = TRUE))[1]

role_reference <- tibble(
  relevance_z = 0, overlap_z = 0, abs_change_z = 0, price_age_z = 0, log_price_z = 0, hour_z = 0,
  cx_is_weekend = 0L,
  cx_direction = factor(mode_of(role_df$cx_direction), levels = levels(role_df$cx_direction)),
  country = factor(mode_of(role_df$country), levels = levels(role_df$country)),
  product_category = factor(mode_of(role_df$product_category), levels = levels(role_df$product_category))
)

promo_probs <- bind_rows(role_reference %>% mutate(cx_promo = 0),
                         role_reference %>% mutate(cx_promo = 1)) %>%
  { bind_cols(select(., cx_promo), as_tibble(predict(role_model, newdata = ., type = "probs"))) } %>%
  pivot_longer(-cx_promo, names_to = "role", values_to = "probability")
save_table(promo_probs, "fig_3_promo_role_probabilities")

p_fig3 <- promo_probs %>%
  filter(role != "no_action") %>%
  mutate(role = str_to_title(role), promo = paste("Promo =", cx_promo)) %>%
  ggplot(aes(promo, probability, fill = role)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = percent(probability, accuracy = 0.1)),
            position = position_dodge(width = 0.8), vjust = -0.4, size = 3.5) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("Follower" = "#F28E2B", "Leader" = "#59A14F")) +
  labs(title = "Predicted role probabilities under promotions",
       subtitle = "Multinomial model at typical values of other covariates",
       x = NULL, y = "Predicted probability", fill = NULL) +
  theme(legend.position = "top")
save_plot(p_fig3, "fig_3_promo_role_probabilities", width = 8, height = 6)

# -----------------------------------------------------------------------------
# Tables 6–7 — competitor role profiles by segment (average predicted probabilities)
# -----------------------------------------------------------------------------
role_pred <- bind_cols(role_df, as_tibble(predict(role_model, newdata = role_df, type = "probs")))

competitor_role_profiles <- role_pred %>%
  group_by(country, product_category, competitor_name) %>%
  summarise(n_event_pairs = n(),
            avg_p_no_action = mean(no_action),
            avg_p_leader    = mean(leader),
            avg_p_follower  = mean(follower),
            .groups = "drop") %>%
  arrange(country, product_category, avg_p_no_action)
save_table(competitor_role_profiles, "model3_competitor_role_profiles")

top5_by_segment <- competitor_role_profiles %>%
  group_by(country, product_category) %>%
  slice_min(avg_p_no_action, n = 5, with_ties = FALSE) %>%
  ungroup()
save_table(top5_by_segment %>% filter(product_category == "mobile_phones"),   "table_6_top5_mobile_phones")
save_table(top5_by_segment %>% filter(product_category == "vacuum_cleaners"), "table_7_top5_vacuum_cleaners")
