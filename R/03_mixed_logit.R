# =============================================================================
# 03 — MODEL 1: MIXED-EFFECTS LOGIT OF RESPONSE PROBABILITY   (Report §4.2.1, §5.1)
# -----------------------------------------------------------------------------
# P(competitor responds within 24h) with random intercepts for competitor and
# product. Continuous predictors are standardised (effects per 1 SD).
# =============================================================================

log_step("03  Mixed-effects logit")

mix_df <- event_level %>%
  drop_na(competitor_relevance, assortment_overlap, cx_promo,
          cx_abs_change, cx_direction, cx_price_age) %>%
  mutate(
    cx_direction  = factor(cx_direction, levels = c("decrease", "increase")),
    country          = factor(country),
    product_category = factor(product_category),
    competitor_id    = factor(competitor_id),
    product_id       = factor(product_id),
    relevance_z  = zscore(competitor_relevance),
    overlap_z    = zscore(assortment_overlap),
    abs_change_z = zscore(cx_abs_change),       # magnitude of Company X's move
    price_age_z  = zscore(cx_price_age)
  )

glmer_ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

mix_formula <- responded ~ relevance_z + cx_promo + abs_change_z + cx_direction +
  price_age_z + overlap_z + country + product_category +
  (1 | competitor_id) + (1 | product_id)

mix_model <- glmer(mix_formula, data = mix_df, family = binomial, control = glmer_ctrl)

mix_coef <- tidy(mix_model, effects = "fixed", conf.int = TRUE) %>%
  mutate(odds_ratio = exp(estimate), or_low = exp(conf.low), or_high = exp(conf.high),
         sig = sig_stars(p.value))
save_table(mix_coef, "model1_mixed_logit_coefficients")

# Variance decomposition (§5.1.2): competitor vs product random intercepts
mix_variance <- as.data.frame(VarCorr(mix_model)) %>%
  transmute(level = grp, variance = vcov, sd = sdcor)
save_table(mix_variance, "model1_random_effect_variances")

# Does promotion amplify the relevance effect? (LRT, §4.2.1)
mix_model_int <- update(mix_model, . ~ . + relevance_z:cx_promo)
mix_lrt <- anova(mix_model, mix_model_int, test = "Chisq") %>%
  as_tibble(rownames = "model")
save_table(mix_lrt, "model1_lrt_relevance_x_promo")

# Fit quality
mix_df$pred <- predict(mix_model, type = "response")
mix_fit <- tibble(
  n = nrow(mix_df), AIC = AIC(mix_model),
  AUC = auc_rank(mix_df$responded, mix_df$pred),
  brier = mean((mix_df$responded - mix_df$pred)^2),
  brier_baseline = mean((mix_df$responded - mean(mix_df$responded))^2)
)
save_table(mix_fit, "model1_fit")

# -----------------------------------------------------------------------------
# Table 3 / Figure 1A — predicted probabilities (fixed effects only)
# Other covariates at their mean (z = 0), reference levels for factors.
# -----------------------------------------------------------------------------
rel_mean <- mean(mix_df$competitor_relevance)
rel_sd   <- sd(mix_df$competitor_relevance)

mix_newdata <- function(relevance_raw, promo) {
  tibble(
    relevance_z  = (relevance_raw - rel_mean) / rel_sd,
    cx_promo     = promo,
    abs_change_z = 0, price_age_z = 0, overlap_z = 0,
    cx_direction = factor("decrease", levels = levels(mix_df$cx_direction)),
    country = factor(levels(mix_df$country)[1], levels = levels(mix_df$country)),
    product_category = factor(levels(mix_df$product_category)[1],
                              levels = levels(mix_df$product_category))
  )
}

rel_q <- quantile(mix_df$competitor_relevance, c(0.25, 0.75))
table_3 <- expand_grid(relevance = c("Low relevance (p25)", "High relevance (p75)"),
                       promo = c(1, 0)) %>%
  mutate(relevance_raw = if_else(str_detect(relevance, "p25"), rel_q[1], rel_q[2]),
         predicted_probability = map2_dbl(relevance_raw, promo, ~
           predict(mix_model, newdata = mix_newdata(.x, .y), type = "response", re.form = NA)),
         promotion = if_else(promo == 1, "Promotion ON", "Promotion OFF")) %>%
  select(relevance, promotion, relevance_raw, predicted_probability)
save_table(table_3, "table_3_predicted_probabilities")

rel_grid <- seq(min(mix_df$competitor_relevance), max(mix_df$competitor_relevance), length.out = 100)
fig_1A_data <- expand_grid(relevance_raw = rel_grid, promo = c(0, 1)) %>%
  mutate(p = predict(mix_model, newdata = mix_newdata(relevance_raw, promo),
                     type = "response", re.form = NA),
         promotion = if_else(promo == 1, "Promotion ON", "Promotion OFF"))

p_1A <- ggplot(fig_1A_data, aes(relevance_raw, p, colour = promotion, linetype = promotion)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = quantile(mix_df$competitor_relevance, c(.25, .5, .75)),
             linetype = "dotted", colour = "grey50") +
  scale_y_continuous(labels = percent) +
  scale_colour_manual(values = c("Promotion OFF" = "#2C6FB7", "Promotion ON" = "#D73027")) +
  labs(title = "Higher competitor relevance is associated with greater response probability",
       subtitle = "Mixed-effects logit, fixed effects only; other variables at typical values",
       x = "Competitor relevance score", y = "Predicted P(response within 24h)",
       colour = "Company X promotion", linetype = "Company X promotion") +
  theme(legend.position = "bottom")
save_plot(p_1A, "fig_1A_relevance_predicted_probability")

# -----------------------------------------------------------------------------
# Robustness: same model with alternative response windows (§3.1)
# -----------------------------------------------------------------------------
window_robustness <- map_dfr(c(6, 24, 48), function(w) {
  d <- mix_df %>% mutate(responded = as.integer(!is.na(response_delay_h) & response_delay_h <= w))
  m <- glmer(mix_formula, data = d, family = binomial, control = glmer_ctrl)
  tidy(m, effects = "fixed") %>% mutate(window_h = w)
}) %>%
  filter(term != "(Intercept)") %>%
  transmute(term, window_h, estimate, p.value, sign = if_else(estimate > 0, "+", "-")) %>%
  pivot_wider(names_from = window_h, values_from = c(estimate, p.value, sign))
save_table(window_robustness, "model1_window_robustness")
