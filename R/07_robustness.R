# =============================================================================
# 07 — ROBUSTNESS OF THE MAIN DESIGN CHOICES
# -----------------------------------------------------------------------------
# A. Risk-set definition: does the hazard model depend on which competitors are
#    counted as able to respond? Compares the "listing" risk set for several
#    lookback windows with the "country" risk set used in the final report.
# B. Clustering stability: are the competitor archetypes stable when the
#    minimum-exposure cutoff or the number of clusters changes?
# Requires objects and functions from 01, 04 and 06.
# =============================================================================

log_step("07  Robustness: risk-set definition")

risk_set_variants <- bind_rows(
  tibble(risk_set = "listing", lookback_h = LOOKBACK_SENSITIVITY),
  tibble(risk_set = "country", lookback_h = NA_real_)
)

key_terms <- c("relevance_c", "cx_promo", "abs_change_c", "cx_directionincrease", "abs_gap_c",
               "reprice_freq_c", "product_categoryvacuum_cleaners",
               "relevance_c:abs_gap_c", "cx_promo:abs_gap_c", "cx_directionincrease:abs_gap_c")

risk_set_results <- pmap_dfr(risk_set_variants, function(risk_set, lookback_h) {
  el <- build_event_level(risk_set, lookback_h)
  h  <- hazard_inputs(el)
  m  <- glm(hazard_formula_for(h$long), data = h$long, family = binomial)
  hazard_table(m, h$long) %>%
    filter(term %in% key_terms) %>%
    transmute(term, estimate, std.error, p.value) %>%
    mutate(risk_set = risk_set,
           lookback_h = lookback_h,
           pairs = nrow(h$base),
           response_rate_24h = mean(h$base$status))
})

risk_set_summary <- risk_set_results %>%
  mutate(variant = if_else(risk_set == "country", "country (report)",
                           paste0("listing, ", lookback_h, "h"))) %>%
  select(variant, pairs, response_rate_24h, term, estimate, p.value)
save_table(risk_set_summary, "robustness_risk_set")

risk_set_wide <- risk_set_summary %>%
  mutate(cell = sprintf("%.3f%s", estimate, sig_stars(p.value))) %>%
  select(variant, pairs, response_rate_24h, term, cell) %>%
  pivot_wider(names_from = term, values_from = cell)
save_table(risk_set_wide, "robustness_risk_set_wide")

# -----------------------------------------------------------------------------
log_step("07  Robustness: clustering stability")

# Agreement with the main solution when the exposure cutoff changes
# (adjusted Rand index on the competitors present in both solutions)
exposure_stability <- expand_grid(product_category = hz_levels$product_category,
                                  min_pairs = EXPOSURE_SENSITIVITY) %>%
  mutate(result = map2(product_category, min_pairs, function(category, min_pairs) {
    base <- clusters[[category]]$features %>% select(competitor_id, cluster_main = cluster)
    alt  <- cluster_category(category, min_pairs = min_pairs)$features %>%
      select(competitor_id, cluster_alt = cluster)
    both <- inner_join(base, alt, by = "competitor_id")
    tibble(n_competitors = nrow(alt),
           n_common = nrow(both),
           adjusted_rand_vs_main = adjusted_rand(both$cluster_main, both$cluster_alt),
           smallest_cluster = min(table(alt$cluster_alt)))
  })) %>%
  unnest(result)
save_table(exposure_stability, "robustness_cluster_exposure")

# Average silhouette width for alternative numbers of clusters
k_comparison <- expand_grid(product_category = hz_levels$product_category, k = K_SENSITIVITY) %>%
  mutate(result = map2(product_category, k, function(category, k) {
    cl <- cluster_category(category, k = k)
    tibble(avg_silhouette = mean(cl$silhouette[, "sil_width"]),
           smallest_cluster = min(table(cl$features$cluster)))
  })) %>%
  unnest(result)
save_table(k_comparison, "robustness_cluster_k")
