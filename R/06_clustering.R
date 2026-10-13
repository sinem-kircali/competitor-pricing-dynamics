# =============================================================================
# 06 — POST-ESTIMATION CLUSTERING OF COMPETITORS          (Report §4.3, §5.4)
# -----------------------------------------------------------------------------
# Three model-implied behavioural dimensions per competitor and category:
#   propensity     predicted P(response within 24h) from the hazard model (04)
#   speed          exp(competitor random intercept) from a mixed model of
#                  log response delay  (<1 faster than average, >1 slower)
#   aggressiveness implied log |price change| when responding, from a
#                  regression with competitor fixed effects (per category)
# k-means (k = 3) on standardised dimensions, separately per category.
# Requires objects from 01 and 04.
# =============================================================================

log_step("06  Clustering")

responses <- event_level %>% filter(responded == 1)

# -----------------------------------------------------------------------------
# 1. Speed — linear mixed model of log response delay
# -----------------------------------------------------------------------------
speed_model <- lmer(log(response_delay_h + 0.01) ~ product_category + country + (1 | competitor_id),
                    data = responses %>% mutate(competitor_id = factor(competitor_id)))

speed_tbl <- ranef(speed_model)$competitor_id %>%
  rownames_to_column("competitor_id") %>%
  transmute(competitor_id = as.integer(competitor_id),
            speed_multiplier = exp(`(Intercept)`))

# -----------------------------------------------------------------------------
# 2. Aggressiveness — log absolute response size with competitor fixed effects
#    Evaluated per competitor at its own country and without a promotion.
# -----------------------------------------------------------------------------
aggressiveness_for <- function(category) {
  d <- responses %>%
    filter(product_category == category, response_abs_change > 0) %>%
    mutate(competitor_id = factor(competitor_id))
  m <- lm(log(response_abs_change) ~ competitor_id + cx_promo + country, data = d)
  grid <- d %>% distinct(competitor_id, country) %>% mutate(cx_promo = 0)
  grid %>%
    mutate(aggressiveness = predict(m, newdata = grid)) %>%
    group_by(competitor_id) %>%
    summarise(aggressiveness = mean(aggressiveness), .groups = "drop") %>%
    mutate(competitor_id = as.integer(as.character(competitor_id)), product_category = category)
}
aggressiveness_tbl <- map_dfr(hz_levels$product_category, aggressiveness_for)

# -----------------------------------------------------------------------------
# 3. Propensity — hazard-model 24h response probability at each competitor's
#    own covariate profile (typical Company X price decrease, no promotion),
#    averaged across countries weighted by exposure (event-competitor pairs)
# -----------------------------------------------------------------------------
competitor_ccc <- hazard_base %>%
  group_by(competitor_id, competitor_name, country, product_category) %>%
  summarise(competitor_relevance = mean(competitor_relevance),
            assortment_overlap   = mean(assortment_overlap),
            reprice_freq         = mean(reprice_freq),
            abs_gap              = mean(abs_gap),
            n_pairs              = n(), .groups = "drop") %>%
  left_join(hazard_base %>% group_by(product_category) %>%
              summarise(cx_abs_change = median(cx_abs_change)), by = "product_category") %>%
  mutate(cx_direction = "decrease", cx_promo = 0)

propensity_tbl <- competitor_ccc %>%
  bind_cols(hz_cumulative(competitor_ccc) %>%
              filter(t_hour == HORIZON_H) %>%
              arrange(scenario_id) %>%
              select(propensity = cum_response)) %>%
  group_by(product_category, competitor_id, competitor_name) %>%
  summarise(propensity = weighted.mean(propensity, n_pairs),
            n_pairs = sum(n_pairs), .groups = "drop")

# -----------------------------------------------------------------------------
# 4. k-means per category
# -----------------------------------------------------------------------------
cluster_category <- function(category, min_pairs = MIN_EXPOSURE_PAIRS, k = N_CLUSTERS) {
  feat <- propensity_tbl %>%
    filter(product_category == category, n_pairs >= min_pairs) %>%
    inner_join(speed_tbl, by = "competitor_id") %>%
    inner_join(aggressiveness_tbl, by = c("competitor_id", "product_category"))

  X <- scale(feat[, c("propensity", "speed_multiplier", "aggressiveness")])
  set.seed(SEED)
  km <- kmeans(X, centers = k, nstart = 50)

  # number clusters by mean propensity (1 = most responsive) so labels are stable
  rank_map <- tibble(raw = km$cluster, propensity = feat$propensity) %>%
    group_by(raw) %>% summarise(m = mean(propensity)) %>%
    mutate(cluster = rank(-m))
  feat$cluster <- factor(rank_map$cluster[match(km$cluster, rank_map$raw)])

  sil <- silhouette(as.integer(feat$cluster), dist(X))
  list(features = feat, silhouette = sil)
}

clusters <- set_names(hz_levels$product_category) %>% map(cluster_category)

cluster_features <- map_dfr(clusters, "features")
save_table(cluster_features, "clustering_competitor_features")

# Tables 8–9 — cluster summary statistics
cluster_summary <- cluster_features %>%
  group_by(product_category, cluster) %>%
  summarise(n_competitors = n(), exposure_pairs = sum(n_pairs),
            mean_propensity = mean(propensity), sd_propensity = sd(propensity),
            mean_speed = mean(speed_multiplier), sd_speed = sd(speed_multiplier),
            mean_aggressiveness = mean(aggressiveness), sd_aggressiveness = sd(aggressiveness),
            competitors = paste(competitor_name, collapse = ", "),
            .groups = "drop")
save_table(cluster_summary %>% filter(product_category == "mobile_phones"),   "table_8_clusters_mobile_phones")
save_table(cluster_summary %>% filter(product_category == "vacuum_cleaners"), "table_9_clusters_vacuum_cleaners")

# Silhouette validation (§5.4.4)
silhouette_summary <- imap_dfr(clusters, function(cl, category) {
  s <- cl$silhouette
  tibble(product_category = category, cluster = s[, "cluster"], width = s[, "sil_width"]) %>%
    group_by(product_category, cluster) %>%
    summarise(n = n(), avg_width = mean(width), .groups = "drop") %>%
    mutate(overall_avg_width = mean(s[, "sil_width"]))
})
save_table(silhouette_summary, "clustering_silhouette")

iwalk(clusters, function(cl, category) {
  png(file.path(OUT_DIR, "figures", paste0("fig_silhouette_", category, ".png")),
      width = 8, height = 5, units = "in", res = 300)
  plot(cl$silhouette, main = paste("Silhouette plot —", category), border = NA,
       col = c("#E15759", "#59A14F", "#4E79A7")[sort(as.integer(cl$features$cluster))])
  abline(v = mean(cl$silhouette[, "sil_width"]), lty = 2)
  dev.off()
})

# Figures 4–7 — clusters in two-dimensional space
scatter_plot <- function(category, y, ylab) {
  cluster_features %>%
    filter(product_category == category) %>%
    ggplot(aes(propensity, .data[[y]], colour = cluster)) +
    geom_point(size = 3) +
    geom_text(aes(label = competitor_name), vjust = -0.8, size = 3, show.legend = FALSE) +
    labs(title = paste0(nice_cat(category), ": propensity vs ", tolower(ylab)),
         x = "Propensity (predicted 24h response probability)", y = ylab, colour = "Cluster")
}
for (category in hz_levels$product_category) {
  save_plot(scatter_plot(category, "aggressiveness", "Aggressiveness"),
            paste0("fig_cluster_propensity_aggressiveness_", category))
  save_plot(scatter_plot(category, "speed_multiplier", "Speed multiplier (<1 faster, >1 slower)"),
            paste0("fig_cluster_propensity_speed_", category))
}
