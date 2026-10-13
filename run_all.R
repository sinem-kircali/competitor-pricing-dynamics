# =============================================================================
# Competitor price-response dynamics — full analysis pipeline
# -----------------------------------------------------------------------------
# Run from the repository root:
#   PRICE_DATA_DIR=/path/to/csvs FOCAL_NAMES="site.nl,site.be" Rscript run_all.R
# Add RISK_SET=country to reproduce the competitor grid used in the final report.
# Tables are written to output/tables, figures to output/figures.
# =============================================================================

source("R/00_setup.R")
source("R/01_data_and_events.R")     # cleaned panel, events, event x competitor panel
source("R/02_descriptives.R")        # §3.3 and appendix tables
source("R/03_mixed_logit.R")         # Model 1: response probability   (§5.1)
source("R/04_hazard.R")              # Model 2: response timing         (§5.2)
source("R/05_multinomial_roles.R")   # Model 3: leader / follower roles (§5.3)
source("R/06_clustering.R")          # competitor archetypes            (§5.4)
source("R/07_robustness.R")          # risk-set and clustering robustness

log_step("Done. Results in ", normalizePath(OUT_DIR))
