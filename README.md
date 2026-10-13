# Competitor Pricing Dynamics: Who Reacts, How Fast, and Who Leads

How do competitors respond when a leading Dutch online consumer-electronics retailer ("Company X") changes a price? This repository contains the analysis code for a multi-week industry case study at Erasmus University Rotterdam (MSc Data Science and Marketing Analytics). It uses high-frequency scraped price data for mobile phones and vacuum cleaners in Belgium and the Netherlands.

> **Data:** the price data are proprietary and not included. The code runs on the original four CSV files (see [Running the analysis](#running-the-analysis)).

## Research question

*How do competitors' pricing behaviours interact with Company X's pricing decisions across products and markets, and how can these interactions improve Company X's pricing strategy?*

| Sub-question | Answered by |
|---|---|
| SQ1 Which competitors lead and which follow? | Multinomial role model, role profiles |
| SQ2 How do competitors respond in timing, direction and magnitude? | Descriptives, hazard model |
| SQ3 How do responses differ across categories and countries? | All models (category and country terms), per-category clustering |
| SQ4 How do promotions and other context change reactions? | Mixed logit, hazard interactions, role model |
| SQ5 How can behaviour be segmented into actionable types? | Post-estimation clustering |

## Approach

**From prices to events.** Every Company X price change is an *event*. By default, a competitor enters the event's risk set if it sells the same product in the same country and posted a price in the 72 hours before the event. Retailers that don't offer the product can't respond, so counting them would inflate "non-response" (see [Risk-set definition](#risk-set-definition)). For each event–competitor pair:

| Construct | Definition |
|---|---|
| Response (follower) | competitor changes its price within **24 h after** the event |
| Leader | competitor changed its price within **6 h before** the event |
| Response delay | hours from the event to the competitor's first price change |
| Price gap | competitor's pre-event price relative to Company X's new price |
| Repricing frequency | competitor's price changes per day (structural speed) |

**Models.**

| Step | Method | Question |
|---|---|---|
| Model 1 | Mixed-effects logit, random intercepts for competitor and product (`lme4`) | *Whether* a competitor responds within 24 h |
| Model 2 | Discrete-time hazard (person-hour logit with hour dummies, right-censored at 24 h, standard errors clustered by competitor) | *When* a competitor responds; price-gap, promotion and direction effects |
| Model 3 | Multinomial logit: no action / follower / leader (`nnet`) | *Which role* a competitor takes around a price change |
| Segmentation | k-means (k = 3, per category) on model-implied propensity, speed and aggressiveness, validated with silhouette widths | Competitor archetypes for monitoring and pricing rules |
| Robustness | Hazard model under alternative risk sets and lookback windows; cluster stability across exposure cutoffs and k | How much the results depend on design choices |

## Key findings

- **Reactions are selective.** Most event–competitor pairs show no price move around a Company X change, so a temporary pricing window usually exists.
- **Who is watching matters more than what is sold.** Competitor relevance is the strongest driver of response, and the competitor-level random effect is much larger than the product-level one. A small set of large, active retailers accounts for most leading and following.
- **Promotions are competitive signals.** Promoted price changes raise the chance of being followed and change how the price gap matters.
- **Proximity is non-linear.** Mid-range gaps to Company X's new price create the most urgency for mobile phones; very small and very large gaps trigger fewer reactions.
- **Structural speed counts.** Competitors that reprice often respond faster. Once standard errors are clustered by competitor, repricing frequency explains response timing more reliably than relevance does.
- **Categories differ.** Mobile phones and vacuum cleaners differ in response intensity and in the structure of competitor archetypes, which supports category-specific pricing governance.

## Repository structure

```
run_all.R                    runs the full pipeline in order
R/00_setup.R                 packages, paths, analysis parameters, helpers
R/01_data_and_events.R       cleaning, Company X events, event × competitor panel
R/02_descriptives.R          descriptive statistics and appendix tables
R/03_mixed_logit.R           Model 1: response probability
R/04_hazard.R                Model 2: response timing, price-gap and threshold analysis
R/05_multinomial_roles.R     Model 3: leader / follower / no-action roles
R/06_clustering.R            competitor archetypes and silhouette validation
R/07_robustness.R            risk-set, lookback and clustering robustness
```

All analysis parameters (windows, lookback, risk set, thresholds, number of clusters, seed) are set once in `R/00_setup.R`.

## Running the analysis

Requirements: R ≥ 4.3 with `tidyverse` (dplyr ≥ 1.1), `lubridate`, `lme4`, `nnet`, `broom`, `broom.mixed`, `cluster`, `sandwich`, `scales`.

```bash
PRICE_DATA_DIR=/path/to/csv/folder \
FOCAL_NAMES="companyx.nl,companyx.be" \
Rscript run_all.R
```

- `PRICE_DATA_DIR` is the folder with the four `competitor_price_data_*.csv` files.
- `FOCAL_NAMES` lists the focal retailer's website names as they appear in `competitor_name`.
- `RISK_SET` (optional) is `listing` (default) or `country`; see below.

Results are written to `output/tables` (CSV) and `output/figures` (PNG). A full run takes about 10–15 minutes; most of the time is spent on the mixed-effects models, the threshold analysis and the robustness refits.

## Risk-set definition

Which competitors count as "able to respond" to an event changes the absolute numbers, so the pipeline supports two definitions:

| `RISK_SET` | Competitors paired with an event | Pairs | 24h response rate |
|---|---|---|---|
| `listing` (default) | offer the same product in the same country and posted a price in the 72 h before the event | ≈ 40k | ≈ 27% |
| `country` | every competitor in the country with assortment overlap > 0 | ≈ 111k | ≈ 10% |

In the `country` grid about two thirds of the pairs are retailers that weren't selling the product, and they almost never "respond" (0.3%). Because these are disproportionately low-relevance retailers, this definition also inflates the estimated effect of competitor relevance. The final report used the `country` grid, and `RISK_SET=country` reproduces its estimates. The default `listing` definition follows the report's own definition of competition (same product, same market).

The direction of the main findings is the same under both definitions:
- relevance and promotions drive whether competitors respond;
- price increases are followed more often;
- the price-gap effect reverses under promotions;
- repricing frequency drives speed;
- promotions shift competitors from leading to following.

`R/07_robustness.R` refits the hazard model for both definitions and for 24 h, 72 h and 168 h lookback windows. The results barely change across lookbacks.

## Notes on inference

- **Hazard model standard errors:** clustered by competitor, because each competitor contributes many person-hours. With conventional standard errors, the competitor-level terms (relevance, overlap, relevance × gap) look highly significant. Clustered, they are not, while the gap, promotion × gap, direction, magnitude and repricing-frequency effects remain significant.
- **Clustering:** it describes 12–15 competitors per category, so it is indicative. Mobile-phone clusters are identical for exposure cutoffs between 50 and 200 event–competitor pairs. Vacuum-cleaner clusters are more sensitive to the cutoff (see `robustness_cluster_exposure.csv`). The default cutoff of 100 avoids a single-competitor cluster.

## Tech stack

R · tidyverse · lme4 · nnet · sandwich · cluster
