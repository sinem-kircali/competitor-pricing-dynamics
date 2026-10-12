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

**From prices to events.** Every Company X price change is an *event*. A competitor enters the event's risk set if it sells the same product in the same country and posted a price in the 72 hours before the event. For each event–competitor pair:

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
| Model 2 | Discrete-time hazard (person-hour logit with hour dummies, right-censored at 24 h) | *When* a competitor responds; price-gap, promotion and direction effects |
| Model 3 | Multinomial logit: no action / follower / leader (`nnet`) | *Which role* a competitor takes around a price change |
| Segmentation | k-means (k = 3, per category) on model-implied propensity, speed and aggressiveness, validated with silhouette widths | Competitor archetypes for monitoring and pricing rules |

## Key findings

- **Reactions are selective.** Most event–competitor pairs show no price move around a Company X change, so a temporary pricing window usually exists.
- **Who is watching matters more than what is sold.** Competitor relevance is the strongest driver of response, and the competitor-level random effect is much larger than the product-level one. A small set of large, active retailers accounts for most leading and following.
- **Promotions are competitive signals.** Promoted price changes raise the chance of being followed and change how the price gap matters.
- **Proximity is non-linear.** Mid-range gaps to Company X's new price create the most urgency for mobile phones; very small and very large gaps trigger fewer reactions.
- **Structural speed counts.** Competitors that reprice often respond faster, regardless of strategic importance.
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
```

All analysis parameters (windows, lookback, thresholds, number of clusters, seed) are set once in `R/00_setup.R`.

## Running the analysis

Requirements: R ≥ 4.3 with `tidyverse`, `lubridate`, `lme4`, `nnet`, `broom`, `broom.mixed`, `cluster`, `scales`.

```bash
PRICE_DATA_DIR=/path/to/csv/folder \
FOCAL_NAMES="companyx.nl,companyx.be" \
Rscript run_all.R
```

- `PRICE_DATA_DIR` is the folder with the four `competitor_price_data_*.csv` files.
- `FOCAL_NAMES` lists the focal retailer's website names as they appear in `competitor_name`.

Results are written to `output/tables` (CSV) and `output/figures` (PNG). A full run takes a few minutes; most of the time is spent on the mixed-effects models and the threshold analysis.

## Tech stack

R · tidyverse · lme4 · nnet · cluster
