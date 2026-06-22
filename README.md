# Competitor Pricing Dynamics: Leader–Follower Analysis

Analysis of competitor price reaction patterns for a leading Dutch online consumer electronics retailer — developed as a multi-week industry case study at Erasmus University Rotterdam.

> **Note:** Data is proprietary and not included in this repository. Scripts reference local CSV paths and can be adapted to equivalent data.

## Business Question
When a retailer changes its price, do competitors react — and if so, how quickly, in what direction, and which competitors lead vs. follow?

## Data (not included)
High-frequency web-scraped pricing data across two product categories × two markets:
- Mobile phones — Belgium & Netherlands
- Vacuum cleaners — Belgium & Netherlands
- 2.35M+ observations, September 2024 – December 2025
- Structured at product × competitor × timestamp level

## Methods & Scripts

| Script | Stage | Method |
|---|---|---|
| `company_x_Case_Study_NEW_CLEAN.R` | Data wrangling + EDA | `tidyverse`, summary stats, 5 simple analyses |
| `company_x_leader_follower_model.R` | Binary response model | Mixed-effects logistic regression (`lme4`) |
| `company_x_multinomial_leader_follower.R` | Three-class model | Multinomial logistic regression (`nnet`) |
| `company_x_multinomial_mixed_logit.R` | Final model | Multinomial mixed logit with competitor/product effects |

### Modelling Approach
Price change **events** are defined as discrete adjustments by the focal retailer. For each event, all competitors are observed within a **24-hour response window** (chosen based on empirical distribution: median reaction ~14 hours, 75th percentile ~20 hours).

Two response dimensions are modelled:
1. **Response probability** — whether a competitor reacts within 24 hours (binary logit with mixed effects)
2. **Response role** — leader / follower / inactive (multinomial logit)

## Key Findings
- **Reactions are not automatic**: most competitor–event pairs show no response within 24 hours, implying tactical pricing space often exists after a price change
- **Concentration among a few rivals**: a small subset of large, active competitors accounts for the majority of leadership and followership behaviour; most sellers remain passive
- **Promotions accelerate retaliation**: promotional activity significantly increases both the probability and speed of competitor reactions — promotions act as competitive signals, not just demand levers
- **Category and market heterogeneity**: competitive dynamics differ substantially between mobile phones and vacuum cleaners, and between Belgium and the Netherlands — uniform pricing rules are suboptimal

## Managerial Implications
- Monitoring systems should be **tiered**: high-frequency tracking for strategically relevant competitors, fewer resources for fringe players
- Promotional strategy should explicitly incorporate **expected retaliation risk**
- Pricing governance should be **category- and market-specific**

## Tech Stack
- R
- `tidyverse` · `lubridate` · `data.table`
- `lme4` · `nnet`
