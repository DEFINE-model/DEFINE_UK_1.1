# DEFINE-UK V1.1 — Replication Instructions

This file explains how to reproduce all figures and tables reported in the paper titled "Evaluating climate policy mixes in the UK: an E-SFC approach"

---

## Requirements

- **R** (version 4.0 or later) and **RStudio**
- The following R packages (installed automatically on first run if not present):

  `zoo`, `tidyverse`, `readxl`, `httr`, `dynamac`, `tseries`, `lmtest`, `urca`,
  `dyn`, `tsDyn`, `dplyr`, `dLagM`, `openxlsx`, `car`, `seasonal`, `ggplot2`,
  `ggthemes`, `showtext`, `sysfonts`, `showtextdb`, `xtable`, `data.table`,
  `Cairo`, `mFilter`

---

## Files

```
DEFINE-UK V1_1.Rmd   ← single model file; run this to reproduce all outputs
input/               ← input data (do not modify)
output/              ← all outputs are written here (created automatically)
```

---

## How to Run

1. Open `DEFINE-UK V1_1.Rmd` in RStudio.
2. Run all chunks in order (**Ctrl+Alt+R**, or *Run → Run All*).
3. All outputs are written to the `output/` folder automatically.

---

## Output Locations

All outputs are written to subdirectories of `output/`:

### Figures — `output/plots/`

Figures are organised by policy block and, where applicable, by credibility case:

| Subfolder | Content |
|---|---|
| `plots/power_sector_regulation/<case>/` | Figures for the fossil fuel ban and power sector subsidy scenarios (S1–S4), one subfolder per credibility case |
| `plots/housing_regulation/` | Figures for the housing regulation scenarios (S1, S5, S6) |
| `plots/gvt_investment/` | Figures for the green public investment scenarios (S1, S8, S9) |
| `plots/mixed/` | Figures for the combined policy scenario (S1, S4, S6, S7) |

The five credibility cases within `power_sector_regulation/` are:

| Subfolder | Description |
|---|---|
| `mutual_trust/` | High private and government credibility |
| `unexpected_enforcement/` | Low private credibility, high government credibility |
| `poor_credibility/` | Low private and government credibility |
| `false_confidence/` | High private credibility, low government credibility |
| `no_forward_guidance/` | Zero private credibility |

### Data tables — `output/tables/`

CSV files containing the full simulated time series for each scenario, organised in the same subfolder structure as the figures.

### Validation — `output/validation/`

Validation plots comparing simulated baseline paths against historical data.

---

## Settings

The following flags at the top of `DEFINE-UK V1_1.Rmd` control what is generated:

```r
Scenarios      <- 1:9  # Scenarios to run (1–9)
make_plots     <- 1    # Set to 1 to produce figures (0 to skip)
result_tables  <- 1    # Set to 1 to produce CSV tables (0 to skip)
run_sensitivity <- 0   # Set to 1 to run sensitivity analysis (slow)
```

The default settings reproduce all figures in the paper. There is no need to change these.
