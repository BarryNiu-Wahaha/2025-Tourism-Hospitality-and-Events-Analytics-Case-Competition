# Hotel Performance and Concert Analysis

This repository analyzes hotel occupancy, average daily rate, RevPAR, demand,
and revenue around concerts in selected U.S. cities.

## Run the analysis

Run these scripts from the repository root in order:

```r
source("R/01_import_market_data.R")
source("R/02_define_concerts.R")
source("R/03_concert_performance_summary.R")
source("R/04_concert_window_trends.R")
```

The seasonal scripts require the imported data to include `Concert_Window`.

## Data

Raw data is kept in `data/raw/` for local analysis. Do not publish it unless
you have permission to redistribute it. A small anonymized sample can be kept
in `data/sample/` for demonstrating the workflow.

## Results

Generated figures are saved under `results/figures/`.