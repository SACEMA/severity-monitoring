#' A script to synthesize a dataset containing a time series of 'COVID-19'
#' incidence, increasing linearly with a fixed proportion
#'
#' PURPOSE: COVID-19 severity monitoring pipeline test-bed
#' Date: March 2022
#' Authors: JB, JA, CABP, JRCP

# Packages
library(dplyr)
library(lubridate)

# Helpers
source("./synthetic/scripts/define_params.R")
source("./synthetic/synth_data_functions.R")


# Generate the primary time series
primary_ts <- generate_primary_linear(
  length = ts_length,
  base_inc = in.flat_primary,
  rate_of_increase = 10
)

# Generate the secondary outcomes
secondary_ts <- generate_secondary_fixed_ratio(
  primary = primary_ts,
  delay = in.fixed_delay,
  ratio = in.prop_to_seconday
)

# Add a date column
reported_data <- data.frame(
  primary = primary_ts,
  secondary = secondary_ts
) %>%
  mutate(
    date = ymd(start_date) + 0:(n() - 1),
    .before = "primary"
  )


saveRDS(reported_data, "./synthetic/data/linear-ts-fixed-ratio.rds")
