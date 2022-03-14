#' A script to synthesize a dataset containing a time series of 'COVID-19' 
#' incidence, increasing linearly with a fixed proportion
#' 
#' PURPOSE: COVID-19 severity monitoring pipeline test-bed 
#' Date: March 2022
#' Authors: JB, JA, CABP, JRCP

#Packages
library(dplyr)
library(lubridate)

#Helpers
source("./scripts/synth_data_params.R")
source("./synth_data_functions.R")


#Generate the primary time series
linear_inc_fixed_prop_ts <- generate_primary_linear(
  length = length,
  base_inc = base_incidence,
  rate_of_increase = 10
)


#Add the date column
final_df <- linear_inc_fixed_prop_ts %>% 
  mutate(date = ymd(start_date) + 0:length(.))


