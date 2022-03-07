# functions to synthesize test-tube incidence data for severity monitoring project
# March 2022
# JB, JA, CABP, JRCP


generate_primary_flat <- function(length, incidence){
  return(rep(incidence, length))
}

generate_primary_linear <- function(length, base_inc, rate_of_increase){
  return(base_inc + (1:length)*rate_of_increase)
}

generate_secondary_fixed_ratio <- function(primary, delay, ratio){
  secondary <- c(rep(0, length(primary)))
  secondary[delay+1:length(secondary)] = primary[delay+1:length(secondary)]*ratio
}