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
  tmp <- primary
  tmp[c(1:delay)] <- 0
  secondary = tmp*ratio
  return(secondary)
}

generate_secondary_two_ratio <- function(primary, delay, ratio1, ratio2, tchange_prim){
  secondary_init <- rep(0, times = delay)
  secondary_tmp1 <- primary[(delay + 1):tchange_prim]*ratio1 #apply ratio1 to the first part of the primary ts after the delay
  secondary_tmp2 <- primary[(tchange_prim + 1): length(primary)]*ratio2 #Change the observation process to use ratio 2 after tchange_prim
  secondary <- c(secondary_init, secondary_tmp1, secondary_tmp2)
  return(secondary)
}
