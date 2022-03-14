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
  secondary <- rep(0, length(primary))
  secondary[delay+1:length(secondary)] = primary[(delay+1:length(secondary))-delay]*ratio
  return(secondary)
}

generate_secondary_two_ratio <- function(primary, delay, ratio1, ratio2, tchange_prim){
  secondary <- rep(0, length(primary))
  secondary[(1:tchange_prim)+delay] = primary[1:tchange_prim]*ratio1
  secondary[((tchange_prim+1):(length(secondary)-delay))+delay] =  primary[((tchange_prim+1):(length(primary)-delay))]*ratio2
  return(secondary)
}
