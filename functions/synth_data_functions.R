## old synth data functions ##

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


## end old functions ##

# functions defining the actual transformations on generic vectors

gen_grad_change_prop <- function(ts, # just eats a vector - needs wrapper function gen_grad_change_sec() to fit schema in functions/FXN-REQS.md
                                 prop1,
                                 prop2,
                                 t_change_start,
                                 duration_change,
                                 delay,
                                 baseval = 0){
  tslen <- length(ts)
  grad_props <- seq(prop1, prop2, length.out = duration_change)
  out_vec <- c(rep(baseval,delay),
               prop1*ts[1:(t_change_start-1)],
               grad_props*ts[t_change_start :(t_change_start + duration_change - 1)],
               prop2*ts[(t_change_start + duration_change) : (tslen - delay)]
               )
  return(out_vec)
}


get_const_prop_with_delay <- function(ts, prop, delay){ # same as gen_const_sec but inputs/outputs unnamed vector instead of case-specific dataframe
  tslen <- length(ts)
  return(c(rep(0,delay), prop*ts[1:tslen-delay]))
}


### wrapper functions


gen_const_sec <-  function(dd, prop, delay){
  tslen <- nrow(dd)
  dd$secondary_underlying <- get_const_prop_with_delay(ts = dd$primary_underlying,
                                                       prop = prop, 
                                                       delay = delay)
  return(dd)
}



gen_grad_change_sec <- function(dd, 
                                 prop1,
                                 prop2,
                                 t_change_start,
                                 duration_change,
                                 delay,
                                 baseval = 0){
  dd$secondary_underlying <- gen_grad_change_prop(dd$primary_underlying,
                                                  prop1 = prop1,
                                                  prop2 = prop2,
                                                  t_change_start = t_change_start,
                                                  duration_change = duration_change,
                                                  delay = delay,
                                                  baseval = baseval)
  return(dd)
}



obs_const_prim <- function(dd, prop, delay){ # wrapper for get_const_prop_with_delay
  dd$primary <- get_const_prop_with_delay( dd$primary_underlying,
                                           prop = prop, 
                                           delay = delay)
  return(dd)
}
  
obs_const_sec <- function(dd, prop, delay){
  dd$secondary <- get_const_prop_with_delay(dd$secondary_underlying,
                                            prop = prop,
                                            delay = delay)
  return(dd)
}

