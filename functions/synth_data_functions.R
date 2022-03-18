## old synth data functions ##

#' Generate a time series with the same incidence per time
#'
#' @param init_primary Initial number of cases
#' @param ts_length Desired length of the time series
#'
#' @return A vector of outcomes
#' @export
#'
#' @examples gen_flat_prim(50, 50)
gen_flat_prim_D1 <- function(init_primary, ts_length) {
  time_series <- rep(init_primary, ts_length)
  ts_df <- data.frame(primary_underlying = time_series)
  return(ts_df)
}


#' Generate a linearly increasing time series
#' @param init_primary Initial number of cases
#' @param ts_length Desired length of the time series
#' @param tchange1_prim Time point where the time series starts to increase linearly
#' @param change_rate_linear_prim Linear rate of change
#'
#' @return A vector of outcomes
#' @export
#'
#' @examples gen_linear_prim(100, 50, 3, 0.1)
gen_linear_prim_D1 <- function(init_primary,
                            ts_length,
                            tchange1_prim,
                            change_rate_linear_prim) {
  t_diff <- ts_length - tchange1_prim
  constant_ts <- rep(init_primary, tchange1_prim)
  increasing_ts <- init_primary + change_rate_linear_prim * seq(1, t_diff, 1)
  time_series <- c(constant_ts, increasing_ts)
  ts_df <- data.frame(primary_underlying = time_series)
  return(ts_df)
}

#' Generate an exponentially increasing time series
#'
#' @param init_primary Initial number of cases
#' @param ts_length Desired length of the time series
#' @param change_rate_exponential_prim Exponential rate of change
#' @param tchange1_prim Time point where the time series starts to increase exponentially
#'
#' @return
#' @export
#'
#' @examples gen_exp_prim(50, 50, 0.1)
gen_exp_prim_D1 <- function(init_primary,
                         ts_length,
                         change_rate_exponential_prim,
                         tchange1_prim) {
  if (tchange1_prim >= ts_length) {
    stop("time point of change cannot be beyond the length of the time series")
  }

  t_diff <- ts_length - tchange1_prim
  constant_ts <- rep(init_primary, tchange1_prim)
  exp_ts <- init_primary * (exp(change_rate_exponential_prim * seq(1, ts_length - tchange1_prim, 1)))
  time_series <- c(constant_ts, exp_ts)
  ts_df <- data.frame(primary_underlying = time_series)
  return(ts_df)
}


### functions defining the underlying transformations on generic vectors ###

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
  return(c(rep(0,delay), prop*ts[1:(tslen-delay)]))
}

# tmp <- rep(10,100)
# get_const_prop_with_delay(tmp, .2 , 20)

### wrapper functions


gen_const_sec_D2 <-  function(dd, prop, delay){
  tslen <- nrow(dd)
  dd$secondary_underlying <- get_const_prop_with_delay(ts = dd$primary_underlying,
                                                       prop = prop, 
                                                       delay = delay)
  return(dd)
}




gen_grad_change_sec_D2 <- function(dd, 
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



obs_const_prim_D3 <- function(dd, prop, delay){ # wrapper for get_const_prop_with_delay
  dd$primary <- get_const_prop_with_delay( dd$primary_underlying,
                                           prop = prop, 
                                           delay = delay)
  return(dd)
}
  
obs_const_sec_D4 <- function(dd, prop, delay){
  dd$secondary <- get_const_prop_with_delay(dd$secondary_underlying,
                                            prop = prop,
                                            delay = delay)
  return(dd)
}


#' Observation process wrapper primary
#'
#'
#' @param dd Data frame with column *primary_underlying*
#' @param prop1 Initial proportion to observe (after *delay*)
#' @param prop2 Final proportion to observe
#' @param t_change_start Time (in *primary_underlying*) when proportion begins to change
#' @param duration_change Time taken for proportion to change from *prop1* to *prop2*
#' @param delay Delay between event and observation of event, i.e. between *primary_underlying* and *primary*
#' @param baseval Default 0. Value to use at the beggining of the observed TS (*primary*)
#' @details 
#' Wrapper for gen_grad_change_prop which eats a data frame as per schema in functions/FXN-REQS.md
#' @return Dataframe *dd* with added column *primary*
#' @export
#'
#' @examples
obs_grad_change_prim_D3 <- function(dd,
                                 prop1,
                                 prop2,
                                 t_change_start,
                                 duration_change,
                                 delay,
                                 baseval = 0){
  dd$primary <- gen_grad_change_prop(dd$primary_underlying,
                                     prop1 = prop1,
                                     prop2 = prop2,
                                     t_change_start = t_change_start,
                                     duration_change = duration_change,
                                     delay = delay,
                                     baseval = baseval)
  return(dd)
}

#' Observation process wrapper secondary
#'
#'
#' @param dd Data frame with column *secondary_underlying*
#' @param prop1 Initial proportion to observe (after *delay*)
#' @param prop2 Final proportion to observe
#' @param t_change_start Time (in *secondary_underlying*) when proportion begins to change
#' @param duration_change Time taken for proportion to change from *prop1* to *prop2*
#' @param delay Delay between event and observation of event, i.e. between *secondary_underlying* and *secondary*
#' @param baseval Default 0. Value to use at the beggining of the observed TS (*secondary*)
#' @details 
#' Wrapper for gen_grad_change_prop which eats a data frame as per schema in functions/FXN-REQS.md
#' @return Dataframe *dd* with added column *secondary*
#' @export
#'
#' @examples
obs_grad_change_sec_D4 <- function(dd, 
                                prop1,
                                prop2,
                                t_change_start,
                                duration_change,
                                delay,
                                baseval = 0){
  dd$secondary <- gen_grad_change_prop(dd$secondary_underlying,
                                       prop1 = prop1,
                                       prop2 = prop2,
                                       t_change_start = t_change_start,
                                       duration_change = duration_change,
                                       delay = delay,
                                       baseval = baseval)
  return(dd)

}
  
#Save the function definitions to file  
save.image(file = './synthetic/data/synth_data_functions.RData')
