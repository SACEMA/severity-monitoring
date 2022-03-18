# Functions to synthesize test-tube incidence data for severity monitoring project
# March 2022
# JB, JA, CABP, JRCP


#' Generate a time series with the same incidence per time
#'
#' @param init_primary Initial number of cases
#' @param ts_length Desired length of the time series
#'
#' @return A vector of outcomes
#' @export
#'
#' @examples gen_flat_prim(50, 50)
gen_flat_prim <- function(init_primary, ts_length) {
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
gen_linear_prim <- function(init_primary,
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
gen_exp_prim <- function(init_primary,
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

