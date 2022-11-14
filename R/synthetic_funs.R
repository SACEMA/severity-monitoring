
c("EpiNow2", "data.table") |> .req()

.args <- .fromArgs(
  file.path("output", "synthetic", "synthetic_funs.rda")
)

# Helper functions (to be refactored)
find_first_time <- function(...) {
  res <- suppressWarnings(pmin(..., na.rm = TRUE))
  res[is.infinite(res)] <- NA
  res
}

remove_nonsense_times <- function(x, y) { fifelse(!is.na(x), y, NA_real_) }

sample_ts <- function(dt) { return(dt[, infections := rpois(.N, infections)]) }

expand_ts <- function(dt) {
  return(dt[,
     .(tmp = 1:infections),
     by=.(t0=time)
  ][, .(t0)])
}

#' @param ll linelist?
#' @param p_severe numeric on (0, 1), probability of severe
#' @param p_hosp_if_severe numeric on (0, 1), probability of hospitalization given severe
#' 
sample_outcomes <- function(
  ll, p_severe, p_hosp_if_severe,
  p_died_if_hosp, p_seek_test
) {
  return(ll[, c(
    "is_severe", "is_severe_hosp", "is_severe_hosp_died", "seek_test"
  ) := .(
    runif(.N) < p_severe,
    runif(.N) < p_hosp_if_severe,
    runif(.N) < p_died_if_hosp,
    runif(.N) < p_seek_test
  )][,
    is_severe_hosp := is_severe & is_severe_hosp
  ][,
    is_severe_hosp_died := is_severe_hosp & is_severe_hosp_died
  ])
}

optrlnorm <- function(n, mn, sd, tst = rep(TRUE, n)) fifelse(tst, rlnorm(
  n, meanlog = convert_to_logmean(mn, sd),
  sdlog = convert_to_logsd(mn, sd)
), NA_real_)

sample_delays <- function(dt,
    mean_bg_test, sd_bg_test,
    rate_bg_hosp,
    mean_hosp_test, sd_hosp_test,
    mean_severe, sd_severe,
    mean_severe_hosp, sd_severe_hosp,
    mean_hosp_died, sd_hosp_died,
    mean_resolve, sd_resolve,
    mean_seek_test, sd_seek_test
) {

  # TODO DRY this out; suggest more structure to parameters?
  # have it introspect by names, etc?
  dt[, delay_bg_test := optrlnorm(
    .N, mean_bg_test, sd_bg_test
  ) ][,
      delay_bg_hosp := rexp(.N, rate = rate_bg_hosp)
  ][, delay_bg_hosp_test := optrlnorm(
      .N, mean_hosp_test, sd_hosp_test
  )][, delay_severe := optrlnorm(
    .N, mean_severe, sd_severe, is_severe
  )][, delay_severe_hosp := optrlnorm(
     .N, mean_severe_hosp, sd_severe_hosp, is_severe_hosp
  )][, delay_severe_hosp_test := optrlnorm(
    .N, mean_hosp_test, sd_hosp_test, is_severe_hosp
  )][, delay_severe_death := optrlnorm(
    .N, mean_hosp_died, sd_hosp_died, is_severe_hosp_died
  )][, delay_resolve := optrlnorm(
      .N, mean_resolve, sd_resolve
  )][, delay_seek_test := optrlnorm(
    .N, mean_seek_test, sd_seek_test, seek_test
  )]
  
  return(dt[, .SD, .SDcols = -c("is_severe", "is_severe_hosp", "is_severe_hosp_died", "seek_test")])
}

compute_event_times_from_delays <- function(delays_df) { # Note: this function assumed non-existent delays are NA's.
  # add unit test here or for prev function to ensure non-existent delays are represented by NA's
  delays_df[,c(
      "time_bg_test", "time_seek_test", "time_bg_hosp", 
      "time_severe",  "time_resolve"
    ) := .(
      t0 +delay_bg_test, t0 +delay_seek_test, t0 +delay_bg_hosp,
      t0 +delay_severe, t0 +delay_resolve
    )][, c(
      "time_bg_hosp_test", "time_severe_hosp"
    ) := .(
      time_bg_hosp + delay_bg_hosp_test,
      time_severe + delay_severe_hosp
    )][, c("time_severe_hosp_test", "time_severe_death") := .(
      time_severe_hosp +delay_severe_hosp_test,
      time_severe_hosp +delay_severe_death
   )]
  
   delays_df[time_bg_test > time_resolve, time_bg_test := NA]
   delays_df[time_bg_hosp_test > time_resolve, time_bg_hosp_test := NA]
   delays_df[time_severe_hosp_test > time_resolve, time_severe_hosp_test := NA]
   delays_df[time_seek_test > time_resolve, time_seek_test := NA]

   delays_df[,c(
     "time_obs_case", "time_severe_hosp_obs", "time_bg_hosp_obs"
   ) := .(
     find_first_time(time_bg_test, time_bg_hosp_test, time_severe_hosp_test, time_seek_test),
     remove_nonsense_times(time_severe_hosp_test, time_severe_hosp),
     remove_nonsense_times(time_bg_hosp_test, time_bg_hosp)
  )][,
     time_admission := find_first_time(time_severe_hosp_obs, time_bg_hosp_obs)
  ]
  
  delays_df[,
    grep("time_", names(delays_df), value = TRUE) := lapply(.SD, floor),
    .SDcols = patterns("time_")
  ]
  
  return(delays_df[, .SD, .SDcols = patterns("t0|case|time_")])
}

compute_time_series_from_linelist <- function(times_df) {
  tmax <- max(times_df$t0)
  
  time_series_cases <- times_df[!is.na(t0), .(cases = .N), keyby=.(time = t0)]

  time_series_severe <- times_df[!is.na(time_severe),.(severe_cases = .N), keyby=.(time = time_severe)]

  time_series_first_positive_tests <- times_df[!is.na(time_obs_case),.(
    cases_observed = .N
  ), keyby=.(time = time_obs_case)]

  time_series_hospitalisations <- times_df[!is.na(time_admission),.(
    admissions = .N
  ), keyby=.(time = time_admission)]

  time_series <- merge(
    time_series_cases,
    time_series_severe,
    by = "time",
    all = TRUE, 
  ) |> merge(
    time_series_first_positive_tests,
    by = "time",
      all = TRUE
  ) |> merge(
    time_series_hospitalisations,
    by = "time",
    all = TRUE
  )
  
  time_series[, grep("time", names(time_series), value = TRUE, invert = TRUE) := lapply(.SD, nafill, fill = 0), .SDcols = -c("time")]

  return(time_series)
}


# ts must have colums tie and infections
generate_exponential_time_series <- function(
  initial_value,
  ts_length,
  rate,
  burn_length
) {
  burn_init <- initial_value * exp(-1 * rate * (burn_length + 1))
  return(data.table(
    time = 1:(ts_length + burn_length)
  )[,
    infections := round(burn_init * exp(rate*time))
  ])
}

save(list=ls(), file = tail(.args, 1))

