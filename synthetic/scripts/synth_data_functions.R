.args <- if(interactive()){
  c("./synthetic/data/synth_data_functions.RData")
}else{
  commandArgs(trailingOnly = TRUE)
}


suppressPackageStartupMessages({
  library(tidyverse)
  library(EpiNow2)
})

# Helper functions (to be refactored)
find_first_obs_time <- function(...) {
  ifelse(all(is.na(c(...))), NA, min(..., na.rm = T))
}

find_hosp_admission_time <- function(x, y) {
  ifelse(is.na(x) & is.na(y), NA, min(x, y, na.rm = T))
}

remove_nonsense_times <- function(x, y) {
  ifelse(!is.na(x), y, NA)
}

expand_ts <- function(ts) {
  minimal_linelist <- ts %>%
    group_by(time) %>%
    uncount(infections, .remove = TRUE) %>%
    mutate(case = row_number()) %>%
    rename(t0 = time) %>%
    ungroup()
  return(minimal_linelist)
}

sample_outcomes <- function(ll,
                            p_severe,
                            p_hosp_if_severe,
                            p_died_if_hosp,
                            p_seek_test) {
  obs_size <- nrow(ll)
  ll_outcomes <- ll %>%
    mutate(
      is_severe = rbinom(obs_size, 1, p_severe),
      is_severe_hosp = rbinom(obs_size, 1, p_hosp_if_severe),
      is_severe_hosp_died = rbinom(obs_size, 1, p_died_if_hosp),
      seek_test = rbinom(obs_size, 1, p_seek_test)
    ) %>%
    mutate(
      is_severe_hosp = is_severe * is_severe_hosp,
      is_severe_hosp_died = is_severe_hosp * is_severe_hosp_died
    )

  return(ll_outcomes)
}

sample_delays <- function(infections, mean_bg_test, sd_bg_test,
                          rate_bg_hosp,
                          mean_hosp_test, sd_hosp_test,
                          mean_severe, sd_severe,
                          mean_severe_hosp, sd_severe_hosp,
                          mean_hosp_died, sd_hosp_died,
                          mean_resolve, sd_resolve,
                          mean_seek_test, sd_seek_test
                          ) {
  # we need to parameterise 7 lognormals and 1 exponential delay distribution
  # we need 6 pairs of meanlog and sdlog
  # and 1 exponential rate
  n_case <- nrow(infections)
  obs <- infections %>%
    mutate(
      delay_bg_test = rlnorm(
        n = n_case,
        meanlog = convert_to_logmean(mean_bg_test, sd_bg_test),
        sdlog = convert_to_logsd(mean_bg_test, sd_bg_test)
      )
    ) %>%
    mutate(
      delay_bg_hosp = rexp(
        n = n_case,
        rate = rate_bg_hosp
      )
    ) %>%
    mutate(
      delay_bg_hosp_test = rlnorm(
        n = n_case,
        meanlog = convert_to_logmean(mean_hosp_test, sd_hosp_test),
        sdlog = convert_to_logsd(mean_hosp_test, sd_hosp_test)
      )
    ) %>%
    mutate(
      delay_severe = rlnorm(
        n = n_case,
        meanlog = convert_to_logmean(mean_severe, sd_severe),
        sdlog = convert_to_logsd(mean_severe, sd_severe)
      )
    ) %>%
    mutate(
      delay_severe_hosp = rlnorm(
        n = n_case,
        meanlog = convert_to_logmean(mean_severe_hosp, sd_severe_hosp),
        sdlog = convert_to_logmean(mean_severe_hosp, sd_severe_hosp)
      )
    ) %>%
    mutate(
      delay_severe_hosp_test = rlnorm(
        n = n_case,
        meanlog = convert_to_logmean(mean_hosp_test, sd_hosp_test),
        sdlog = convert_to_logsd(mean_hosp_test, sd_hosp_test)
      )
    ) %>%
    mutate(
      delay_severe_death = rlnorm(
        n = n_case,
        meanlog = convert_to_logmean(mean_hosp_died, sd_hosp_died),
        sdlog = convert_to_logsd(mean_hosp_died, sd_hosp_died)
      )
    ) %>%
    mutate(
      delay_resolve = rlnorm(
        n = n_case,
        meanlog = convert_to_logmean(mean_resolve, sd_resolve),
        sdlog = convert_to_logsd(mean_resolve, sd_resolve)
      )
    ) %>%
    mutate(
      delay_seek_test = rlnorm(
        n = n_case,
        meanlog = convert_to_logmean(mean_seek_test, sd_seek_test),
        sdlog = convert_to_logsd(mean_seek_test, sd_seek_test)
      )
    )

  obs <- obs %>%
    mutate(
      delay_severe = ifelse(is_severe, delay_severe, NA),
      delay_severe_hosp = ifelse(is_severe_hosp, delay_severe_hosp, NA),
      delay_severe_hosp_test = ifelse(is_severe_hosp, delay_severe_hosp_test, NA),
      delay_severe_death = ifelse(is_severe_hosp_died, delay_severe_death, NA),
      delay_seek_test = ifelse(seek_test, delay_seek_test, NA)
    ) %>%
    select(
      c(-is_severe, -is_severe_hosp, -is_severe_hosp_died, -seek_test)
    )
}

compute_event_times_from_delays <- function(delays_df) { # Note: this function assumed non-existent delays are NA's.
  # add unit test here or for prev function to ensure non-existent delays are represented by NA's
  times_df <- delays_df %>%
    mutate(
      time_bg_test = t0 + delay_bg_test,
      time_seek_test = t0 + delay_seek_test,
      time_bg_hosp = t0 + delay_bg_hosp,
      time_bg_hosp_test = time_bg_hosp + delay_bg_hosp_test,
      time_severe = t0 + delay_severe,
      time_severe_hosp = time_severe + delay_severe_hosp,
      time_severe_hosp_test = time_severe_hosp + delay_severe_hosp_test,
      time_severe_death = time_severe_hosp + delay_severe_death,
      time_resolve = t0 + delay_resolve
    ) %>%
    select(
      -c(starts_with("delay_"))
    ) %>%
    mutate( # note: we are setting times of negative tests (time_test > time_resolve) to NA
      time_bg_test = ifelse(time_bg_test <= time_resolve,
        time_bg_test, NA
      ),
      time_bg_hosp_test = ifelse(time_bg_hosp_test <= time_resolve,
        time_bg_hosp_test, NA
      ),
      time_severe_hosp_test = ifelse(time_severe_hosp_test <= time_resolve,
        time_severe_hosp_test, NA
      ),
      time_seek_test = ifelse(time_seek_test <= time_resolve,
                              time_seek_test, NA)
    ) %>%
    mutate(
      time_obs_case = pmap_dbl(list(time_bg_test, time_bg_hosp_test, time_severe_hosp_test, time_seek_test), find_first_obs_time),
      time_severe_hosp_obs = pmap_dbl(list(time_severe_hosp_test, time_severe_hosp), remove_nonsense_times),
      time_bg_hosp_obs = pmap_dbl(list(time_bg_hosp_test, time_bg_hosp), remove_nonsense_times),
      time_admission = pmap_dbl(list(time_severe_hosp_obs, time_bg_hosp_obs), find_hosp_admission_time)
    ) %>%
    mutate(
      across(.cols = starts_with("time_"), .fns = floor)
    )
  return(times_df)
}

compute_time_series_from_linelist <- function(times_df) {
  tmax <- max(times_df$t0)

  time_series_cases <- times_df %>%
    group_by(t0) %>%
    summarise(cases = n(), .groups = "drop") %>% # by date of onset of test positivity
    rename(time = t0)

  time_series_severe <- times_df %>%
    group_by(time_severe) %>%
    summarise(severe_cases = n(), .groups = "drop") %>% # by date of onset of severe symptoms
    rename(time = time_severe)

  time_series_first_positive_tests <- times_df %>%
    group_by(time_obs_case) %>%
    summarise(cases_observed = n(), .groups = "drop") %>%
    rename(time = time_obs_case)

  time_series_hospitalisations <- times_df %>%
    group_by(time_admission) %>%
    summarise(admissions = n(), .groups = "drop") %>%
    rename(time = time_admission)

  time_series <- merge(
    time_series_cases,
    time_series_severe,
    by = "time",
    all.x = T,
    all.y = T
  ) %>%
    merge(
      time_series_first_positive_tests,
      by = "time",
      all.x = T,
      all.y = T
    ) %>%
    merge(
      time_series_hospitalisations,
      by = "time",
      all.x = T,
      all.y = T
    )

#   time_series <- time_series %>%
#     mutate(across(.cols = everything(), .fns = replace_na, replace = 0))

  return(time_series)
}


# ts must have colums tie and infections
generate_exponential_time_series <- function(initial_value,
         ts_length,
         rate,
         burn_length) {
  burn_init <- initial_value * exp(-1 * rate * (burn_length + 1))
  ts_out  <- data.frame(time = 1:(ts_length + burn_length)) %>%
      mutate(infections = c((burn_init * exp(rate*time)))) %>%
      mutate(infections = ifelse(infections<1, ceiling(infections), round(infections)))
  return(ts_out)
}


save(list=ls(), file = tail(.args, 1))

