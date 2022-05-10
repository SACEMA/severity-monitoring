library(tidyverse)
ts_len <- 100
ts_tmp <- data.frame(time = 1:ts_len, infections = rep(10,ts_len))

set.seed(123410000)

p1 = 0.1
p2 = 0.5
p3 = 0.2

ts_to_ll <- function(ts){
  ts_len <- nrow(ts)
  t0s <- c()
  for(timestep in 1:ts_len){
    t0s <- c(t0s, rep(ts$time[timestep], ts$infections[timestep]))
  }
  ll <- data.frame(id = 1:length(t0s), t0 = t0s)
  return(ll)
}

ts_to_ll(ts_tmp)

ll_tilde_to_conditional_ll <- function(ll,
                                 p_severe,
                                 p_hosp_if_severe,
                                 p_died_if_hosp){
  ll <- (ll
         %>% rowwise()
         %>% mutate(is_severe = rbinom(1, 1, p_severe))
         %>% mutate(is_severe_hosp = ifelse(is_severe, rbinom(1, 1, p_hosp_if_severe), 0))
         %>% mutate(is_severe_hosp_died = ifelse(is_severe_hosp, rbinom(1, 1, p_died_if_hosp), 0))
         %>% ungroup()
         )
  
  return(ll)
}

ll <- ts_to_ll(ts_tmp) %>%  ll_tilde_to_conditional_ll(p1, p2, p3)

delayparms <- list(mean_bg_test = log(5), sd_bg_test = log(5),
                   rate_bg_hosp = log(0.05),
                   mean_hosp_test = log(2), sd_hosp_test = log(2),
                   mean_severe = log(5), sd_severe = log(5),
                   mean_severe_hosp = log(2), sd_severe_hosp = log(2),
                   mean_hosp_died = log(10), sd_hosp_died = log(10),
                   mean_resolve = log(12), sd_resolve = log(5))

minimal_linelist_to_delays <- function(infections,
                          mean_bg_test = log(5), sd_bg_test = log(5),
                          rate_bg_hosp = 0.05,
                          mean_hosp_test = log(2), sd_hosp_test = log(2),
                          mean_severe = log(5), sd_severe = log(5),
                          mean_severe_hosp = log(2), sd_severe_hosp = log(2),
                          mean_hosp_died = log(10), sd_hosp_died = log(10),
                          mean_resolve = log(12), sd_resolve = log(5)){
  # we need to parameterise 7 lognormals and 1 exponential delay distribution
  # we need 6 pairs of meanlog and sdlog
  # and 1 exponential rate
  n_case <- nrow(ll)
  obs <- infections %>%
    mutate(
      delay_bg_test = rlnorm(
        n = n_case,
        meanlog = mean_bg_test,
        sdlog = sd_bg_test
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
        meanlog = mean_hosp_test,
        sdlog = sd_hosp_test
        )
      ) %>%
    mutate(
      delay_severe = rlnorm(
        n = n_case,
        meanlog = mean_severe,
        sdlog = sd_severe
        )
      ) %>%
    mutate(
      delay_severe_hosp = rlnorm(
        n = n_case,
        meanlog = mean_severe_hosp,
        sdlog = sd_severe_hosp
        )
      ) %>% 
    mutate(
      delay_severe_hosp_test = rlnorm(
        n = n_case,
        meanlog = mean_hosp_test,
        sdlog = sd_hosp_test
        )
      ) %>%
    mutate(
      delay_severe_death = rlnorm(
        n = n_case,
        meanlog = mean_hosp_died,
        sdlog = sd_hosp_died
        )
      ) %>% 
    mutate(
      delay_resolve = rlnorm(
        n = n_case,
        meanlog = mean_resolve,
        sdlog = sd_resolve
        )
      )
  
  obs <- obs %>%
    mutate(
      delay_severe = ifelse(is_severe, delay_severe, NA),
      delay_severe_hosp = ifelse(is_severe_hosp, delay_severe_hosp, NA),
      delay_severe_hosp_test = ifelse(is_severe_hosp, delay_severe_hosp_test, NA),
      delay_severe_death = ifelse(is_severe_hosp_died, delay_severe_death, NA)
    ) %>%
    select(
      c(-is_severe, -is_severe_hosp, -is_severe_hosp_died)
    )
  }

names(minimal_linelist_to_delays(ll))

delays <- minimal_linelist_to_delays(ll)


delays_to_times <- function(delays_df){ #Note: this function assumed non-existent delays are NA's.
  # add unit test here or for prev function to ensure non-existent delays are represented by NA's
  times_df <- delays_df %>%
    mutate(
      time_bg_test = t0 + delay_bg_test,
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
      #delay_bg_test, delay_bg_hosp, delay_bg_hosp_test, delay_severe, delay_severe_hosp,
       #  delay_severe_hosp_test, delay_severe_death, delay_resolve)
    ) %>%
    mutate( # note: we are setting times of negative tests (time_test > time_resolve) to NA
      time_bg_test = ifelse(time_bg_test <= time_resolve,
                            time_bg_test, NA),
      time_bg_hosp_test = ifelse(time_bg_hosp_test <= time_resolve,
                                 time_bg_hosp_test, NA),
      time_severe_hosp_test = ifelse(time_severe_hosp_test <= time_resolve,
                                     time_severe_hosp_test, NA)
    ) %>%
    rowwise() %>%
    mutate(
      time_obs_case = ifelse( 
        is.na(time_bg_test) & is.na(time_bg_hosp_test) & is.na(time_severe_hosp_test),
        NA,
        min(time_bg_test, time_bg_hosp_test, time_severe_hosp_test, na.rm = T)
        )
    ) %>%
    mutate( # we are keeping the redundant time_severe_hosp column so that we can do sanity checks on it later
      time_severe_hosp_obs = ifelse(!is.na(time_severe_hosp_test),
                                time_severe_hosp,
                                NA),
      time_bg_hosp_obs = ifelse(!is.na(time_bg_hosp_test),
                                time_bg_hosp,
                                NA)
    ) %>%
    mutate(
      time_admission = ifelse(
        is.na(time_severe_hosp_obs) & is.na(time_bg_hosp_obs),
        NA,
        min(time_severe_hosp_obs, time_bg_hosp_obs, na.rm = T)
      )
    ) %>%
     ungroup() %>%
    mutate(
      across(.cols = starts_with("time_"), .fns = floor)
    )
  return(times_df)
}

times <- delays_to_times(delays)
View(times)

times_of_events_to_time_series <- function(times_df){
  
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
    by = 'time',
    all.x = T,
    all.y = T
    ) %>%
    merge(
      time_series_first_positive_tests,
      by = 'time',
      all.x = T,
      all.y = T
      ) %>%
    merge(
      time_series_hospitalisations,
      by = 'time',
      all.x = T,
      all.y = T
    )
  
  time_series <- time_series %>%
    mutate(across(.cols = everything(), .fns = replace_na, replace = 0))
  
  return(time_series)
}

View(times_of_events_to_time_series(times))

# note: we need a rule for if someone is admitted via background process as well as because they are severe

