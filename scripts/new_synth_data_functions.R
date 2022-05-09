
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

infs_to_obs <- function(infections,
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
      d_bg_hosp = rexp(
        n = n_case,
        rate = rate_bg_hosp
        )
      ) %>%
    mutate(
      d_bg_hosp_test = rlnorm(
        n = n_case,
        meanlog = mean_hosp_test,
        sdlog = sd_hosp_test
        )
      ) %>%
    mutate(
      d_severe = rlnorm(
        n = n_case,
        meanlog = mean_severe,
        sdlog = sd_severe
        )
      ) %>%
    mutate(
      d_severe_hosp = rlnorm(
        n = n_case,
        meanlog = mean_severe_hosp,
        sdlog = sd_severe_hosp
        )
      ) %>% 
    mutate(
      d_severe_hosp_test = rlnorm(
        n = n_case,
        meanlog = mean_hosp_test,
        sdlog = sd_hosp_test
        )
      ) %>%
    mutate(
      d_severe_death = rlnorm(
        n = n_case,
        meanlog = mean_hosp_died,
        sdlog = sd_hosp_died
        )
      ) %>% 
    mutate(
      d_resolve = rlnorm(
        n = n_case,
        meanlog = mean_resolve,
        sdlog = sd_resolve
        )
      )
  
  obs <- obs %>%
    mutate(
      d_severe = ifelse(is_severe, d_severe, NA),
      d_severe_hosp = ifelse(is_severe_hosp, d_severe_hosp, NA),
      d_severe_hosp_test = ifelse(is_severe_hosp, d_severe_hosp_test, NA),
      d_severe_death = ifelse(is_severe_hosp_died, d_severe_death, NA)
    ) 
  }



View(infs_to_obs(ll))


