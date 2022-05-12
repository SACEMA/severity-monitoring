## common params
ts_len <- 20

## params strain1

initial_incidence_strain_1 = 100
exp_growth_rate_strain_1 = -0.0005
p_severe_strain_1 = 0.05
p_hosp_if_severe_strain_1 = 0.5 
p_died_if_hosp_strain_1 = 0.1
mean_bg_test_strain_1 = log(5)
sd_bg_test_strain_1 = log(5)
rate_bg_hosp_strain_1 = 0.05
mean_hosp_test_strain_1 = log(3)
sd_hosp_test_strain_1 = log(3)
mean_severe_strain_1 = log(7)
sd_severe_strain_1 = log(7)
mean_severe_hosp_strain_1 = log(2)
sd_severe_hosp_strain_1 = log(2)
mean_hosp_died_strain_1 = log(7)
sd_hosp_died_strain_1 = log(7)
mean_resolve_strain_1 = log(14)
sd_resolve_strain_1 = log(14)


## params strain2

initial_incidence_strain_2 = 1
exp_growth_rate_strain_2 = 0.05
p_severe_strain_2 = 0.1
p_hosp_if_severe_strain_2 = p_hosp_if_severe_strain_1 
p_died_if_hosp_strain_2 = 0.2
mean_bg_test_strain_2 = mean_bg_test_strain_1 
sd_bg_test_strain_2 = sd_bg_test_strain_1
rate_bg_hosp_strain_2 = rate_bg_hosp_strain_1
mean_hosp_test_strain_2 = mean_hosp_test_strain_1 
sd_hosp_test_strain_2 = sd_hosp_test_strain_1
mean_severe_strain_2 = mean_severe_strain_1
sd_severe_strain_2 = sd_severe_strain_1
mean_severe_hosp_strain_2 = mean_severe_hosp_strain_1
sd_severe_hosp_strain_2 = sd_severe_hosp_strain_1
mean_hosp_died_strain_2 = mean_hosp_died_strain_1
sd_hosp_died_strain_2 = sd_hosp_died_strain_1
mean_resolve_strain_2 = mean_resolve_strain_1
sd_resolve_strain_2 = sd_resolve_strain_1




# exp(exp_growth_rate_strain_2*150)

##  create TS for strain1 including true and observed cases and admissions
## pipe functions from synth data functions to one another

dd_strain_1 <- generate_exponential_time_series(
  initial_value = initial_incidence_strain_1,
  rate = exp_growth_rate_strain_1,
  ts_length = ts_len 
    ) %>%
  expand_ts() %>%
  sample_outcomes(
    p_severe = p_severe_strain_1,
    p_hosp_if_severe = p_hosp_if_severe_strain_1 ,
    p_died_if_hosp = p_died_if_hosp_strain_1
                  ) %>%
  sample_delays(mean_bg_test = mean_bg_test_strain_1,
                sd_bg_test = sd_bg_test_strain_1,
                rate_bg_hosp = rate_bg_hosp_strain_1,
                mean_hosp_test = mean_hosp_test_strain_1,
                sd_hosp_test = sd_hosp_test_strain_1,
                mean_severe = mean_severe_strain_1,
                sd_severe = sd_severe_strain_1,
                mean_severe_hosp = mean_severe_hosp_strain_1,
                sd_severe_hosp = sd_severe_hosp_strain_1,
                mean_hosp_died = mean_hosp_died_strain_1,
                sd_hosp_died = sd_hosp_died_strain_1,
                mean_resolve = mean_resolve_strain_1,
                sd_resolve = sd_resolve_strain_1
  ) %>%
  compute_event_times_from_delays() %>%
  compute_time_series_from_linelist() %>%
  filter(time<= ts_len, time != 0)

##  create TS for strain2 including true and observed cases and admissions

dd_strain_2 <- generate_exponential_time_series(
  initial_value = initial_incidence_strain_2,
  rate = exp_growth_rate_strain_2,
  ts_length = ts_len 
) %>%
  expand_ts() %>%
  sample_outcomes(
    p_severe = p_severe_strain_2,
    p_hosp_if_severe = p_hosp_if_severe_strain_2 ,
    p_died_if_hosp = p_died_if_hosp_strain_2
  ) %>%
  sample_delays(mean_bg_test = mean_bg_test_strain_2,
                sd_bg_test = sd_bg_test_strain_2,
                rate_bg_hosp = rate_bg_hosp_strain_2,
                mean_hosp_test = mean_hosp_test_strain_2,
                sd_hosp_test = sd_hosp_test_strain_2,
                mean_severe = mean_severe_strain_2,
                sd_severe = sd_severe_strain_2,
                mean_severe_hosp = mean_severe_hosp_strain_2,
                sd_severe_hosp = sd_severe_hosp_strain_2,
                mean_hosp_died = mean_hosp_died_strain_2,
                sd_hosp_died = sd_hosp_died_strain_2,
                mean_resolve = mean_resolve_strain_2,
                sd_resolve = sd_resolve_strain_2
  ) %>%
  compute_event_times_from_delays() %>%
  compute_time_series_from_linelist()%>%
  filter(time<= ts_len, time != 0) # note fix this probably replace_na plus non-severe cases
## add TS_1 + TS_2 and save to RDS file.

ts_combined <- left_join(dd_strain_1, dd_strain_2, by = 'time') %>%
  mutate(latent_primary = cases.x + cases.y,
         latent_severe = severe_cases.x + severe_cases.y,
         primary = cases_observed.x + cases_observed.y,
         secondary = admissions.x + admissions.y
  )  %>%
  select(time, latent_primary, latent_severe, primary, secondary)%>%
  pivot_longer(cols = -c('time'))

ts_combined %>%
  ggplot(aes(x = time, y = value, color = name))+
  geom_line()
## note: do we want to generate true primary in a stochastic fashion? YEs
## when do we want to start doing this?


# ---------------------------------------------------------------------------- #

# ts_len <- 1E2
# ts_tmp <- data.frame(time = 1:ts_len, infections = rep(1E5, ts_len))
# 
# set.seed(123410000)
# 
# p1 <- 0.1
# p2 <- 0.5
# p3 <- 0.2
# 
# df <- expand_ts(ts_tmp)
# df %>% head()
# 
# ll <- expand_ts(ts_tmp) %>% sample_outcomes(p1, p2, p3)
# 
# delays_df <- sample_delays(ll)
# 
# View(delays_df)
# 
# times_df <- compute_event_times_from_delays(delays)
# times_df |> View()
# 
# 
# compute_time_series_from_linelist(times_df) %>% View()
#   