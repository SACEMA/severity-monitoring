.args <- if (interactive()) {
  c(
    "./synthetic/data/synth_data_functions.RData",
    "./synthetic/inputs/scenario_1.json",
    "./synthetic/outputs/full/scenario_1.RData"
  )
} else {
  commandArgs(trailingOnly = TRUE)
}

# print(.args[1])
# print(.args[2])
# print(.args[3])

suppressPackageStartupMessages({
  library(tidyverse)
  library(EpiNow2)
  library(jsonlite)
})

load(.args[1])

scenario_params <- jsonlite::read_json(.args[2])

ts_len <- as.numeric(scenario_params$ts_len)
scenario_description <- scenario_params$scen_desc

# str(scenario_params$strain_2)

strain_1_params <- data.frame(scenario_params$strain_1) %>%
  mutate(across(.cols = everything(), .fns = ~as.numeric(.x)))

strain_2_params <- data.frame(scenario_params$strain_2) %>%
  mutate(across(.cols = everything(), .fns = ~as.numeric(.x)))

burnin_length = round(2 * (strain_1_params$mean_severe + strain_1_params$mean_severe_hosp))


##  create TS for strain1 including true and observed cases and admissions
## pipe functions from synth data functions to one another
dd_strain_1 <- generate_exponential_time_series(
  initial_value = strain_1_params$initial_incidence,
  rate = strain_1_params$exp_growth_rate,
  ts_length = ts_len,
  burn_length = burnin_length
  ) %>%
  expand_ts() %>%
  sample_outcomes(
    p_severe = strain_1_params$p_severe,
    p_hosp_if_severe = strain_1_params$p_hosp_if_severe,
    p_died_if_hosp = strain_1_params$p_died_if_hosp
  ) %>%
  sample_delays(
    mean_bg_test = strain_1_params$mean_bg_test,
    sd_bg_test = strain_1_params$sd_bg_test,
    rate_bg_hosp = strain_1_params$rate_bg_hosp,
    mean_hosp_test = strain_1_params$mean_hosp_test,
    sd_hosp_test = strain_1_params$sd_hosp_test,
    mean_severe = strain_1_params$mean_severe,
    sd_severe = strain_1_params$sd_severe,
    mean_severe_hosp = strain_1_params$mean_severe_hosp,
    sd_severe_hosp = strain_1_params$sd_severe_hosp,
    mean_hosp_died = strain_1_params$mean_hosp_died,
    sd_hosp_died = strain_1_params$sd_hosp_died,
    mean_resolve = strain_1_params$mean_resolve,
    sd_resolve = strain_1_params$sd_resolve
  ) %>%
  compute_event_times_from_delays() %>%
  compute_time_series_from_linelist() %>%
  filter(time <= (ts_len + burnin_length), time != 0) %>% 
  rename(
    primary = cases_observed,
    secondary = admissions,
    latent_primary = cases,
    latent_severe =  severe_cases
  ) %>%
  filter(time > burnin_length) %>%
  mutate(time = 1 : nrow(.))

##  create TS for strain2 including true and observed cases and admissions

dd_strain_2 <- generate_exponential_time_series(
  initial_value = strain_2_params$initial_incidence,
  rate = strain_2_params$exp_growth_rate,
  ts_length = ts_len,
  burn_length = burnin_length
  ) %>%
  expand_ts() %>%
  sample_outcomes(
    p_severe = strain_2_params$p_severe,
    p_hosp_if_severe = strain_2_params$p_hosp_if_severe,
    p_died_if_hosp = strain_2_params$p_died_if_hosp
  ) %>%
  sample_delays(
    mean_bg_test = strain_2_params$mean_bg_test,
    sd_bg_test = strain_2_params$sd_bg_test,
    rate_bg_hosp = strain_2_params$rate_bg_hosp,
    mean_hosp_test = strain_2_params$mean_hosp_test,
    sd_hosp_test = strain_2_params$sd_hosp_test,
    mean_severe = strain_2_params$mean_severe,
    sd_severe = strain_2_params$sd_severe,
    mean_severe_hosp = strain_2_params$mean_severe_hosp,
    sd_severe_hosp = strain_2_params$sd_severe_hosp,
    mean_hosp_died = strain_2_params$mean_hosp_died,
    sd_hosp_died = strain_2_params$sd_hosp_died,
    mean_resolve = strain_2_params$mean_resolve,
    sd_resolve = strain_2_params$sd_resolve
  ) %>%
  compute_event_times_from_delays() %>%
  compute_time_series_from_linelist() %>%
  filter(time <= (ts_len + burnin_length), time != 0) %>% # note fix this probably replace_na plus non-severe cases
  ## add TS_1 + TS_2 and save to RDS file.
  rename(
    primary = cases_observed,
    secondary = admissions,
    latent_primary = cases,
    latent_severe =  severe_cases
  )  %>%
  filter(time > burnin_length) %>%
  mutate(time = 1 : nrow(.))

ts_combined <- left_join(dd_strain_1, dd_strain_2,
                       by = "time",
                       suffix = c("_strain_1", "_strain_2")
) %>%
mutate(
  latent_primary = rowSums(
    select(., latent_primary_strain_1, latent_primary_strain_2), na.rm = TRUE
    ),
  latent_severe = rowSums(
    select(., latent_severe_strain_1, latent_severe_strain_2), na.rm= TRUE
    ),
  primary = rowSums(
    select(., primary_strain_1, primary_strain_2), na.rm = TRUE
    ),
  secondary = rowSums(
    select(.,secondary_strain_1, secondary_strain_2), na.rm = TRUE
    )
) %>%
select(time, latent_primary, latent_severe, primary, secondary)%>%
pivot_longer(cols = -c("time"))


if(interactive()){  
  ts_plot_combo <- ts_combined %>%
    ggplot(aes(x = time, y = value, color = name))+
    geom_line() +
    labs(title = scenario_description)
  print(ts_plot_combo) 
}


save(list= c("ts_combined", "dd_strain_1", "dd_strain_2"),
   file = tail(.args, 1))
  
    


## note: do we want to generate true primary in a stochastic fashion? YEs
## when do we want to start doing this?
