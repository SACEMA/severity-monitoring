.args <- if (interactive()) {
  c(
    "./synthetic/data/synth_data_functions.RData",
    "./synthetic/data/demo_params.RData",
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
})

load(.args[1])
load(.args[2])


#Global control params; We could put these in a separate global_params.RData creation script
ts_len <- 80
plot_synth_data <- TRUE

#Get the scenario number
scenario_label <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(.args[3])) %>%
  sub(pattern = "scenario_", replacement = "") #' REMARK: if this is only to extract the 
#' scenario number from .args[[3]], we could simply do <str_extract(basename(.args[3]), '\\d+')> but
#' I guess this will change depending on how we decide to name the scenarios

params <- param_table %>% filter(scenario == scenario_label)

#Create the ts for each strain separately
with(params, {
  ## common params
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
      p_hosp_if_severe = p_hosp_if_severe_strain_1,
      p_died_if_hosp = p_died_if_hosp_strain_1
    ) %>%
    sample_delays(
      mean_bg_test = mean_bg_test_strain_1,
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
    filter(time <= ts_len, time != 0)

  ##  create TS for strain2 including true and observed cases and admissions

  dd_strain_2 <- generate_exponential_time_series(
    initial_value = initial_incidence_strain_2,
    rate = exp_growth_rate_strain_2,
    ts_length = ts_len
  ) %>%
    expand_ts() %>%
    sample_outcomes(
      p_severe = p_severe_strain_2,
      p_hosp_if_severe = p_hosp_if_severe_strain_2,
      p_died_if_hosp = p_died_if_hosp_strain_2
    ) %>%
    sample_delays(
      mean_bg_test = mean_bg_test_strain_2,
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
    compute_time_series_from_linelist() %>%
    filter(time <= ts_len, time != 0) # note fix this probably replace_na plus non-severe cases
  ## add TS_1 + TS_2 and save to RDS file.
  
  
  ts_combined <- left_join(dd_strain_1, dd_strain_2,
                           by = "time",
                           suffix = c("_strain_1", "_strain_2")
  ) %>%
    mutate(
      latent_primary = cases_strain_1 + cases_strain_2,
      latent_severe = severe_cases_strain_1 + severe_cases_strain_2,
      primary = cases_observed_strain_1 + cases_observed_strain_2,
      secondary = admissions_strain_1 + admissions_strain_2
    ) %>%
    select(time, latent_primary, latent_severe, primary, secondary)%>%
    pivot_longer(cols = -c("time"))

  
    if(plot_synth_data){  
      ts_plot <- ts_combined %>%
        ggplot(aes(x = time, y = value, color = name))+
        geom_line()
      print(ts_plot)
    }
    
    
    save(list= c("ts_combined", "dd_strain_1", "dd_strain_2"),
       file = tail(.args, 1))
  
    
    
})




## note: do we want to generate true primary in a stochastic fashion? YEs
## when do we want to start doing this?
