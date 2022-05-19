.args <- if(interactive()){
  c('./synthetic/data/demo_params.RData')
}else{
  commandArgs(trailingOnly = TRUE)
}


suppressPackageStartupMessages({
  library(tidyverse)
})

scenario_definitions <-
  tribble(
    ~scenario, ~strain, ~parameter, ~value,
    1, "strain_1", "initial_incidence", "100",
    1, "strain_1", "exp_growth_rate", "-0.0005",
    1, "strain_1", "p_severe", "0.05",
    1, "strain_1", "p_hosp_if_severe", "0.5",
    1, "strain_1", "p_died_if_hosp", "0.1",
    1, "strain_1", "mean_bg_test", "5",
    1, "strain_1", "sd_bg_test", "5",
    1, "strain_1", "rate_bg_hosp", "0.05",
    1, "strain_1", "mean_hosp_test", "2",
    1, "strain_1", "sd_hosp_test", "2",
    1, "strain_1", "mean_severe", "5",
    1, "strain_1", "sd_severe", "5",
    1, "strain_1", "mean_severe_hosp", "2",
    1, "strain_1", "sd_severe_hosp", "2",
    1, "strain_1", "mean_hosp_died", "10",
    1, "strain_1", "sd_hosp_died", "10",
    1, "strain_1", "mean_resolve", "5",
    1, "strain_1", "sd_resolve", "5",
    1, "strain_2", "initial_incidence", "1",
    1, "strain_2", "exp_growth_rate", "0.05",
    1, "strain_2", "p_severe", "0.1",
    1, "strain_2", "p_hosp_if_severe", "s1",
    1, "strain_2", "p_died_if_hosp", "0.2",
    1, "strain_2", "mean_bg_test", "s1",
    1, "strain_2", "sd_bg_test", "s1",
    1, "strain_2", "rate_bg_hosp", "s1",
    1, "strain_2", "mean_hosp_test", "s1",
    1, "strain_2", "sd_hosp_test", "s1",
    1, "strain_2", "mean_severe", "s1",
    1, "strain_2", "sd_severe", "s1",
    1, "strain_2", "mean_severe_hosp", "s1",
    1, "strain_2", "sd_severe_hosp", "s1",
    1, "strain_2", "mean_hosp_died", "s1",
    1, "strain_2", "sd_hosp_died", "s1",
    1, "strain_2", "mean_resolve", "s1",
    1, "strain_2", "sd_resolve", "s1"
  )

param_table_wide <- scenario_definitions %>%
  relocate(parameter, .before = strain) %>%
  unite("param", parameter:strain) %>%
  pivot_wider(id_cols = scenario, names_from = param, values_from = value)


param_table <- param_table_wide %>%
  mutate(
    p_severe_strain_2 = ifelse(p_severe_strain_2 == "s1", p_severe_strain_1, p_severe_strain_2),
    p_hosp_if_severe_strain_2 = ifelse(p_hosp_if_severe_strain_2 == "s1", p_hosp_if_severe_strain_1, p_hosp_if_severe_strain_2),
    p_died_if_hosp_strain_2 = ifelse(p_died_if_hosp_strain_2 == "s1", p_died_if_hosp_strain_1, p_died_if_hosp_strain_2),
    mean_bg_test_strain_2 = ifelse(mean_bg_test_strain_2 == "s1", mean_bg_test_strain_1, mean_bg_test_strain_2),
    sd_bg_test_strain_2 = ifelse(sd_bg_test_strain_2 == "s1", sd_bg_test_strain_1, sd_bg_test_strain_2),
    rate_bg_hosp_strain_2 = ifelse(rate_bg_hosp_strain_2 == "s1", rate_bg_hosp_strain_1, rate_bg_hosp_strain_2),
    mean_hosp_test_strain_2 = ifelse(mean_hosp_test_strain_2 == "s1", mean_hosp_test_strain_1, mean_hosp_test_strain_2),
    sd_hosp_test_strain_2 = ifelse(sd_hosp_test_strain_2 == "s1", sd_hosp_test_strain_1, sd_hosp_test_strain_2),
    mean_severe_strain_2 = ifelse(mean_severe_strain_2 == "s1", mean_severe_strain_1, mean_severe_strain_2),
    sd_severe_strain_2 = ifelse(sd_severe_strain_2 == "s1", sd_severe_strain_1, sd_severe_strain_2),
    mean_severe_hosp_strain_2 = ifelse(mean_severe_hosp_strain_2 == "s1", mean_severe_hosp_strain_1, mean_severe_hosp_strain_2),
    sd_severe_hosp_strain_2 = ifelse(sd_severe_hosp_strain_2 == "s1", sd_severe_hosp_strain_1, sd_severe_hosp_strain_2),
    mean_hosp_died_strain_2 = ifelse(mean_hosp_died_strain_2 == "s1", mean_hosp_died_strain_1, mean_hosp_died_strain_2),
    sd_hosp_died_strain_2 = ifelse(sd_hosp_died_strain_2 == "s1", sd_hosp_died_strain_1, sd_hosp_died_strain_2),
    mean_resolve_strain_2 = ifelse(mean_resolve_strain_2 == "s1", mean_resolve_strain_1, mean_resolve_strain_2),
    sd_resolve_strain_2 = ifelse(sd_resolve_strain_2 == "s1", sd_resolve_strain_1, sd_resolve_strain_2)
  ) %>% 
  mutate(across(everything(), ~ as.numeric(.x)))


save(param_table, file = tail(.args, 1))
