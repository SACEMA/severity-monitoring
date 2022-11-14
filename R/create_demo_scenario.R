
c("EpiNow2", "jsonlite", "data.table") |> .req()

.args <- .fromArgs(c(
  file.path("output", "synthetic", "synthetic_funs.rda"),
  file.path("data", "scenario_5.json"),
  file.path("output", "synthetic", "scenario_5.rds")
))

load(.args[1])
scenario_params <- jsonlite::read_json(.args[2], simplifyVector = TRUE)

ts_len <- scenario_params$ts_len
scenario_description <- scenario_params$scen_desc
trash_burn <- scenario_params$trashburn

strain_1_params <- scenario_params$strain_1
strain_2_params <- scenario_params$strain_2

strain_series <- function(
  pars, burnin_length
) with(pars, generate_exponential_time_series(
  initial_value = initial_incidence,
  rate = exp_growth_rate,
  ts_length = ts_len,
  burn_length = burnin_length
) |>
  sample_ts() |>
  expand_ts() |>
  sample_outcomes(
    p_severe, p_hosp_if_severe, p_died_if_hosp,
    p_seek_test
  ) |>
  sample_delays(
    mean_bg_test, sd_bg_test,
    rate_bg_hosp,
    mean_hosp_test, sd_hosp_test,
    mean_severe, sd_severe,
    mean_severe_hosp, sd_severe_hosp,
    mean_hosp_died, sd_hosp_died,
    mean_resolve, sd_resolve,
    mean_seek_test, sd_seek_test
  ) |>
  compute_event_times_from_delays() |>
  compute_time_series_from_linelist() |>
  subset(between(time, 0, burnin_length + ts_len)) |>
  setnames(
    c("cases_observed", "admissions", "cases", "severe_cases"),
    c("primary", "secondary", "latent_primary", "latent_secondary")
  )
)

generate_scenario <- function(s1, s2, verbose = FALSE) {

  burnin_length <- round(2 * (s1$mean_severe + s1$mean_severe_hosp))

  ##  create TS for strain1 including true and observed cases and admissions
  ## pipe functions from synth data functions to one another
  dd_strain_1 <- strain_series(s1, burnin_length)
  if (trash_burn) dd_strain_1 <- dd_strain_1[burnin_length < time][, time := 1:.N]
  dd_strain_2 <- strain_series(s2, burnin_length)
  if (trash_burn) dd_strain_2 <- dd_strain_2[burnin_length < time][, time := 1:.N]
  
  if (verbose) browser()
  
  res <- merge(
    dd_strain_1, dd_strain_2,
    by = "time", all = TRUE
  )[, .(
    time,
    latent_primary = nafill(latent_primary.x, fill = 0) + nafill(latent_primary.y, fill = 0),
    primary = nafill(primary.x, fill = 0) + nafill(primary.y, fill = 0),
    latent_secondary = nafill(latent_secondary.x, fill = 0) + nafill(latent_secondary.y, fill = 0),
    secondary = nafill(secondary.x, fill = 0) + nafill(secondary.y, fill = 0)
  )]
  
  return(res)
}

num_sims <- scenario_params$num_sims

tot.dt <- lapply(
  1:num_sims, \(i, s1, s2) {
    set.seed(i);
    generate_scenario(s1, s2)
}, s1 = strain_1_params, s2 = strain_2_params) |> rbindlist(idcol = "sim_id")

saveRDS(tot.dt, file = tail(.args, 1))

#' @examples 
#' require(ggplot2)
#' ggplot(melt(tot.dt, id.vars = c("sim_id", "time"))) +
#'   aes(time, value, color = variable, group = interaction(sim_id, variable)) +
#'   geom_line(alpha = 0.3) +
#'   theme_minimal() +
#'   scale_y_log10()
#' 
# scenario_data <- foreach(
#   sim_num = 1:num_sims,
#   .combine = rbindlist,
#   .packages = c("data.table", "EpiNow2")
# ) %dopar% {
#   generate_scenario(strain_1_params, strain_2_params)[, sim_id := sim_num]
# }

# Parallelize the runs
# start_time <- Sys.time()
# end_time <- Sys.time()
# 
# stopCluster(cl)
# 
# print(end_time - start_time)


# if (interactive()) {
#   options(scipen = 9999) # remove scientific notation
#   
#   labels_df <- scenario_data %>% 
#     group_by(name) %>% 
#     slice(1)
#   
#   ts_plot_combo <- ggplot(
#     scenario_data,
#     aes(
#       x = time,
#       y = value,
#       groups = factor(sim_id),
#       color = name
#     )
#   ) +
#     geom_line() +
#     geom_labelline(data = labels_df, 
#                    aes(
#       label = name,
#       group = interaction(sim_id, name)
#     ),
#     hjust = 0,
#     size = 3.5,
#     linewidth = 0.45,
#     straight = TRUE
#     ) +
#     scale_y_log10() +
#     theme_minimal() +
#     theme(legend.position = "none") +
#     labs(
#       x = "Day",
#       y = "Daily count (log transformed)",
#       color = "",
#       title = scenario_description
#     )
#   print(ts_plot_combo)
# }

# save(list= c("ts_combined", "dd_strain_1", "dd_strain_2"),
#    file = tail(.args, 1))
