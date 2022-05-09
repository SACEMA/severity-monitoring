#' Load required packages

#' Source tools

#' Load observations

#' Estimate the secondary fraction
fit <- sf_estimate(
  reports = observations,
  secondary = EpiNow2::secondary_opts(),
  delays = delay_opts(
    list(
      mean = 2.5, mean_sd = 0.5,
      sd = 0.47, sd_sd = 0.25, max = 30
    )
  ),
  truncation = EpiNow2::trunc_opts(),
  obs = EpiNow2::obs_opts(),
  windows = c(7*6, 7*2),
  window_overlapping = TRUE,
  prior_inflation = 2,
  CrIs = c(0.2, 0.5, 0.9),
  model = NULL,
  verbose = interactive(),
  ...
)

#' Plot posterior predictions (optional)
if (plot) {
  plot(fit)
}
#' Extract posterior estimates
posterior <- sf_posterior_samples(fit)

#' Save posterior estimates
data.table::fwrite(posterior, file = results_path)