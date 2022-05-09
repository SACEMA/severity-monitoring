#' Estimate the secondary fraction using iterative convolution models
#'
#' Example usage:
#'
#' Rscript R/secondary-fraction.R
#' Rscript R/secondary-fraction.R
#'
#' For more information on execution run Rscript R/secondary-fraction.R --help
#'
#' This file is concerned with the high level control of the datasets,
#' estimation, plotting, and results saving. For more detail on each modelling
#' step see R/secondary-fraction-utils.R
#'
#' Author: Sam Abbott
#' Licence: MIT
#' Last modified: 2022-05-14

#' Source tools
source(here::here("R", "secondary-fraction-utils.R"))

#' Set up command line arguments (or use defaults when interactive)
args <- sf_cli_interface()

#' Load observations
observations <- readRDS(args$observations)

#' Filter for target date
if (!is.null(args$target_date)) {
  observations <- observations[date <= args$target_date]
}

#' Load delay prior distributions
delays <- readRDS(args$delay_prior)

#' Estimate the secondary fraction
fit <- sf_estimate(
  reports = observations,
  secondary = EpiNow2::secondary_opts(
    type = args$observation_type,
  ),
  delays = delays,
  obs = EpiNow2::obs_opts(
    week_effect = args$day_of_week,
    family = args$observation_model,
    scale = list(mean = args$scale[1], sd = args$scale[2])
  ),
  windows = args$windows,
  window_overlap = args$window_overlap,
  prior_inflation = args$prior_inflation,
  verbose = args$verbose,
  control = list(
    adapt_delta = args$adapt_delta, max_treedepth = args$max_treedepth
  )
)

#' Make a directory to save output
dir.create(
  args$output_path,
  showWarnings = FALSE,
  recursive = TRUE
)

#' Plot posterior predictions (optional)
if (args$save_plot) {
  if (args$verbose) {
    message("Saving plot of posterior predictions")
  }
  p <- plot(fit, primary = TRUE)
  ggplot2::ggsave(
    file.path(args$output_path, "secondary-fraction.png"), p
  )
}

#' Save the fit
if (args$save_fit) {
  if (args$verbose) {
    message("Saving estimate_secondary fit object")
  }
  saveRDS(
    fit, file.path(args$output_path, "fit.rds")
  )
}

if (args$save_summary) {
  if (args$verbose) {
    message("Saving posterior summary")
  }
  posterior_summary <- sf_summarise_posterior(fit)
  saveRDS(
    posterior_summary, file.path(args$output_path, "posterior-summary.rds")
  )
}

#' Extract posterior estimates
posterior <- sf_posterior_samples(fit)

if (args$verbose) {
  message("Saving posterior samples")
}

#' Save posterior estimates
saveRDS(
  posterior, file = file.path(
    args$output_path, "secondary-fraction-samples.rds"
  )
)
