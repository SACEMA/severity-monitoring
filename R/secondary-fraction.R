#' Estimate the secondary fraction using iterative convolution models
#'
#' Example usage:
#'
#' Rscript R/secondary-fraction.R -o data/example-incidence.rds -v -a -p tmp
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

#' Turn on all output if specified
if (args$all) {
  args$plot <- TRUE
  args$fit <- TRUE
  args$summary <- TRUE
  args$scores <- TRUE
  args$relative_performance <- TRUE
}

#' Make sure all progress is output in very verbose mode
if (args$very_verbose) {
  args$verbose <- TRUE
}

#' Load observations
if (args$verbose) {
  message("Reading in observations from: ", args$observations)
}
observations <- readRDS(args$observations)

#' Filter for target date
if (!is.null(args$target_date)) {
  if (args$verbose) {
    message("Filtering observations to target date: ", args$target_date)
  }
  observations <- observations[date <= args$target_date]
}

#' Load delay prior distributions
if (args$verbose) {
  message("Reading in delay distribution priors from: ", args$delays)
}
delays <- readRDS(args$delay_prior)

#' Estimate the secondary fraction
if (args$verbose) {
  message("Estimating the secondary fraction")
}
estimates <- sf_estimate(
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
  verbose = args$very_verbose,
  control = list(
    adapt_delta = args$adapt_delta, max_treedepth = args$max_treedepth
  )
)

#' Make a directory to save output
dir.create(
  args$path,
  showWarnings = FALSE,
  recursive = TRUE
)


#' Plot posterior predictions (optional)
if (args$plot) {
  dir.create(
    file.path(args$path, "plots"),
    showWarnings = FALSE,
    recursive = TRUE
  )
  if (args$verbose) {
    message("Saving plots of posterior predictions")
  }
  p <- plot(fit, primary = TRUE)
  ggplot2::ggsave(
    file.path(args$path, "posterior-predictions.png"), p
  )
}

#' Save the fit
if (args$fit) {
  fit_path <- file.path(args$path, "fit")
  es_path <- file.path(fit_path, "estimate_secondary")
  dir.create(
    es_path,
    showWarnings = FALSE,
    recursive = TRUE
  )
  fs_path <- file.path(fit_path, "forecast_secondary")
  dir.create(
    fs_path,
    showWarnings = FALSE,
    recursive = TRUE
  )
  if (args$verbose) {
    message("Saving estimate_secondary fit object")
  }
  purrr::walk2(
    estimates$estimate_secondary, names(estimates$estimate_secondary),
    ~ saveRDS(.x, file.path(es_path, paste0(.y, ".rds")))
  )
  if (args$verbose) {
    message("Saving forecast_secondary fit object")
  }
  purrr::walk2(
    estimates$forecast_secondary, names(estimates$forecast_secondary),
    ~ saveRDS(.x, file.path(fs_path, paste0(.y, ".rds")))
  )
}

if (args$summary) {
  if (args$verbose) {
    message("Saving posterior summary")
  }
  posterior_summary <- purrr::map(
    estimates$estimate_secondary,
    sf_summarise_posterior
  )
  names(posterior_summary) <- names(estimates$estimate_secondary)
  posterior_summary <- data.table::rbindlist(
    posterior_summary, idcol = "model"
  )
  saveRDS(
    posterior_summary, file.path(args$path, "posterior-summary.rds")
  )
}

#' Extract posterior estimates
if (args$posterior_predictions) {
  if (args$verbose) {
    message("Saving posterior samples")
  }
  #' Save posterior estimates
  saveRDS(
    estimates$posterior_predictions, file = file.path(
      args$path, "secondary-fraction-samples.rds"
    )
  )
}

#' Compare estimates from target and baseline using proper scoring rules
if (args$scores | args$relative_performance) {
  if (args$verbose) {
    message("Calculating scores for the baseline and target")
  }
  pp <- estimates$posterior_predictions
  pp <- observations[pp, on = "date"]
  pp[, true_value := secondary][, prediction := value]

  scores <- scoringutils::score(
    pp[, .(date, model, sample, true_value, prediction)]
  )
  if (args$scores){
    if (args$verbose) {
      message("Saving scores")
    }
    saveRDS(scores, file.path(args$path, "scores.rds"))
  }
  if (args$relative_performance) {
    if (args$verbose) {
      message("Saving relative performance")
    }
    summarised_scores <- scoringutils::summarise_scores(
      scores, by = "model"
    )[, relative_performance := crps / data.table::shift(crps, 1)]
    saveRDS(summarised_scores[model == "target"]$relative_performance,
            file.path(args$path, "relative-performance.rds")
    )
  }
}
