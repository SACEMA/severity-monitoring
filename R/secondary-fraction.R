#! /usr/bin/env Rscript

#' Estimate the secondary fraction using iterative convolution models
#'
#' Example usage:
#'
#' Rscript R/secondary-fraction.R -o data/example-incidence.rds --path test -a -l
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
  args$obs_preds <- TRUE
}

#' Set core usage
options(mc.cores = args$cores)

#' Error if being run with no output flags
if (!args$plot & !args$fit & !args$summary & !args$scores &
     ! args$relative_performance) {
  if (!interactive()) {
    stop("No output flags specified. Use --help for more information.")
  }
}

#' Make sure all progress is output in very verbose mode
if (args$loud) {
  args$verbose <- TRUE
}

#' Make a directory to save output
if (args$verbose) {
  message("Creating a directory to save output at: ", args$path)
}
dir.create(
  args$path,
  showWarnings = FALSE,
  recursive = TRUE
)

#' Load observations
if (is.null(args$observations)) {
  stop("No observations specified. Use --help for more information.")
}
if (args$verbose) {
  message("Reading in observations from: ", args$observations)
}
observations <- readRDS(args$observations)
observations <- data.table::as.data.table(observations)

#' Filter for target date
if (!is.null(args$target_date)) {
  if (args$verbose) {
    message("Filtering observations to target date: ", args$target_date)
  }
  observations <- observations[date <= as.date(args$target_date)]
}

#' Load delay prior distributions
if (args$verbose) {
  message("Reading in delay distribution priors from: ", args$delay)
}
delay <- readRDS(args$delay)

#' Estimate the secondary fraction
if (args$verbose) {
  message("Estimating the secondary fraction")
}
estimates <- sf_estimate(
  reports = observations,
  secondary = EpiNow2::secondary_opts(args$observation_type),
  delays = delay,
  obs = EpiNow2::obs_opts(
    week_effect = args$dow,
    family = args$family,
    scale = list(mean = args$scale_mean, sd = args$scale_sd)
  ),
  windows = c(args$baseline_window, args$window),
  window_overlap = args$windows_overlap,
  priors = if (!is.null(args$priors)) readRDS(args$priors) else NULL,
  prior_from_posterior = args$independent,
  prior_inflation = args$prior_inflation,
  verbose = args$loud,
  control = list(
    adapt_delta = args$adapt_delta, max_treedepth = args$max_treedepth
  )
)

#' Extract and munge summarised posterior predictions
if (args$verbose) {
  message("Extracting summarised predictions")
}
summarised_predictions <- sf_extract_summarised_predictions(estimates)


#' Plot posterior predictions (optional)
if (args$plot) {
  plot_path <- file.path(args$path, "plots")
  dir.create(
    file.path(plot_path),
    showWarnings = FALSE,
    recursive = TRUE
  )
  if (args$verbose) {
    message(
      "Saving plots of posterior predictions"
    )
  }
  pp_plot <- sf_plot_pp(summarised_predictions, fill = model) +
    ggplot2::facet_wrap(ggplot2::vars(type), scales = "free_x") +
    ggplot2::theme(legend.position  = "bottom") + 
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Model"))
  
  ggplot2::ggsave(
    file.path(plot_path, "posterior-predictions.png"),
    pp_plot
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
  saveRDS(
    posterior_prediction_summary,
    file.path(args$path, "posterior-prediction-summary.rds")
  )
}

#' Extract secondary fraction posterior samples
if (args$obs_preds) {
  if (args$verbose) {
    message("Saving posterior samples of observations")
  }
  #' Save posterior estimates
  saveRDS(
    estimates$posterior_predictions, file = file.path(
      args$path, "obs-pp-samples.rds"
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
