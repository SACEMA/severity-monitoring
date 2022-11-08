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

.args <- .fromArgs(
  c(
    here::here("data", "sf_gp_utils.rda"),
    here::here("data", "est_config.json"),
    here::here("data", "example-incidence.rds"),
    here::here("data", "example-sf-est.rds")
  )
)

load(.args[1])

# TODO a with-defaults-json config reader
jargs <- jsonlite::read_json(.args[2])
options(mc.cores = jargs$cores)

observations <- readRDS(.args[3]) |> data.table::as.data.table()

#' Filter for target date
if (!is.null(jargs$target_date)) {
  observations <- observations[date <= as.Date(jargs$target_date)]
}

#' Load delay prior distributions
delay <- readRDS(.args[4])

#' Estimate the secondary fraction
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

saveRDS(estimates, tail(.args, 1))
