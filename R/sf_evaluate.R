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

c("data.table") |> .req()

.args <- .fromArgs(c(
  file.path("data", "sf_gp_utils.rda"),
  file.path("data", "est_config.json"),
  file.path("data", "weakly-informed-delays.rds"),
  file.path("output", "synthetic", "scenario_3.rds"),
  file.path("output", "analysis", "scenario_3.rds")
))

load(.args[1])

# TODO a with-defaults-json config reader
jargs <- jsonlite::read_json(.args[2])
options(mc.cores = jargs$cores)

#' Load delay prior distributions
delay <- readRDS(.args[3])

observations <- readRDS(.args[4]) |> data.table::as.data.table()
observations[, date := as.Date(jargs$start_date)+time ]
#' Filter for target date
if (!is.null(jargs$target_date)) {
  observations <- observations[date <= as.Date(jargs$target_date)]
}

rolling_window_starts <- seq(
  observations[, min(time)],
  observations[, max(time) - sum(jargs$baseline_window, jargs$window)],
  by = 1
)
rolling_window_ends <- rolling_window_starts + sum(jargs$baseline_window, jargs$window)

# TODO: currently reporting burn-in error?
res <- mapply(function(s, e, dt) {
  dt[sim_id == 1][between(time, s, e)] |> sf_estimate(
    secondary = EpiNow2::secondary_opts(jargs$observation_type),
    delays = delay,
    obs = EpiNow2::obs_opts(
      week_effect = jargs$dow,
      family = jargs$family,
      scale = list(mean = jargs$scale_mean, sd = jargs$scale_sd)
    ),
    windows = c(jargs$baseline_window, jargs$window),
    window_overlap = jargs$windows_overlap,
    priors = if (!is.null(jargs$priors)) readRDS(jargs$priors) else NULL,
    prior_from_posterior = jargs$independent,
    prior_inflation = jargs$prior_inflation,
    verbose = jargs$loud,
    control = list(
      adapt_delta = jargs$adapt_delta, max_treedepth = jargs$max_treedepth
    )
  )
}, s = rolling_window_starts, e = rolling_window_ends, MoreArgs = list(dt = observations), SIMPLIFY = FALSE)

res <- observations[sim_id == 1] |> sf_estimate(
  secondary = EpiNow2::secondary_opts(jargs$observation_type),
  delays = delay,
  obs = EpiNow2::obs_opts(
    week_effect = jargs$dow,
    family = jargs$family,
    scale = list(mean = jargs$scale_mean, sd = jargs$scale_sd)
  ),
  windows = c(jargs$baseline_window, jargs$window),
  window_overlap = jargs$windows_overlap,
  priors = if (!is.null(jargs$priors)) readRDS(jargs$priors) else NULL,
  prior_from_posterior = jargs$independent,
  prior_inflation = jargs$prior_inflation,
  verbose = jargs$loud,
  control = list(
    adapt_delta = jargs$adapt_delta, max_treedepth = jargs$max_treedepth
  )
)

summs <- res |> lapply(sf_extract_summarised_predictions) |> rbindlist(idcol = "startIdx")

# res$posterior_predictions[model == "baseline", range(date)]

rel_scores <- res |> lapply(\(inc.dt) {
  scores <- (observations[
    sim_id == 1
  ][
    inc.dt$posterior_predictions, on =.(date)][,
      .(date, model, sample, true_value = secondary, prediction = value)
    ] |> scoringutils::score() |>
    scoringutils::summarise_scores(by = "model"))[,
      .(model, crps)
    ][, 
      compbaseline := 1
    ]
  return(scores[
    model != "baseline"
  ][
    scores[model == "baseline", .SD, .SDcols = -c("model")],
    on=.(compbaseline)
  ][, relative_performance := crps / i.crps ]
  )
}) |> rbindlist(idcol = "startIdx")

# scores <- observations[
#   sim_id == 1
# ][
#   res$posterior_predictions, on =.(date)][,
#   .(date, model, sample, true_value = secondary, prediction = value)
# ] |> scoringutils::score() |>
#   scoringutils::summarise_scores(by = "model")
# scores <- scores[, .(model, crps) ][, compbaseline := 1]
# rel_score <- scores[
#   model != "baseline"
# ][
#   scores[model == "baseline", .SD, .SDcols = -c("model")],
#   on=.(compbaseline)
# ][, relative_performance := crps / i.crps ]

saveRDS(summs, tail(.args, 1))
saveRDS(rel_scores, gsub("\\.", "_score.", tail(.args, 1)))

# res <- observations[,
#                     .SD[,.(date, primary, secondary)] |> sf_estimate(
#                       secondary = EpiNow2::secondary_opts(jargs$observation_type),
#                       delays = delay,
#                       obs = EpiNow2::obs_opts(
#                         week_effect = jargs$dow,
#                         family = jargs$family,
#                         scale = list(mean = jargs$scale_mean, sd = jargs$scale_sd)
#                       ),
#                       windows = c(jargs$baseline_window, jargs$window),
#                       window_overlap = jargs$windows_overlap,
#                       priors = if (!is.null(jargs$priors)) readRDS(jargs$priors) else NULL,
#                       prior_from_posterior = jargs$independent,
#                       prior_inflation = jargs$prior_inflation,
#                       verbose = jargs$loud,
#                       control = list(
#                         adapt_delta = jargs$adapt_delta, max_treedepth = jargs$max_treedepth
#                       )
#                     ) |> (\(dt) {
#                       sf_extract_summarised_predictions(dt)
#                     })(),
#                     by = sim_id
# ]

#' @examples 
#' sf_plot_pp(thing, group = sim_id)