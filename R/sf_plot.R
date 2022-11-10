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

c("ggplot2") |> .req()

.args <- .fromArgs(c(
  file.path("data", "sf_gp_utils.rda"),
  file.path("data", "scenario_5.json"),
  file.path("output", "analysis", "scenario_5.rds"),
  file.path("output", "analysis", "plot_5.png")
))

load(.args[1])

# TODO a with-defaults-json config reader
jargs <- jsonlite::read_json(.args[2])
res <- readRDS(.args[3])

p <- sf_plot_pp(res, fill = model) + facet_grid(. ~ type) +
  ggtitle(label = jargs$scen_desc) + ggplot2::theme(legend.position  = "bottom") +
  ggplot2::guides(fill = ggplot2::guide_legend(title = "Model")) +
  ggplot2::scale_fill_brewer(palette = "Dark2")

ggsave(tail(.args, 1), p, width = 10, height = 6)