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

c("ggplot2", "data.table") |> .req()

.args <- .fromArgs(c(
  file.path("data", "sf_gp_utils.rda"),
  file.path("data", "scenario_5.json"),
  file.path("output", "synthetic", "scenario_5.rds"),
  file.path("output", "analysis", "scenario_5.rds"),
  file.path("output", "analysis", "plot_5.png")
))

load(.args[1])

# TODO a with-defaults-json config reader
jargs <- jsonlite::read_json(.args[2])
syn <- readRDS(.args[3])[sim_id == 1]
syn$sim_id <- NULL
res <- readRDS(.args[4])

sf_plot_pp2 <- function(predictions, ...) {
  ooscut <- predictions[type == "out of sample", min(date[!is.na(mean)]), by=.(model)][, max(V1)]
  
  plot <- 
    ggplot2::ggplot(
      predictions[!is.na(mean)][(type == "in sample") | date >= ooscut]
    ) +
    ggplot2::aes(x = date, y = secondary, ...)
  
  plot <- EpiNow2:::plot_CrIs(
    plot, EpiNow2:::extract_CrIs(predictions), alpha = 0.4, size = 1
  )
  
  plot <- plot +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "Notifications", x = "Date") +
    ggplot2::scale_x_date(date_breaks = "week", date_labels = "%b %d") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  
  return(plot)
}

p <- sf_plot_pp2(res, fill = model) + facet_grid(. ~ type) +
  ggtitle(label = jargs$scen_desc) + ggplot2::theme(legend.position  = "bottom") +
  ggplot2::guides(fill = ggplot2::guide_legend(title = "Model"), color = guide_legend(override.aes = list(fill = NA))) +
  ggplot2::scale_fill_brewer(palette = "Dark2") +
  geom_point(
    aes(x=time-1+res[, min(date)], y=value, color = variable),
    data = melt.data.table(syn, id.vars = c("time"))[!(variable %like% "primary")], inherit.aes = FALSE,
    alpha = 0.2
  ) +
  geom_point(
    aes(x=time-1+res[, min(date)], y=value/100, color = variable),
    data = melt.data.table(syn, id.vars = c("time"))[(variable %like% "primary")], inherit.aes = FALSE,
    alpha = 0.2
  ) + scale_y_log10(
    name = "Secondary", sec.axis = dup_axis(trans = ~ 100*., name = "Primary (100x)"), labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) + coord_cartesian(ylim = c(30, 3000), expand = FALSE)

ggsave(tail(.args, 1), p, width = 10, height = 6)