.args <- if (interactive()) {
  c("./main/data/plotting_functions.RData")
} else {
  commandArgs(trailingOnly = TRUE)
}

#' Plot basic output from estimate_secondary
#'
#' @param predictions As in, estimate_secondary()[['predictions']]
#' @param data_raw  The raw data used for the predictions
#' @param plot_title plot title
#'
#' @description Basic plot, showing primary and secondary input time series,
#' as well as median and mean time series of fit from estimate_secondary().
#' @return
#' @export
#'
#' @examples
plot_est_sec_out <- function(dat,
                             fig_title = "True, observed, and predicted outcomes") {
  ggplot(
    data = dat,
    aes(x = date)
  ) +
    geom_line(aes(
      y = median,
      color = "Median secondary estimate"
    ),
    linetype = 3,
    size = 2
    ) +
    geom_line(aes(
      y = mean,
      color = "Mean secondary estimate"
    ),
    linetype = 3,
    size = 2
    ) +
    geom_line(aes(
      y = primary,
      color = "Primary data"
    )) +
    geom_line(aes(
      y = secondary,
      color = "Secondary data"
    )) +
    geom_line(
      aes(
        y = primary_underlying,
        color = "Underlying primary data"
      )
    ) +
    geom_line(
      aes(
        y = secondary_underlying,
        color = "Underlying secondary data"
      )
    ) +
    scale_y_log10(breaks = seq(0, max(dat$primary_underlying), 100),
                  labels = seq(0, max(dat$primary_underlying), 100)
                  ) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(
      y = "Counts (log-transformed)",
      title = fig_title,
      color = "Predicted"
    ) +
    # theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank()
    ) +
    NULL
}

#' Plot ratios of the true, observed, predicted outcomes
#'
#' @param dat
#' @param plot_title
#'
#' @return
#' @export
#'
#' @examples
plot_ratios <- function(dat, fig_title = "Fraction of secondary to primary outcomes",
                        fig_caption = "Vertical axis is log-transformed and obs_opts has prior of mean = 1 and sd = 1; default delay_opts()") {
  ggplot(
    data = dat,
    aes(x = date)
  ) +
    geom_line(aes(
      y = secondary / primary,
      color = "Sec/prim (observed)"
    ),
    linetype = 'solid',
    size = 1
    ) +
    geom_line(aes(
      y = secondary_underlying / primary_underlying,
      color = "Sec/prim (true)"
    ),
    linetype = 'solid',
    size = 1
    ) +
    geom_line(aes(
      y = frac_obs_stan,
      color = "Fraction observed (estimated)"
    ),
    linetype = 'dotted',
    size = 1
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0)) +
    scale_y_log10() +
    # theme(axis.text.x = element_text(hjust = -0.5)) +
    labs(
      y = "Fractions",
      x = "Date",
      title = fig_title,
      caption = fig_caption,
      color = "Fractions"
    ) +
    # theme_minimal(base_size = 14) +
    NULL
}

save(list = ls(), file = tail(.args, 1))
