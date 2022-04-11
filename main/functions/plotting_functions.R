# .args <- if (interactive()) {
#   c("./main/data/plotting_functions.RData")
# } else {
#   commandArgs(trailingOnly = TRUE)
# }

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
    theme(legend.title = element_blank()) +
    labs(y = "Counts", x = "Date", title = plot_title)
}


save(list = ls(), file = tail(.args, 1))
