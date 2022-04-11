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
    # scale_y_log10(breaks = seq(0, max(dat$primary_underlying), 50),
    #               labels = seq(0, max(dat$primary_underlying), 50)
    #               ) +
    scale_y_log10() +
    labs(
      y = "Counts",
      # x = "Date",
      title = fig_title,
      color = "Predicted"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank()
    )
}


save(list = ls(), file = tail(.args, 1))
