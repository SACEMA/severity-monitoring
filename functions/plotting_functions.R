.args <- if(interactive()){c('./functions/plotting_functions.RData')}else{commandArgs(trailingOnly=TRUE)}

#' Plot basic output from estimate_secondary
#'
#' @param predictions As in, estimate_secondary()[['predictions']]
#' @param plot_title plot title
#' @description Basic plot, showing primary and secondary input time series, 
#' as well as median and mean time series of fit from estimate_secondary().
#' @return
#' @export
#'
#' @examples
plot_est_sec_out <- function(predictions,
                             plot_title = "obs_opts has scale set to: mean = 0.5, sd = 0.0001"){
  predictions %>% 
    ggplot(aes(x = date)) +
    geom_line(aes(y = median, color = "Median secondary estimate"), lwd=2) + 
    geom_line(aes(y = mean, color = "Mean secondary estimate"), lty = 3, lwd =2)+
    geom_line(aes(y = primary, color = "Primary data")) +
    geom_line(aes(y = secondary, color = "Secondary data")) +
    theme(legend.title = element_blank()) +
    labs(y="Counts", x = "Date", title = plot_title)
  
}


save(list = ls(), file = tail(.args,1))
