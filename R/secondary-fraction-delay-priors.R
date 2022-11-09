#' Define delay distriution priors
#'
#'
#' Author: Sam Abbott
#' Licence: MIT
#' Last modified: 2022-05-14

c("EpiNow2") |> .req()

.args <- .fromArgs(file.path("data", "weakly-informed-delays.rds"))

EpiNow2::delay_opts(
  list(
    mean = EpiNow2::convert_to_logmean(10, 5),
    mean_sd = 0.5,
    sd = EpiNow2::convert_to_logsd(10, 5),
    sd_sd = 0.25,
    max = 30
  )
) |> saveRDS(file = tail(.args, 1))
