#' Define delay distriution priors
#'
#'
#' Author: Sam Abbott
#' Licence: MIT
#' Last modified: 2022-05-14

c("EpiNow2", "here") |> .req()

resultfile <- fromArgs(
  here::here("data", "weakly-informed-delays.rds")
)

weak_delays <- EpiNow2::delay_opts(
  list(
    mean = EpiNow2::convert_to_logmean(10, 5),
    mean_sd = 0.5,
    sd = EpiNow2::convert_to_logsd(10, 5),
    sd_sd = 0.25,
    max = 30
  )
)

saveRDS(weak_delays, resultfile)
