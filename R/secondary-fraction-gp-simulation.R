#' Simulate incidence and prevalence examples assuming the convolution
#' generative process is correct
#'
#' Author: Sam Abbott
#' Licence: MIT
#' Last modified: 2022-05-14

#' load data.table for manipulation
c("data.table", "EpiNow2") |> .req()

.args <- .fromArgs(
  c(
    here::here("data", "sf_gp_utils.rda"),
    here::here("data", "example-incidence.rds")
# here::here("data", "example-prevalence.rds"),
  )
)

resultfile <- tail(.args, 1)
load(.args[1])

#' #### Incidence data example ####

#' make some example secondary incidence data
cases <- as.data.table(EpiNow2::example_confirmed)

cases[, c(
  "primary",
  "scaling",
  "meanlog", "sdlog"
) := .(
    confirm,
    0.4, #' Assume that only 40 percent of cases are reported
    1.8, 0.5 #' Parameters of the assumed log normal delay distribution
)]

#' sniff type

simtype <- gsub(
  ".*-(\\w+)\\.rds",
  "\\1",
  basename(resultfile),
  ignore.case = TRUE
)

#' Simulate secondary cases
sim_cases <- sf_gp_simulate(cases, type = simtype)

saveRDS(sim_cases, resultfile)
