#' Simulate incidence and prevalence examples assuming the convolution
#' generative process is correct
#'
#'
#' Author: Sam Abbott
#' Licence: MIT
#' Last modified: 2022-05-14

#' load data.table for manipulation
c("data.table") |> .req()

#' #### Incidence data example ####

#' make some example secondary incidence data
inc_cases <- as.data.table(EpiNow2::example_confirmed)

inc_cases[, c(
  "primary",
  "scaling",
  "meanlog", "sdlog"
) := .(
    confirm,
    0.4, #' Assume that only 40 percent of cases are reported
    1.8, 0.5 #' Parameters of the assumed log normal delay distribution
)]

#' TODO break up simulate, save steps

#' Simulate secondary cases
inc_cases <- sf_gp_simulate(inc_cases, type = "incidence")

saveRDS(inc_cases, here::here("data", "example-incidence.rds"))

#' #### Prevalence data example ####

#' make some example prevalence data
prev_cases <- EpiNow2::example_confirmed
prev_cases <- as.data.table(prev_cases)[, primary := confirm]

#' Assume that only 30 percent of cases are reported
prev_cases[, scaling := 0.3]

# Parameters of the assumed log normal delay distribution
prev_cases[, meanlog := 1.6][, sdlog := 0.8]

# Simulate secondary cases
prev_cases <- sf_gp_simulate(prev_cases, type = "prevalence")

saveRDS(prev_cases, here::here("data", "example-prevalence.rds"))