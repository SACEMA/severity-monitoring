 sf_discretised_lognormal_pmf <- function(meanlog, sdlog, max_d,
                                          reverse = FALSE) {
  pmf <- plnorm(1:max_d, meanlog, sdlog) -
    plnorm(0:(max_d - 1), meanlog, sdlog)
  pmf <- as.vector(pmf) / as.vector(plnorm(max_d, meanlog, sdlog))
  if (reverse) {
    pmf <- rev(pmf)
  }
  return(pmf)
}

 sf_discretised_lognormal_pmf_conv <- function(x, meanlog, sdlog) {
  pmf <- sf_discretised_lognormal_pmf(meanlog, sdlog, length(x), reverse = TRUE)
  conv <- sum(x * pmf, na.rm = TRUE)
  return(conv)
}

#' Simulate secondary observations using the secondary fraction generative
#' process model
#'
#' @param family Character string defining the observation model. Options are
#' Negative binomial ("negbin"), the default, Poisson ("poisson"), and "none"
#' meaning the expectation is returned.
#'
#' @param delay_max Integer, defaulting to 30 days. The maximum delay used in
#' the convolution model.
#'
#' @param ... Additional arguments to pass to `rnbinom()` if
#' `family = "negbin"`.
#'
#' @return A `data.table` containing the simulated observations and input data
#' @inheritParams secondary_opts
#' @importFrom data.table as.data.table copy shift
#' @importFrom purrr pmap_dbl
#' @export
#' @examples
#' @author Sam Abbott
#' # load data.table for manipulation
#' library(data.table)
#'
#' #### Incidence data example ####
#'
#' # make some example secondary incidence data
#' cases <- EpiNow2::example_confirmed
#' cases <- as.data.table(cases)[, primary := confirm]
#'
#' # Assume that only 40 percent of cases are reported
#' cases[, scaling := 0.4]
#'
#' # Parameters of the assumed log normal delay distribution
#' cases[, meanlog := 1.8][, sdlog := 0.5]
#' 
#' # Simulate secondary cases
#' cases <- sf_gp_simulate(cases, type = "incidence")
#' cases
#' #### Prevalence data example ####
#'
#' # make some example prevalence data
#' cases <- EpiNow2::example_confirmed
#' cases <- as.data.table(cases)[, primary := confirm]
#'
#' # Assume that only 30 percent of cases are reported
#' cases[, scaling := 0.3]
#'
#' # Parameters of the assumed log normal delay distribution
#' cases[, meanlog := 1.6][, sdlog := 0.8]
#'
#' # Simulate secondary cases
#' cases <- sf_gp_simulate(cases, type = "prevalence")
#' cases
sf_gp_simulate <- function(data, type = "incidence", family = "poisson",
                               delay_max = 30, ...) {
  type <- match.arg(type, choices = c("incidence", "prevalence"))
  family <- match.arg(family, choices = c("none", "poisson", "negbin"))
  data <- data.table::as.data.table(data)
  data <- data.table::copy(data)
  data <- data[, index := 1:.N]
  # apply scaling
  data <- data[, scaled := scaling * primary]
  # add convolution
  data <- data[,
    conv := purrr::pmap_dbl(list(i = index, m = meanlog, s = sdlog),
     function(i, m, s) {
       sf_discretised_lognormal_pmf_conv(
         scaled[max(1, i - delay_max):i], meanlog = m, sdlog = s
        )
     })]
  # build model
  if (type == "incidence") {
    data <- data[, secondary := conv]
  }else if (type == "prevalence") {
    data <- data[1, secondary := scaled]
    for (i in 2:nrow(data)) {
      index <-
        data[c(i - 1, i)][, secondary := shift(secondary, 1) - conv]
      index <- index[secondary < 0, secondary := 0]
      data[i, ] <- index[2][, secondary := secondary + scaled]
    }
  }
  # check secondary is greater that zero
  data <- data[secondary < 0, secondary := 0]
  data <- data[!is.na(secondary)]
  # apply observation model
  if (family == "poisson") {
    data <- data[, secondary := purrr::map_dbl(secondary, ~ rpois(1, .))]
  }else if (family == "negbin") {
    data <- data[, secondary := purrr::map_dbl(
      secondary, ~ rnbinom(1, mu = .), ...)
    ]
  }
  data <- data[, secondary := as.integer(secondary)]
  return(data[])
}
#' Command line interface for secondary fraction estimation
#'
#' Define the CLI interface and return the parsed arguments
#'
#' @param args_string String (optional) of command line flags to simulate CLI
#' interface when running interactively.
#'
#' @return List of arguments
#' @export
#' @author Sam Abbott
sf_cli_interface <- function(args_string = NA) {
  # set up the arguments
  option_list <- list(
    optparse::make_option(
      c("-v", "--verbose"),
      action = "store_true",
      default = FALSE,
      help = "Print verbose output "
    ),
    optparse::make_option(
      c("-q", "--quiet"),
      action = "store_true",
      default = FALSE,
      help = "Print less output "
    ),
    optparse::make_option(c("-o", "--observations"),
     default = "",
    type = "character",
    help = "Path to csv of observations including the following columns:
     'date', 'primary', and 'secondary'"
    )
  )
  if (is.character(args_string)) {
    args <- optparse::parse_args(
      optparse::OptionParser(option_list = option_list), args = args_string
    )
  }else {
    args <- optparse::parse_args(
      optparse::OptionParser(option_list = option_list)
    )
  }
  return(args)
}

sf_set_burn_in <- function(obs, window = 14, min_burn_in = 14) {
  burn_in <-  as.integer(max(obs$date) - min(obs$date)) - window
  if (burn_in < min_burn_in) {
   stop("Burn in must be greater than or equal to ",
        min_burn_in,
        " but with the currently specified window of ",
        window,
        " is ",
        burn_in
   )
  }
  return(burn_in)
}

sf_summarise_posterior <- function(fit, CrIs = c(0.05, 0.5, 0.95),
                                   params = c("delay", "frac_obs", "rep_phi")) {

  posterior <- EpiNow2::extract_stan_param(
    fit$fit,
    CrIs = CrIs
  )
  return(posterior)
}

#' Update estimate_secondary default priors
#'
#' This functions allows the user to more easily specify data driven or model
#' based priors for `estimate_secondary()` from example from previous model fits
#' using a `data.frame` to overwrite other default settings. Note that default
#' settings are still required.
#'
#' @param obs
#'
#' @param delays
#'
#' @param priors A `data.frame` of named priors to be used in model fitting
#' rather than the defaults supplied from other arguments. This is typically
#' useful if wanting to inform a estimate from the posterior of another model
#' fit. Priors that are currently use to update the defaults are the scaling
#' fraction ("frac_obs"), the mean delay ("delay_mean"), and standard deviation
#' of the delay ("delay_sd"). The `data.frame` should have the following
#' variables: `variable`, `mean`, and `sd`.
#'
#' @return A list as produced by `create_stan_data()`.
#' @author Sam Abbott
#' @inheritParams create_stan_args
#' @importFrom data.table as.data.table
#' @examples
#' priors <- data.frame(variable = "frac_obs", mean = 3, sd = 1)
#' sf_update_secondary_args(priors = priors)
sf_update_secondary_args <- function(obs = EpiNow2::obs_opts(),
                                     delays = EpiNow2::delay_opts(),
                                     priors, prior_inflation = 1,
                                     verbose = TRUE) {
  priors <- data.table::as.data.table(priors)
  if (!missing(priors)) {
    if (!is.null(priors) & nrow(priors) > 0) {
      if (verbose) {
        message(
          "Replacing specified priors with those from the passed in prior dataframe" # nolint
        )
      }
      priors <- priors[, sd := sd * prior_inflation]

      # replace scaling if present in the prior
      scale <- priors[grepl("frac_obs", variable)]
      if (nrow(scale) > 0) {
        obs$scale <- c(
          as.array(signif(scale$mean, 3)), as.array(signif(scale$sd, 3))
        )
      }
      # replace delay parameters if present
      delay_mean <- priors[grepl("delay_mean", variable)]
      delay_sd <- priors[grepl("delay_sd", variable)]
      if (nrow(delay_mean) > 0) {
        if (is.null(delays$delay_mean_mean)) {
         warning(
           "Cannot replace delay distribution parameters as no default has been set" # nolint
          )
        }
        delays$delay_mean_mean <- as.array(signif(delay_mean$mean, 3))
        delays$delay_mean_sd <- as.array(signif(delay_mean$sd, 3))
        delays$delay_sd_mean <- as.array(signif(delay_sd$mean, 3))
        delays$delay_sd_sd <- as.array(signif(delay_sd$sd, 3))
      }
      phi <- priors[grepl("rep_phi", variable)]
      if (nrow(phi) > 0) {
        obs$phi <- c(signif(phi$mean, 3), signif(phi$sd, 3))
      }
    }
  }
  return(list(obs = obs, delays = delays))
}

#' Estimate a Secondary Observation from a Primary Observation
#'
#' Estimates the relationship between a primary and secondary observation, for
#' example hospital admissions and deaths or hospital admissions and bed
#' occupancy. This implementation is an extension of the original function
#' `EpiNow2::estimate_secondary()`, the documentation of this function contains
#' additional details.
#'
#' @param secondary A call to `secondary_opts()` or a list containing the
#' following binary variables: cumulative, historic, primary_hist_additive,
#' current, primary_current_additive. These parameters control the structure of
#' the secondary model, see `secondary_opts()` for details.
#'
#' @param delays A call to `delay_opts()` defining delay distributions between
#' primary and secondary observations See the documentation of `delay_opts()`
#' for details. By default a diffuse prior  is assumed with a mean of 14 days
#' and standard deviation of 7 days (with a standard deviation of 0.5 and 0.25
#' respectively on the log scale).
#'
#' @param reports A data frame containing the `date` of report and both
#' `primary` and `secondary` reports.
#'
#' @param model A compiled stan model to override the default model. May be
#' useful for package developers or those developing extensions.
#'
#' @param priors A `data.frame` of named priors to be used in model fitting
#' rather than the defaults supplied from other arguments. This is typically
#' useful if wanting to inform a estimate from the posterior of another model
#' fit.
#'
#' @param windows A numeric vector of fitting windows to use. If only one window
#' is specified, the model is fit to the data in the window. If multiple windows
#' then the model is fit to the data in each window using the posterior as the
#' prior for the next window. Currently this is limited to two windows. The
#' default is a window of 2 weeks followed by a window of 1 week.
#'
#' @param prior_inflation A numeric value to increase the prior standard
#' deviation by. This is useful if the prior is not sufficiently diffuse to
#' capture likely changes in subsequent windows. The default is 1.
#'
#' @param windows_overlap A logical value that defaults to  `TRUE`. Should
#' fitting windows be assumed to be overlapping?
#'
#' @param verbose Logical, should model fitting progress be returned. Defaults
#' to `interactive()`.
#'
#' @param ... Additional parameters to pass to `rstan::sampling()`.
#'
#' @return A list containing: `predictions` (a data frame ordered by date with
#' the primary, and secondary observations, and a summary of the model
#' estimated secondary observations), `posterior` which contains a summary of
#' the entire model posterior, `data` (a list of data used to fit the
#' model), and `fit` (the `stanfit` object).
#' @export
#' @inheritParams estimate_infections
#' @inheritParams update_secondary_args
#' @inheritParams calc_CrIs
#' @importFrom EpiNow2 estimate_secondary
#' @author Sam Abbott
#' @examples
#' # make stan multicore 
#' options(cores = 4)
#' # load data.table for manipulation
#' library(data.table)
#'
#' #### Incidence data example ####
#'
#' # make some example secondary incidence data
#' cases <- EpiNow2::example_confirmed
#' cases <- as.data.table(cases)[, primary := confirm]
#'
#' # Assume that only 40 percent of cases are reported
#' cases[, scaling := 0.4]
#'
#' # Parameters of the assumed log normal delay distribution
#' cases[, meanlog := 1.8][, sdlog := 0.5]
#'
#' # Simulate secondary cases
#' cases <- sf_gp_simulate(cases, type = "incidence")
#' cases
#'
#' # fit model to example data specifying a weak prior for fraction reported
#' with a secondary case
#' inc <- sf_estimate(cases[1:60],
#'   obs = EpiNow2::obs_opts(
#'    scale = list(mean = 0.2, sd = 0.2), week_effect = FALSE
#'   )
#' )
#' plot(inc, primary = TRUE)
#'
#' # forecast future secondary cases from primary
#' inc_preds <- EpiNow2::forecast_secondary(
#'  inc, cases[61:.N][, value := primary]
#' )
#' plot(inc_preds, new_obs = cases, from = "2020-05-01")
#'
#' #### Prevalence data example ####
#'
#' # make some example prevalence data
#' cases <- EpiNow2::example_confirmed
#' cases <- as.data.table(cases)[, primary := confirm]
#'
#' # Assume that only 30 percent of cases are reported
#' cases[, scaling := 0.3]
#'
#' # Parameters of the assumed log normal delay distribution
#' cases[, meanlog := 1.6][, sdlog := 0.8]
#'
#' # Simulate secondary cases
#' cases <- sf_gp_simulate(cases, type = "prevalence")
#' cases
#'
#' # fit model to example prevalence data
#' prev <- sf_estimate(
#'   cases[1:100],
#'   secondary = EpiNow2::secondary_opts(type = "prevalence"),
#'   obs = EpiNow2::obs_opts(
#'     week_effect = FALSE,
#'     scale = list(mean = 0.4, sd = 0.1)
#'   )
#' )
#' plot(prev, primary = TRUE)
#'
#' # forecast future secondary cases from primary
#' prev_preds <- EpiNow2::forecast_secondary(
#'  prev, cases[101:.N][, value := primary]
#' )
#' plot(prev_preds, new_obs = cases, from = "2020-06-01")
sf_estimate <- function(reports,
                        secondary = EpiNow2::secondary_opts(),
                        delays = EpiNow2::delay_opts(
                          list(
                            mean = 2.5, mean_sd = 0.5,
                            sd = 0.47, sd_sd = 0.25, max = 30
                          )
                        ),
                        truncation = EpiNow2::trunc_opts(),
                        obs = EpiNow2::obs_opts(),
                        windows = c(14, 7),
                        window_overlap = TRUE,
                        prior_inflation = 1,
                        CrIs = c(0.2, 0.5, 0.9),
                        model = NULL,
                        verbose = interactive(),
                        ...) {
  if (length(windows) > 2) {
    stop("Currently only the use of up to two fitting windows is supported")
  }
  if (length(windows) > 1) {
    if (window_overlap) {
      long_reports <- reports[date <= (max(date) - windows[2])]
    }else{
      long_reports <- reports
    }
    long_burn_in <- sf_set_burn_in(long_reports, window = windows[1])

    long_fit <- EpiNow2::estimate_secondary(
      reports = long_reports,
      burn_in = long_burn_in,
      secondary = secondary,
      delays = delays,
      truncation = truncation,
      obs = obs,
      CrIs = CrIs,
      model = model,
      verbose = verbose,
      ...
    )

    posterior <- sf_summarise_posterior(long_fit, CrIs = CrIs)

    short_args <- sf_update_secondary_args(
      obs = obs, delays = delays, priors = posterior,
      prior_inflation = prior_inflation
    )
    delays <- short_args$delays
    obs <- short_args$obs
    window <- windows[2]
  }else{
    window <- windows[]
  }

  short_burn_in <- sf_set_burn_in(reports, window = window)

  fit <-  EpiNow2::estimate_secondary(
      reports = reports,
      burn_in = short_burn_in,
      secondary = secondary,
      delays = delays,
      truncation = truncation,
      obs = obs,
      CrIs = CrIs,
      model = model,
      verbose = verbose,
      ...
    )

  return(fit)
}
