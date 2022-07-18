#'  Tools for estimating the secondary fraction using iterative convolution
#'  models
#'
#' See R/secondary-fraction.R for a command line tool implementation of
#' these functions.
#'
#' Author: Sam Abbott
#' Licence: MIT
#' Last modified: 2022-05-14

#' Calculate the probability mass function of a discretised
#' log normal distribution
#' @author Sam Abbott
sf_discretised_lognormal_pmf <- function(
  meanlog, sdlog, max_d, reverse = FALSE
) {
  pmf <- plnorm(1:max_d, meanlog, sdlog) -
    plnorm(0:(max_d - 1), meanlog, sdlog)
  pmf <- as.vector(pmf) / as.vector(plnorm(max_d, meanlog, sdlog))
  if (reverse) {
    pmf <- rev(pmf)
  }
  return(pmf)
}

#' Calculate the convolution of a probability mass function of a 
#' discretised log normal distribution and a vector of counts
#' @author Sam Abbott
sf_discretised_lognormal_pmf_conv <- function(
  x, meanlog, sdlog
) {
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
#' @author Sam Abbott
#' @examples
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
sf_gp_simulate <- function(
  data,
  type = c("incidence", "prevalence"),
  family = c("poisson", "negbin", "none"),
  delay_max = 30, ...
) {
  type <- match.arg(type)
  family <- match.arg(family)
  data <- data.table::copy(as.data.table(data))
  data[, index := 1:.N]
  # apply scaling
  data[, scaled := scaling * primary]
  # add convolution
  data[,
    conv := purrr::pmap_dbl(list(i = index, m = meanlog, s = sdlog),
     function(i, m, s) {
       sf_discretised_lognormal_pmf_conv(
         scaled[max(1, i - delay_max):i], meanlog = m, sdlog = s
        )
  })]
  # TODO hand this off to distinct functions for incidence vs prevalence
  # build model
  if (type == "incidence") {
    data[, secondary := conv ]
  } else if (type == "prevalence") {
    data[1, secondary := scaled ]
    for (i in 2:nrow(data)) {
      index <-
        data[c(i - 1, i)][, secondary := shift(secondary, 1) - conv]
      index <- index[secondary < 0, secondary := 0]
      data[i, ] <- index[2][, secondary := secondary + scaled]
    }
  }
  # check secondary is greater that zero
  data[secondary < 0, secondary := 0]
  data <- data[!is.na(secondary)]
  # apply observation model
  # TODO: DRY; construct function in if-else, then apply once
  if (family == "poisson") {
    data[, secondary := purrr::map_dbl(secondary, ~ rpois(1, .)) ]
  } else if (family == "negbin") {
    data[, secondary := purrr::map_dbl(secondary, ~ rnbinom(1, mu = .), ...) ]
  }

  data[, secondary := as.integer(secondary)]

  return(data)
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
#' @examples
#' # Defaults
#' sf_cli_interface()
#'
#' # Override defaults
#' sf_cli_interface(c("-l", "-a"))     
sf_cli_interface <- function(args_string = NA) {
  option_list <- list(
    optparse::make_option(c("-o", "--observations"),
      type = "character",
      action = "store",
      default = 'data/example-incidence.rds',
      help =
      "Path to rds of observations including the following columns:
       'date', 'primary', and 'secondary'. Defaults to using an 
       example simulated data set 'data/example-incidence.rds'."
    ),
    optparse::make_option(c("--target_date"),
      type = "character",
      action = "store",
      default = NULL,
      help =
      "A target date (in YYYY-MM-DD format) to use in estimation rather than using the maximum date in the observed data."
    ),
    optparse::make_option(c("--path"),
      type = "character",
      action = "store",
      default = tempdir(),
      help = "Path to a folder in which to save results. Use the tmp directory by default." # nolint
    ),
    optparse::make_option(
      c("-r", "--relative_performance"),
      action = "store_true",
      default = FALSE,
      help = "Save relative performance of target versus baseline"
    ),
    optparse::make_option(
      c("-s", "--scores"),
      action = "store_true",
      default = FALSE,
      help = "Save scores per target observation"
    ),
    optparse::make_option(
      c("--summary"),
      action = "store_true",
      default = FALSE,
      help = "Save posterior summaries"
    ),
    optparse::make_option(
      c("--obs_preds"),
      action = "store_true",
      default = FALSE,
      help = "Save posterior samples of the observations"
    ),
    optparse::make_option(
      c("--fit"),
      action = "store_true",
      default = FALSE,
      help = "Save fit objects"
    ),
    optparse::make_option(
      c("--plot"),
      action = "store_true",
      default = FALSE,
      help = "Save plots"
    ),
    optparse::make_option(
      c("-a", "--all"),
      action = "store_true",
      default = FALSE,
      help = "Save all output"
    ),
    optparse::make_option(
      c("-c", "--cores"),
      type = "integer",
      action = "store",
      default = 4,
      help = "Number of cores to use when executing the chains in parallel,
          which defaults to 4. In general we recommend setting the number of
          cores to the number of chains used if possible. Note that more cores
          than chains will have no impact on fitting times."
    ),
    optparse::make_option(c("-d", "--delay"),
      type = "character",
      action = "store",
      default = "data/weakly-informed-delays.rds",
      help =
      "Path to an rds defining the delay distribution prior. By default
      uses 'data/weakly-informed-delays.rds' which is a weakly informed prior"
    ),
    optparse::make_option(c("-t", "--observation_type"),
      type = "character",
      action = "store",
      default = "incidence",
      help =
      "Defines the type of observation. Can be 'incidence' or 'prevalence'.
       Defaults to 'incidence'."
    ),
    optparse::make_option(c("--family"),
      type = "character",
      action = "store",
      default = "negbin",
      help =
      "Observation family to use in the convolution model. Defaults to negative
       binomial ('negbin') with Poisson ('poisson') also being supported"
    ),
    optparse::make_option("--dow",
      action = "store_true",
      default = FALSE,
      help =
      "Should a day of the week effect be included in the observation model."
    ),
    optparse::make_option("--scale_mean",
      type = "numeric",
      action = "store",
      default = 0.5,
      help =
      "Define the prior mean for the secondary fraction
       prior. By default uses an uninformed prior N(0.5, 0.25)."
    ),
    optparse::make_option("--scale_sd",
      type = "numeric",
      action = "store",
      default = 0.25,
      help =
      "Define the prior standard deviation for the secondary fraction
       prior. By default uses an uninformed prior N(0.5, 0.25)."
    ),
    optparse::make_option(c("-w", "--window"),
      type = "numeric",
      action = "store",
      default = 14,
      help =
      "Define the fitting window for the target model.
       By default this is 14 days."
    ),
    optparse::make_option(c("-b", "--baseline_window"),
      type = "numeric",
      action = "store",
      default = 28,
      help =
      "Define the fitting window for the baseline model.
       By default this is 28 days."
    ),
    optparse::make_option("--windows_overlap",
      action = "store_true",
      default = FALSE,
      help =
      "Should target and baseline windows overlap or cover contiguous time
       periods."
    ),
    optparse::make_option("--priors",
      type = "character",
      action = "store",
      default = NULL,
      help =
      "A data.frame of priors as required by sf_estimate. This may be used to
      help inform the initial baseline model. By default this is not used."
    ),
    optparse::make_option(c("i", "--independent"),
      action = "store_true",
      default = FALSE,
      help =
      "Should the target and baseline models be fit independently or should the
      target model use the posterior from the baseline model as a prior?"
    ),
    optparse::make_option("--prior_inflation",
      type = "numeric",
      action = "store",
      default = 1,
      help =
      "What factor should the standard deviation of posteriors used as priors
      be inflated by. Defaults to 1 (i.e no inflation)."
    ),
    optparse::make_option("--adapt_delta",
      type = "numeric",
      action = "store",
      default = 0.99,
      help =
      "Set the minimum size of steps used in HMC fitting. See ?rstan::sampling 
      for more detail. Defaults to 0.99."
    ),
    optparse::make_option("--max_treedepth",
      type = "numeric",
      action = "store",
      default = 12,
      help =
      "Set the maximum size of trees to construct in HMC fitting.
       See ?rstan::sampling for details. Defaults to 12."
    ),
    optparse::make_option(
      c("-v", "--verbose"),
      action = "store_true",
      default = FALSE,
      help = "Print verbose output "
    ),
    optparse::make_option(
      c("-l", "--loud"),
      action = "store_true",
      default = FALSE,
      help = "Print verbose output including fitting information"
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

#' Set the model burn in period for estimate_secondary
#'
#' @param obs A data.frame of observations counting a `date` variable
#' in date format.
#'
#' @param window Numeric, defaults to 14. The fitting window in days.
#'
#' @param min_burn_in Numeric, defaults to 14. The minimum amount of
#' data in the burn in period.
#'
#' @error Logical, defaults to `FALSE`. If `TRUE` then this function fails
#' if the specified burn in period is too short. If `FALSE` then the burn in
#' period is set to the minimum burn in period (`min_burn_in`).
#' @return A numeric value indicating the burn in period
#' @export
#' @author Sam Abbott
sf_set_burn_in <- function(obs, window = 14, min_burn_in = 14, error = FALSE) {
  burn_in <-  as.integer(max(obs$date) - min(obs$date)) - window + 1
  if (burn_in < min_burn_in) {
   if (error) {
      stop("Burn in must be greater than or equal to ",
           min_burn_in,
           " but with the currently specified window of ",
           window, " is ", burn_in
      )
   }else{
     warning("Specified window is too large for the data. Using ",
             as.integer(max(obs$date) - min(obs$date)) - min_burn_in,
             " instead which allows for a minimum burn in of",
             min_burn_in
     )
     burn_in <- min_burn_in
   }
  }
  return(burn_in)
}

#' Summarise the posterior of a estimate_secondary model fit.
#'
#' @param fit A fit object from estimate_secondary
#'
#' @param CrIs A numeric vector of credible intervals to return. 
#' Defaults to the 5%, 50%, and 95% credible intervals..
#'
#' @return A `data.table` of the posterior summary.
#' @export
#' @author Sam Abbott
sf_summarise_posterior <- function(fit, CrIs = c(0.05, 0.5, 0.95)) {
  posterior <- EpiNow2::extract_stan_param(
    fit$fit, CrIs = CrIs
  )
  return(posterior[])
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
          "Replacing specified priors with those from the passed in prior 
           data frame"
        )
      }
      priors <- priors[, sd := sd * prior_inflation]

      # replace scaling if present in the prior
      scale <- priors[grepl("frac_obs", variable)]
      if (nrow(scale) > 0) {
        obs$scale$mean <- as.array(signif(scale$mean, 3))
        obs$scale$sd <- as.array(signif(scale$sd, 3))
      }
      # replace delay parameters if present
      delay_mean <- priors[grepl("delay_mean", variable)]
      delay_sd <- priors[grepl("delay_sd", variable)]
      if (nrow(delay_mean) > 0) {
        if (is.null(delays$delay_mean_mean)) {
         warning(
           "Cannot replace delay distribution parameters as no default has been
            set"
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

#' Extract posterior samples from a estimate_secondary model fit.
#'
#' @param fit An `estimate_secondary` object.
#'
#' @return A `data.table` with the following variables:
#' sample, value, and date.
#' @export
#' @author Sam Abbott
sf_extract_secondary_samples <- function(fit, obs) {
  samples <- rstan::extract(fit$fit, "sim_secondary")
  samples <- data.table::as.data.table(samples)[, sample := 1:.N]
  samples <- data.table::melt.data.table(
    samples, id.vars = c("sample"), variable.name = "time"
  )
  samples[, time := as.numeric(gsub("sim_secondary.V", "", time))]
  samples <- samples[order(sample, time)]
  samples <- samples[, date := obs$date, by = "sample"]
  samples[, time := NULL]
  return(samples[])
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
#' @param windows A numeric vector of fitting windows to use. The model is fit
#' to the data in each window using the posterior as the prior for the next
#' window. Currently this is limited to two windows. The default is a window of
#' 4 weeks followed by a window of 2 week.
#'
#' @param prior_inflation A numeric value to increase the prior standard
#' deviation by. This is useful if the prior is not sufficiently diffuse to
#' capture likely changes in subsequent windows. The default is 1.
#'
#' @param windows_overlap A logical value that defaults to  `FALSE`. Should
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
#'
#' plot(inc$forecast_secondary$baseline, new_obs = cases[1:60],  primary = TRUE)
#' plot(inc$forecast_secondary$target, primary = TRUE)
#'
#' sf_summarise_posterior(inc)
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
#' plot(prev$forecast_secondary$baseline, cases[1:100], primary = TRUE)
#' plot(prev$forecast_secondary$target, primary = TRUE)
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
                        windows = c(28, 14),
                        window_overlap = FALSE,
                        priors = NULL,
                        prior_from_posterior = TRUE,
                        prior_inflation = 1,
                        CrIs = c(0.2, 0.5, 0.9),
                        model = NULL,
                        verbose = interactive(),
                        ...) {
  if (!length(windows) == 2) {
    stop("Currently only the use of two fitting windows is supported")
  }
  if (!window_overlap) {
    long_reports <- reports[date <= (max(date) - windows[2])]
  }else{
    long_reports <- reports
  }
  long_burn_in <- sf_set_burn_in(long_reports, window = windows[1])

  if (verbose) {
    message("Fitting baseline window")
  }

  if (!is.null(priors)) {
    long_args <- sf_update_secondary_args(
      obs = obs, delays = delays, priors = priors,
      prior_inflation = prior_inflation,
      verbose = verbose
    )
    delays <- long_args$delays
    obs <- long_args$obs
  }

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

  out <- list(
    "estimate_secondary" = list("baseline" = long_fit)
  )

  if (!window_overlap) {
    if (verbose) {
      message("Forecasting target window using baseline")
    }

    long_forecast <- EpiNow2::forecast_secondary(
      long_fit,  data.table::copy(reports)[date > (max(date) - windows[2])][,
       value := primary
      ]
    )
    out$forecast_secondary$baseline <- long_forecast
    out$posterior_predictions <- list("baseline" = long_forecast$samples)
  }else{
    out$forecast_secondary <- list("baseline" = long_fit)
    out$posterior_predictions <- list(
      "baseline" = sf_extract_secondary_samples(
        long_fit, long_reports[date > (min(date) + long_burn_in)]
      )[
        date > (max(date) - windows[2])
      ]
    )
  }

  if (prior_from_posterior) {
    if (verbose) {
      message("Extracting posterior estimates and updating fitting arguments")
    }
    posterior <- sf_summarise_posterior(long_fit, CrIs = CrIs)

    short_args <- sf_update_secondary_args(
      obs = obs, delays = delays, priors = posterior,
      prior_inflation = prior_inflation,
      verbose = verbose
    )
    delays <- short_args$delays
    obs <- short_args$obs
  }

  short_burn_in <- sf_set_burn_in(reports, window = windows[2])

  if (verbose) {
    message("Fitting the target window")
  }

  short_fit <- EpiNow2::estimate_secondary(
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

  out$estimate_secondary$target <- short_fit
  out$forecast_secondary$target <- short_fit
  out$posterior_predictions$target <-
    sf_extract_secondary_samples(
      short_fit,
      reports[date > (max(date) - windows[2])]
    )
  out$posterior_predictions <- data.table::rbindlist(
    out$posterior_predictions, idcol = "model"
  )
  return(out)
}

 #' Extract summarised posterior predictions
 #'
 #' @param estimates The output from `sf_estimate()`
 #'
 #' @return A data frame of predictions by model and type.
 #' @export
 #' @author Sam Abbott
sf_extract_summarised_predictions <- function(estimates) {
  in_sample <- purrr::map(estimates$estimate_secondary,
    ~ .$predictions
  )
  names(in_sample) <- names(estimates$estimate_secondary)
  in_sample <- data.table::rbindlist(in_sample, idcol = "model")

  out_sample <- purrr::map(estimates$forecast_secondary,
    ~ .$predictions
  )
  names(out_sample) <- names(estimates$forecast_secondary)
  out_sample <- data.table::rbindlist(out_sample, idcol = "model", fill = TRUE)

  preds <- rbind(
    in_sample[, type := "in sample"],
    out_sample[, type := "out of sample"]
  )
  return(preds)
}

 #'Plot summarised posterior predictions
 #'
 #' @param predictions The output from `sf_extract_summarised_predictions()`
 #'
 #' @param ... Pass additional arguments to `ggplot2::aes()`
 #' 
 #' @return A `ggplot2` plot.
 #' @export
 #' @author Sam Abbott
sf_plot_pp <- function(predictions, ...) {
  plot <- 
   ggplot2::ggplot(
     predictions[!is.na(mean)]
   ) +
   ggplot2::aes(x = date, y = secondary, ...) +
   ggplot2::geom_col(
     fill = "grey", col = "white", show.legend = FALSE, na.rm = TRUE,
     position = "identity"
   )

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

save(list = ls(), file = .fromArgs(here::here("data", "sf_gp_utils.rda")))