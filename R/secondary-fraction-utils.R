#' Command line interface for secondary fraction estimation
#'
#' Define the CLI interface and return the parsed arguments
#'
#' @param args_string String (optional) of command line flags to simulate CLI
#' interface when running interactively.
#'
#' @return List of arguments
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
   stop("Burn in must be greater than or equal to ", min_burn_in)
  }
  return(burn_in)
}

sf_summarise_posterior <- function(fit, CrIs = c(0.05, 0.5, 0;.95),
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
        obs$ <- c(signif(phi$mean, 3), signif(phi$sd, 3))
      }
    }
  }
  return(list(obs = obs, delays = delays))
}

sf_estimate <- function(reports,
                        secondary = EpiNow2::secondary_opts(),
                        delays = delay_opts(
                          list(
                            mean = 2.5, mean_sd = 0.5,
                            sd = 0.47, sd_sd = 0.25, max = 30
                          )
                        ),
                        truncation = EpiNow2::trunc_opts(),
                        obs = EpiNow2::obs_opts(),
                        windows = c(7*6, 7*2),
                        window_overlapping = TRUE,
                        prior_inflation = 2,
                        CrIs = c(0.2, 0.5, 0.9),
                        model = NULL,
                        verbose = interactive(),
                        ...) {
  if (length(windows) > 2) {
    stop("Currently only the use of up to two fitting windows is supported")
  }
  if (length(windows) > 1) {
    if (window_overlapping) {
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
      obs = obs, delays = delays, prior_inflation = prior_inflation
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
