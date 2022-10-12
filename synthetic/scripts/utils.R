# utils!

.args <- if (interactive()) {
  c("./data/utils.RData")
} else {
  commandArgs(trailingOnly = TRUE)
}

extract_scenario_number_from_filename <- function(filename) {
  sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(filename)) %>%
    sub(pattern = "scenario_", replacement = "")
}


save(list = ls(), file = tail(.args, 1))
