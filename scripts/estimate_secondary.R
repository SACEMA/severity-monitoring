# load input data (time series) and additional estimate_secondary() parameters
# from file, run estimate_secondary(), and save raw output to file
library(EpiNow2)
library(tidyverse)

.args <- if (interactive()) c(
  file.path('synthetic/data', 'flat_constant.rds'), # input
  file.path('synthetic/data', 'params.rds'),
  file.path('synthetic/output/raw', 'flat_constant.rds') # output
) else commandArgs(trailingOnly = TRUE)

dd <- readRDS(.args[1])
load(.args[2]) # must include variables: sec_delays, obs_process, burn_in_length

out <- estimate_secondary(dd,
                          delays = sec_delays,
                          obs = obs_process,
                          burn_in = burn_in_length
                          )

saveRDS(out, file = .args[3])