# Implement estimate_secondary "101"
# March 2022
# Jeremy, James, Juliet, Carl

library(EpiNow2)
library(tidyverse)


?estimate_secondary


dd_flat = readRDS('./data/flat-ts-fixed-proportion.rds')

sec_delays = delay_opts(list(mean = log(14), mean_sd = 0.5, sd =  log(7), sd_sd = 0.25, max = 30))



