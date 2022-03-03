# Implement estimate_secondary "101"
# March 2022
# Jeremy, James, Juliet, Carl

library(EpiNow2)
library(tidyverse)


# ?estimate_secondary


dd_flat = readRDS('./data/flat-ts-fixed-proportion.rds')

sec_delays = delay_opts(list(mean = log(5), mean_sd = 0.1, sd =  log(1.01), sd_sd = 0.00001, max = 10))


out_flat <- estimate_secondary(reports = dd_flat,  delays= sec_delays, burn_in = 30, )

plot(out_flat)
