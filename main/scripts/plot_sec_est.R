# plot output from estimate_secondary
library(tidyverse)
.args <- if (interactive()) c(
  file.path('synthetic/outputs/full', 'flat_const_const_const.rds'), # input
  file.path('functions','plotting_functions.RData'), # load functions
  file.path('synthetic/data','flat_const_const_const.rds'), # input 2
  file.path('synthetic/outputs/figures', 'flat_const_const_const.png') # output
) else commandArgs(trailingOnly = TRUE)

load(.args[[2]])

dd_in <- readRDS(.args[[3]])
dd <- readRDS(.args[[1]])

fig <- plot_est_sec_out(predictions = dd[['predictions']], data_raw = dd_in)

ggsave(tail(.args,1), plot = fig, width = 12, height = 8)
