# plot output from estimate_secondary
library(tidyverse)
library(patchwork)
#library(ggthemes)
library(scales)
#library(rstan)

.args <- if (interactive()) c(
  file.path('synthetic/outputs/full', 'const_const_const_const.rds'), # input
  file.path('main/data','plotting_functions.RData'), # load functions
  file.path('synthetic/data','const_const_const_const.rds'), # input 2
  file.path('synthetic/outputs/figures', 'const_const_const_const.png') # output
) else commandArgs(trailingOnly = TRUE)

#load the data functions
load(.args[[2]])

#load the predictions
dd_raw <- readRDS(.args[[3]])

#load the raw data
dd_est <- readRDS(.args[[1]]) 

dd_pred <- dd_est$predictions %>% 
  as_tibble()

#combine the raw and prediction data
dd <- left_join(dd_raw, dd_pred, 
                by = c('date', 'primary', 'primary_underlying', 
                       'secondary', 'secondary_underlying'
                       )
                )
#Extract the predicted fraction observed
frac_obs_stan <- round(rstan::summary(dd_est$fit)$summary['frac_obs[1]', 'mean'], 2)

dd <- dd %>% 
  mutate(frac_obs_stan = frac_obs_stan)


# Plot panels
fig_top_panel <- plot_est_sec_out(dat = dd)

fig_bottom_panel <- plot_ratios(dat = dd)

fig_final <- fig_top_panel/fig_bottom_panel

ggsave(tail(.args,1), plot = fig_final, width = 12, height = 8)
