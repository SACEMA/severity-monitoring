# Implement estimate_secondary "101"
# March 2022
# Jeremy, James, Juliet, Carl

library(EpiNow2)
library(tidyverse)

source('./functions/plotting_functions.R')
source('./functions/synth_data_functions.R')


# ?estimate_secondary
# -- setup --

dd_flat = readRDS('./synthetic/data/flat_constant.rds')


# we try to mimick our synthesized data, with a delay distribution 
# as close as possible to a fixed delay of 5 days

sec_delays = delay_opts(list(mean = log(5),
                             mean_sd = 0.00001,
                             sd =  log(1.01),
                             sd_sd = 0.0000001,
                             max = 10
                             ))

#week effect is fitted even when not present in data,
# so remove when using non-weekly synth data

obs_no_week = obs_opts(week_effect = FALSE)

# plot delay distribution
tmp <- rlnorm( 100000 , mean = log(5), sd = log(1.01))
mean(tmp)
hist(tmp)

# run with data + default parameters

out_flat_default <- estimate_secondary(reports = dd_flat)

plot(out_flat_default, primary = TRUE)


# run with data + no week + no delay parameters

out_flat_simple <- estimate_secondary(reports = dd_flat,
                                       obs = obs_no_week,
                                       delays = delay_opts())

# plot(out_flat_simple, primary = TRUE)

plot_est_sec_out(out_flat_simple[['predictions']], plot_title = "obs_opts week_effect = FALSE; default otherwise")

# out_flat_simple[['predictions']] %>% ggplot(aes(x = date)) +
  # geom_line(aes(y = median)) + 
  # geom_line(aes(y = primary), color = 'blue') +
  # geom_line(aes(y = secondary), color = 'green')


# specify delays and week effect = FALSE

out_flat <- estimate_secondary(reports = dd_flat,
                               # delays= sec_delays,
                               delays = delay_opts(),
                               burn_in = 10,
                               obs = obs_opts(week_effect = FALSE,
                                               scale = list(mean = 1,
                                                           sd = 10)))

# plot(out_flat, primary = TRUE)



out_flat[['predictions']] %>% ggplot(aes(x = date)) +
  geom_line(aes(y = median, color = "Median secondary estimate"), lwd=2) + 
  geom_line(aes(y = mean, color = "Mean secondary estimate"), lty = 3, lwd =2)+
  geom_line(aes(y = primary, color = "Primary data")) +
  geom_line(aes(y = secondary, color = "Secondary data")) +
  theme(legend.title = element_blank()) +
  labs(y="Counts", x = "Date", title = "obs_opts has scale set to: mean = 1, sd = 10")
