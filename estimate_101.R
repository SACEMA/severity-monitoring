# Implement estimate_secondary "101"
# March 2022
# Jeremy, James, Juliet, Carl

library(EpiNow2)
library(tidyverse)


# ?estimate_secondary
# -- setup --

dd_flat = readRDS('./data/flat-ts-fixed-proportion.rds')

sec_delays = delay_opts(list(mean = log(5),
                             mean_sd = 0.00001,
                             sd =  log(1.01),
                             sd_sd = 0.0000001,
                             max = 10
                             ))

obs_no_week = obs_opts(week_effect = FALSE)

sec_type = secondary_opts('incidence')

# run with data + default parameters

out_flat_default <- estimate_secondary(reports = dd_flat)

plot(out_flat_default, primary = TRUE)


# run with data + no week + no delay parameters

out_flat_simple <- estimate_secondary(reports = dd_flat,
                                       obs = obs_no_week,
                                       delays = delay_opts(),
                                      secondary = sec_type)

plot(out_flat_simple, primary = TRUE)

out_flat_simple[['predictions']] %>% ggplot(aes(x = date)) +
  geom_line(aes(y = median)) + 
  geom_line(aes(y = primary), color = 'blue') +
  geom_line(aes(y = secondary), color = 'green')


# specify delays and week effect = FALSE

out_flat <- estimate_secondary(reports = dd_flat[1:80,],
                               # delays= sec_delays,
                               delays= delay_opts(),
                               burn_in = 10,
                               obs = obs_opts(week_effect = FALSE, 
                                              scale = list(mean = 0.1, 
                                                           sd = 0.01)),
                               secondary = sec_type)

plot(out_flat, primary = TRUE)
