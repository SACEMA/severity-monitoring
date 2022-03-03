# Set up synthetic data 
# March 2022
# Jeremy, James, Juliet, Carl

#Load packages
library(dplyr)
library(tidyr)
library(lubridate)


#Inputs
fixed_delay <- 5 #delay until secondary outcomes become observable
prop_to_seconday <- 0.1 #fixed proportion of primary outcomes that become secondary outcomes
flat_primary <- 10 #fixed number of primary outcomes per day


#Create the fake data
reported_data <- tibble(date = ymd('2021-01-01') + 0:99,
           primary = rep(10, length(date)),
           secondary = c(rep(NA, times = fixed_delay), 
                         rep(prop_to_seconday*flat_primary, 
                             times = (length(date)-fixed_delay)
                             )
                         )
           )


reported_data$secondary = replace_na(reported_data$secondary, 0)

saveRDS(reported_data, './data/flat-ts-fixed-proportion.rds')
