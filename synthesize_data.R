library(dplyr)
library(lubridate)


#inputs
fixed_delay <- 5 #delay until secondary outcomes become observable
prop_to_seconday <- 0.1 #fixed proportion of primary outcomes that become secondary outcomes
flat_primary <- 10 #fixed number of primary outcomes per day



reported_data <- tibble(date = ymd('2021-01-01') + 0:99,
           primary = rep(10, length(date)),
           secondary = c(rep(NA, times = fixed_delay), 
                         rep(prop_to_seconday*flat_primary, 
                             times = (length(date)-fixed_delay)
                             )
                         )
           )
