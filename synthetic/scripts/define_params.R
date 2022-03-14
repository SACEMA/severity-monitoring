# specify parameters, for synthesizing data and running estimate_secondary on 
# the synthesized data, and save these to file
# March 2022
## Question: would it be better to save the parameters (in and out) as single RDS
# containing a list (e.g.) with "in" and "out" sublists?

library(EpiNow2)

# params for data generation
in.fixed_delay <- 5 #delay until secondary outcomes become observable
in.prop_to_secondary <- 0.1 #fixed proportion of primary outcomes that become secondary outcomes after a first time point
in.prop_to_secondary2 <- 0.2 #fixed proportion of primary outcomes that become secondary outcomes after a second time point
in.flat_primary <- 10 #fixed number of primary outcomes per day
start_date <- '2022-01-01' #First date of time series
ts_length <- 50
tchange_prim <- round(ts_length/2) #time to change observed secondary outcomes from ratio1 to ratio 2

# (matching) params for estimate_secondary
sec_delays = delay_opts(list(mean = log(5),
                             mean_sd = 0.00001,
                             sd =  log(1.01),
                             sd_sd = 0.0000001,
                             max = 10
                             ))

obs_process <- obs_opts(week_effect = FALSE,
                scale = list(mean = 1,
                             sd = 10))

burn_in_length <- 10

save(in.fixed_delay,
       in.prop_to_seconday,
       in.prop_to_seconday,
       sec_delays,
       obs_process,
       burn_in_length,
     file = file.path("synthetic/data", "params.RData"))

