# load input data (time series) and additional estimate_secondary() parameters
# from file, run estimate_secondary(), and save raw output to file
library(EpiNow2)
library(tidyverse)

.args <- if (interactive()) c(
  file.path('./synthetic/data', 'flat_const_const_const.rds'), # input
file.path('./synthetic/inputs','param_combos.rds'), 
 file.path('./synthetic/inputs', 'estimation_params.RData'),
  file.path('./synthetic/outputs/full', 'flat_const_const_const.rds') # output
) else commandArgs(trailingOnly = TRUE)

dd <- readRDS(.args[1])

param_table <- readRDS(.args[2])

# loop_over param_table 
# call estimate_secondary on each iteration of the loop 
# save output with name = ./outputs/[scenario_name]_[experiment_number]
# row_wise(groups by row; operations; bind results)
# take param_table
load(.args[3]) # must include variables: sec_delays, obs_process, burn_in_length

#create scenario_table (Scenario for each expermiment)


scenario_table %>% row_wise() %>% 
	scenarios_from_table(param_table) # can create_scenario become a function?

	estimate_secondary(.,
                          delays = delay_opts(),
                          obs = obs_process,
                          burn_in = burn_in_length
                          )
				
out <- estimate_secondary(dd,
                          delays = delay_opts(),
                          obs = obs_process,
                          burn_in = burn_in_length
                          )

saveRDS(out, tail(.args,1))

