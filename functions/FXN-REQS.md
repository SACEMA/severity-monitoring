# Common parameters for various dimensions of scenarios for synthesized data generation

## Function argument naming convention:
- initial primary incidence (init_primary)
- time of change for primary incidence (tchange1_prim)
- duration of change for primary incidence (dur_change_prim)
- linear rate of change for primary incidence (change_rate_linear_prim)
- exponential growth rate for primary incidence (change_rate_exponential_prim)
- length of the time series (ts_length)
- proportion sec gen (prop_sec_gen)
- proportion sec obs (prop_sec_obs)
- proportion primary obs (prop_prim_obs)
- prop2_sec_gen
- prop2_sec_obs
- prop2_prim_obs
- delay_sec_gen
- delay_prim_obs
- delay_sec_obs
- tchange_sec_gen
- tchange_sec_obs
- tchange_prim_obs
- dur_change_sec_gen
- dur_change_sec_obs
- dur_change_prim_obs

## Variable names for parameters:
Generate in synth_parameters.R which saves to synth_parameters.RData



## Column names:
primary, secondary, primary_underlying, secondary_underlying

nb: "primary" and "secondary" are the *observed* time series; names as needed for estimate_secondary()


## Function requirements:

data synthesis goes D1 => D2 => D3 => D4 => add dates

D1(): 
	
	in: initial values to generate primary TS
	
	out: data frame with one column: primary_underlying
	
D2(): 
	
	in: data frame with one column: primary_underlying
	
	out: data frame with two columns: primary_underlying, secondary_underlying
	
D3(): 
	
	in: data frame with two columns: primary_underlying, secondary_underlying
	
	out: data frame with three columns: primary_underlying, secondary_underlying, primary
	
D4(): 
	
	in: data frame with three columns: primary_underlying, secondary_underlying, primary
	
	out: data frame with four columns: primary_underlying, secondary_underlying, primary, secondary
	
add dates():
	
	in: data frame with four columns: primary_underlying, secondary_underlying, primary, secondary
	
	out: data frame with five columns: date, primary_underlying, secondary_underlying, primary, secondary

- [x] D1 a: gen_flat_prim_D1()
- [x] D1 b: gen_linear_up_prim_D1()
- [x] D1 c: gen_linear_down_prim_D1()
- [x] D1 d: gen_exp_up_prim_D1()
- [x] D1 e: gen_exp_down_prim_D1()
- [x] D2 a: gen_const_sec_D2()
- [x] D2 b: gen_grad_change_sec_D2()
- [x] D3 a: obs_const_prim_D3()
- [x] D3 b: obs_grad_change_prim_D3()
- [x] D4 a: obs_const_sec_D4()
- [x] D4 b: obs_grad_change_sec_D4()


## To Do
- [ ] 40 make targets (test bed data scenarios)
- [ ] makefile for ./synthetic (will refer to some general-purpose scripts but synthetic needs its own makefile to avoid a mess)

James:
- [x] add wrapper functions for increasing and decreasing linear and exponential primary series
- [x] save function definitions to an a .RData file for later use  
- [ ] define_synth_params.R creates ./synthetic/data/synth_params.RData (note: check define_synth_params_test.R and see if this works)

Jeremy:
- [x] write create_scenario.R: takes in 7 arguments: 1-4 specify dimensions, 5 specifies parameters, 6 specifies function-definition-file, 7 specifies scenario filename
- [x] estimate_secondary.R reads  synth_params.RData and <scenario_name>.rds (defined in makefile) and saves /synthetic/outputs/full/<scenario_name>_out.rds
- [x] plot_sec_est.R reads scenario_name_out.rds and plots, then saves /synthetic/outputs/figures/scenario_name.png


Work plan: each to work on separate branch and consolidate/merge after review at next meeting
