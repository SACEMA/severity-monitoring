Common parameters for various dimensions of scenarios for synthesized data generation
Function argument naming convention:
- initial primary incidence (init_primary)
- time of change for primary incidence (tchange1_prim)
- duration of change for primary incidence (dur_change_prim)
- linear rate of change for primary incidence (change_rate_linear_prim)
- exponential growth rate for primary incidence (change_rate_exponential_prim)
- length of the time series (ts_length)

Variable names for parameters:
Generate in synth_parameters.R which saves to synth_parameters.RData



Column names:
primary, secondary, primary_underlying, secondary_underlying
nb: "primary" and "secondary" are the *observed* time series; names as needed for estimate_secondary()


function requirements:
data synthesis goes D1 => D2 => D3 => D4 => add dates
D1: 
	in:
	out: data frame with one column: primary_underlying
D2: 
	in: data frame with one column: primary_underlying
	out: data frame with two columns: primary_underlying, secondary_underlying
D3: 
	in: data frame with two columns: primary_underlying, secondary_underlying
	out: data frame with three columns: primary_underlying, secondary_underlying, primary
D4: 
	in: data frame with three columns: primary_underlying, secondary_underlying, primary
	out: data frame with four columns: primary_underlying, secondary_underlying, primary, secondary
add dates:
	in: data frame with four columns: primary_underlying, secondary_underlying, primary, secondary
	out: data frame with five columns: date, primary_underlying, secondary_underlying, primary, secondary


Function list
1. [] D1 a				:	gen_flat_prim
2. [] D1 b & c			:	gen_linear_prim
3. [] D1 d & e			:	gen_exp_prim
4. [] D2 a				:	gen_const_sec
5. [] D2 b				:	gen_grad_change_sec
6. [] D3 a				:	obs_const_prim
7. [] D3 b				:	obs_grad_change_prim
8. [] D4 a				:	obs_const_sec
9. [] D4 b				:	obs_grad_change_sec


To-Do:
James: 1,2,3,7,9
Jeremy: 4,5,6,8

each to work on separate branch until next meeting