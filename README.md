# COVID-19 severity routine monitoring

## Project description

## Directory structure

## Instructions for how-to with synthesized data

## Synthesized data

### Other ingredients:
- Delay from primary to secondary (primary time series (TS) reference event to secondary TS reference event)
- need to specify whether we refer to time according to primary or secondary reference event

### Dimensions:
1. True primary time series
2. True primary -> secondary process: leads to true secondary time series (* always includes delay)
3. Observation process on true primary time series: leads to observed primary time series
4. Observation process on true secondary time series

### Values to put in each dimension:

1. Dimension 1 (primary time series):

	a) flat

		- value
		
	(b) increasing linear	

		- initial value
		- time of change (if >0 then TS is constant at initial value before time of change)
		- linear rate of increase
		
	c) decreasing linear

		- initial value
		- time of change (if >0 then TS is constant at initial value before time of change)
		- linear rate of increase

	d) exponential growth

		- inital value
		- exponential growth rate

	e) exponential decay

		- initial value
		- exponential growth rate
		
2. Dimension 2 (process transforming true primary TS to true secondary TS):

	a) constant proportion with fixed delay

		- proportion
		- delay

	b) gradual change in proportion (two constant values; change over some time period)

		- proportion 1
		- proportion 2
		- time that change starts
		- duration of change

3. Dimension 3 (observation process on primary TS):

	a) observe constant proportion

		- proportion
		- delay

	b) gradual change in proportion observed (constant until t1, decrease until t2, then constant)

		- proportion 1
		- proportion 2
		- time that change starts
		- duration of change

4. Dimension 4 (obervation process on secondary TS):

	a) observe constant proportion

		- proportion
		- delay

	b) gradual change in proportion observed (constant until t1, decrease until t2, then constant)

		- proportion 1
		- proportion 2
		- time that change starts
		- duration of change

#### Next iteration:
- Stochastic version of D3: a) and b) and D4: a) and b) and stochastic delay for dimension 2


### Inputs to estimate_secondary():

1. data
2. obs_opts() needs scale(mean = proportion_D2 times proportion_D4, sd = mean)
3. delay_opts():
- default for now
## Outputs:
1. plots (plot b is more important):
a) data: synthesized data raw
includes TS of: primary true, primary observed, secondary true, secondary observed
b) data: data observed, outputs from estimate_secondary
includes TS of: primary observed, secondary observed, secondary_estimated with 97.5% CI's
2. Values
		
## Rmarkdown document:
1. 3 sentence intro
2. for each chosen data scenario (i.e. combination of 4 dimensions with numbers chosen)
	- specification/description
	- plot

## Notes / observations to highlight about choices we make:
1. We are not including any delay in the observation process. i.e. "true" time series are according to time of observation
2. Stochasticity will only be added to the observation processes, and hasn't been planned out just yet.
