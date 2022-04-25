# to-do

- [ ] update makefile to produce d1_d2_d3_d4.png targets and dependencies with pattern matching (i.e. have one set of pattern-matching targets/rules for making all figures)
- [ ] update makefile to use variable names for folders
- [ ] check / potentially rearrange locations for scripts involved in synthetic stuff. synthetic stuff should all be ./synthetic. only common stuff like plot_script.R and est_sec.R should be in ./scripts. common functions should be defined in ./functions/function_group.R and common defined functions should be saved in ./functions/function_group.RData. Synthetic-specific functions should be defined in ./synthetic/scripts and their definitions (function_group.RData files) should be saved in ./synthetic/data
- [ ] fix directory references in makefile and in if(interactive()) within R scripts
- [ ] change "list-names" of "function-names" in synth_data_functions.R from alphabet to up/down/const/etc naming convention
	- [ ] ammend and check ./synthetic/scenario_list.md for accuracy. i.e. remove alphabet shortcuts, keep table of names, check table of names for consistency with synth_data_functions.R
- [ ] troubleshoot the complex figure
	- [ ] also try with different priors
	- sidenote: may also be (we think, Carl has emailed Sam to confirm) that estimate_secondary() is only fittig *one* ratio per window i.e. per fit
	- [ ] try making up_twovals_twovals_const
- [ ] (don't over-commit) look for where in the est_sec output the fraction (sec/primary) is reported 
	- [ ] compare this with the fraction we calculate between primary_observed ("primary") and secondary_estimated (i.e. outputs from estimate_secondary)

	- plot to-dos
- plot underlying pieces (true primary and true secondary) (i.e. include all four "dimensions" on top panel)
- plot "computed-from-outputs" sec/primary fraction and sec_observed/sec_underlying fraction + others (secondary_fraction estimates from STAN)
- plot existing (top-panel) plot with y-axis on a log scale
- outputs of estimate_secondary should have model outputs for params (e.g. ratios, delays, ..)

	- minor plot improvements:
- center month labels on ticks
- include ticks for all months

overall: two or if necessary more plots. upper panel similar to existing plot with underlying ts'es included + log-scale y-axis. lower panel(s) showing inputs and model outputs of delays and fractions

plan/landscape
- rolling window of fractions 
- compare between windows
- think about window size and what will be suitable
- what we want is a "statement" of the *fractions* involved. i.e. secondary / primary
