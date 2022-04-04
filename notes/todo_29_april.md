# to-do

- [x] update makefile to produce d1_d2_d3_d4.png targets and dependencies with pattern matching (i.e. have one set of pattern-matching targets/rules for making all figures) (Jeremy: Draft + James review)
- [nd] update makefile to use variable names for folders (Jeremy)
- [x] check / potentially rearrange locations for scripts involved in synthetic stuff. synthetic stuff should all be ./synthetic. only common stuff like plot_script.R and est_sec.R should be in ./main/scripts. common functions should be defined in ./main/functions/function_group.R and common defined functions should be saved in ./main/data/function_group.RData. Synthetic-specific functions should be defined in ./synthetic/scripts and their definitions (function_group.RData files) should be saved in ./synthetic/data (Together)
	- [] fix directory references in makefile and in if(interactive()) within R scripts (Jeremy)
- [] change "list-names" of "function-names" in synth_data_functions.R from alphabet to up/down/const/etc naming convention (James)
	- [] ammend and check ./synthetic/scenario_list.md for accuracy. i.e. remove alphabet shortcuts, keep table of names, check table of names for consistency with synth_data_functions.R (James)
- [x] troubleshoot the complex figure (Together/Jeremy) A: Sec_obs doesn't depend on primary observation process (D1 -> D3)
	- [] also try with different priors (Jeremy)
	- sidenote: may also be (we think, Carl has emailed Sam to confirm) that estimate_secondary() is only fittig *one* ratio per window i.e. per fit
	- [] try make up_twovals_twovals_const.png (defer until top stuff done/James)
- [] (don't over-commit) look for where in the est_sec output the fraction (sec/primary) is reported (James)
	- [] compare this with the fraction we calculate between primary_observed ("primary") and secondary_estimated (i.e. outputs from estimate_secondary) (James)

##	plot to-dos
- [] plot underlying pieces (true primary and true secondary) (i.e. include all four "dimensions" on top panel) (James)
	- [] plot existing (top-panel) plot with y-axis on a log scale (James)
- [] plot "computed-from-outputs" sec/primary fraction and sec_observed/sec_underlying fraction + others (secondary_fraction estimates from STAN) (James)
	- [] plot input ratios (from data generation process; dashed lines; these are the ratios that transform one dimension to another. i.e. secondary generation (D1 -> D2), primary observation (D1 -> D3), and secondary observation (D2 -> D4)) and output ratio (secondary_estimated/primary_observed). include fitted and input delays (input delays = delays used in data generation) in figure label/title

### minor plot improvements:
- [] center month labels on periods which they refer to
- [] include ticks for every 100 cases
- [] include ticks for all months

overall: two or if necessary more plots. upper panel similar to existing plot with underlying ts'es included + log-scale y-axis. lower panel(s) showing inputs and model outputs of delays and fractions

plan/landscape
- rolling window of fractions 
- compare between windows
- think about window size and what will be suitable
- what we want is a "statement" of the *fractions* involved. i.e. secondary / primary
