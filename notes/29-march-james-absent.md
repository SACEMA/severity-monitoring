to-do
- what we want is a "statement" of the *fractions* involved. i.e. secondary / primary
- troubleshoot the complex figure
	- also try with different priors
	- may also be (we think, Carl has emailed Sam to confirm) that estimate_secondary() is only fittig *one* ratio per window i.e. per fit
	- up_twovals_twovals_const
- look for where in the actual output the fraction (sec/primary) is reported 
	- compare this with the fraction we calculate between primary_observed ("primary") and secondary_estimated (i.e. outputs from estimate_secondary)


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

