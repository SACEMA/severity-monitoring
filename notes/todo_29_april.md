# To-do
- [x] plot existing (top-panel) plot with y-axis on a log scale (James)
- [x] plot "computed-from-outputs" sec/primary fraction and sec_observed/sec_underlying fraction + others (secondary_fraction estimates from STAN) (James)
- [ ] plot input ratios (from data generation process; dashed lines; these are the ratios that transform one dimension to another. i.e. secondary generation (D1 -> D2), primary observation (D1 -> D3), and secondary observation (D2 -> D4)) and output ratio (secondary_estimated/primary_observed). include fitted and input delays (input delays = delays used in data generation) in figure label/title
- [x] plot frac_obs from stanfit$fit object on bottom panel (James)
- [x] ammend and check ./synthetic/scenario_list.md for accuracy. i.e. remove alphabet shortcuts, keep table of names, check table of names for consistency with synth_data_functions.R (James)

### Minor plot improvements:
- [ ] center month labels on periods which they refer to (James)
- [x] include ticks for every 100 cases on linear scale plots (James)
- [x] include ticks for all months
- [ ] see lhs resource in R (Jeremy)
	- [ ] compare with Sobol sequence (Jeremy)
- [ ] remove whitespaces before colons (Jeremy)
- [x] .PRECIOUS: ${OUTDIR}/full/%.rds ${OTHERDIR}/folderwithstuff/%.RData (Jeremy)
- [ ] update makefile to use variable names for folders (Jeremy)
- [ ] academic questions: what's the question, why is it interesting, where can *you* [reader] find the answers to these q's (and their answers)? incl technical steps required (Jeremy)
- [ ] find nice stan tutorial (Jeremy / James)
- [ ] review last meeting with Carl and Juliet + hypothesize on a path forward to have parameters be easily changeable

# Old to-do
- [x] update makefile to produce d1_d2_d3_d4.png targets and dependencies with pattern matching (i.e. have one set of pattern-matching targets/rules for making all figures) (Jeremy: Draft + James review)
- [x] check / potentially rearrange locations for scripts involved in synthetic stuff. synthetic stuff should all be ./synthetic. only common stuff like plot_script.R and est_sec.R should be in ./main/scripts. common functions should be defined in ./main/functions/function_group.R and common defined functions should be saved in ./main/data/function_group.RData. Synthetic-specific functions should be defined in ./synthetic/scripts and their definitions (function_group.RData files) should be saved in ./synthetic/data (Together)
	- [50%] fix directory references in makefile (done) and in if(interactive()) (not yet done) within R scripts (Jeremy)
- [x] change "list-names" of "function-names" in synth_data_functions.R from alphabet to up/down/const/etc naming convention (James)
- [x] troubleshoot the complex figure (Together/Jeremy) A: Sec_obs doesn't depend on primary observation process (D1 -> D3) A: working as expected
	- [nd] also try with different priors (Jeremy)
	- sidenote: may also be (we think, Carl has emailed Sam to confirm) that estimate_secondary() is only fittig *one* ratio per window i.e. per fit
	- [x] try make up_twovals_twovals_const.png (defer until top stuff done/James)

##	Plot to-dos
- [x] plot underlying pieces (true primary and true secondary) (i.e. include all four "dimensions" on top panel) (James)

overall: two or if necessary more plots. upper panel similar to existing plot with underlying ts'es included + log-scale y-axis. lower panel(s) showing inputs and model outputs of delays and fractions

## Some notes
1. We are builders! (childhood dream!)
2. Let's still keep track of the science
3. Keep track of approximatly how RStan works (/how stan works in general)
4. How do we math this out into a paper? (something to do with a moving Gaussian process)
 

# New to-do's established 4 April
- add stochastic stuff:
	- sampled delays
	- time-varying delay distributions (one of the *types* of changes to include)
	- negative binomial and/or posson: delay distributions and primary time series
	- regular binomials and beta binomials: secondary generation and primary/secondary observation processes)
	- set seed
	- set sampling params
	- output a class of outcomes each with a sample id (i.e. a bunch of outputs for one scenario/parameter combination)
	- something like sensitivity and specificity of this method given known underlying changes
	- insert a shift of specified magnitude & randomness; we want our method to detect the shift within x time with y precision
	- i.e., if one is worried about a particular change happening, then the method with abc specs (e.g. window size, total eval window, etc) will have a "known" ability to detect the change of concern
	- want to be able to assess method in terms of the "scale" of changes. big vs small (threshold?)
	- currently a hardcoded parameter set for various kinds of feature changes
	- want to be able to scan over a variety of parameters for each feature
	- e.g. when we generate reference stuff, we give it plausible boundaries for relevant params, i
- think about good numbers to use
- start writing more conceptual whatthisisfor and howtothinkabout it stuff

## Plan/Landscape (to be deprecated? i.e. once estimate_secondary() fits a time-varying ratio, will one window per run suffice?)
- rolling window (of fractions)
- compare between windows
- think about window size and what will be suitable
- what we want is a "statement" of the *fractions* involved. i.e. secondary / primary

### Big question: What is/are our *key outputs* for each run of a scenario
	- what is key output for each window?
	- how are different windows within scenario compared
	- how are different scenarios within scenario-type compared [big Q]



