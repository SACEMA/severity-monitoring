
FIGDIR := ./outputs/figures
FULLOUTDIR := ./outputs/full
DATADIR := ./data

#VAR1d := up down flat
#VAR2D := onevale twoval
#$(foreach v1,${VAR1D},...)
#${v1}_${v2}

#all : up_twovals_twovals_twovals.png down_const_const_twovals.png # why doesn't this work again?

#all: figure_nameso

#${FIGDIR}/%.png : thescript.R %.thedata

#./outputs/figures/%.png : ../scripts/plot_sec_est.R ./outputs/full/%.rds ../functions/plotting_functions.RData
#	$*

# in the terminal, we type:
# make ./outputs/figures/hello_plot.png
# then make will check the dependencies:
# ../scripts/plot_sec_est.R ./outputs/full/hello_plot.rds ../functions/plotting_functions.RData
# ./outputs/full/hello_plot.rds will match with ./outputs/full/%.rds
# inside the shell commands for a pattern-matched make target, we can access "the pattern match" via $*
# we can also use $(subst _, ,$*) to get from (% =) d1_d2_d3_d4 to args d1 d2 d3 d4 (i.e. with spaces)

./outputs/figures/%.png : ../main/scripts/plot_sec_est.R ./outputs/full/%.rds ./data/%.rds ../main/data/plotting_functions.RData
	Rscript $^ $@

./outputs/full/%.rds : ../main/scripts/estimate_secondary.R ./data/%.rds ./data/estimation_params.RData
	Rscript $^ $@

./data/%.rds : ./scripts/create_scenario.R ./data/synth_params.RData ./data/synth_data_functions.RData
	Rscript $^ $@ $(subst _, ,$*)

#./outputs/figures/up_twovals_twovals_twovals.png : ../scripts/plot_sec_est.R ./outputs/full/up_twovals_twovals_twovals.rds ../functions/plotting_functions.RData
#	Rscript $^ $@

#./outputs/full/up_twovals_twovals_twovals.rds : ../scripts/estimate_secondary.R ./data/up_twovals_twovals_twovals.rds ./data/estimation_params.RData
#	Rscript $^ $@

#./data/up_twovals_twovals_twovals.rds : ./scripts/create_scenario.R ./data/synth_params.RData ./data/synth_data_functions.RData
#	Rscript $^ $@ b b b b

# flat_const_const_const

#./outputs/figures/flat_const_const_const.png : ../scripts/plot_sec_est.R ./outputs/full/flat_const_const_const.rds ../functions/plotting_functions.RData
#	Rscript $^ $@

#./outputs/full/flat_const_const_const.rds : ../scripts/estimate_secondary.R ./data/flat_const_const_const.rds ./data/estimation_params.RData
#	Rscript $^ $@

#./data/flat_const_const_const.rds : ./scripts/create_scenario.R ./data/synth_params.RData ./data/synth_data_functions.RData
#	Rscript $^ $@ a a a a

# up_const_const_const

#./outputs/figures/up_const_const_const.png : ../scripts/plot_sec_est.R ./outputs/full/up_const_const_const.rds ../functions/plotting_functions.RData
#	Rscript $^ $@

#./outputs/full/up_const_const_const.rds : ../scripts/estimate_secondary.R ./data/up_const_const_const.rds ./data/estimation_params.RData
#	Rscript $^ $@

#./data/up_const_const_const.rds : ./scripts/create_scenario.R ./data/synth_params.RData ./data/synth_data_functions.RData
#	Rscript $^ $@ b a a a

# exp_const_const_const

#./outputs/figures/exp_const_const_const.png : ../scripts/plot_sec_est.R ./outputs/full/exp_const_const_const.rds ../functions/plotting_functions.RData
#	Rscript $^ $@

#./outputs/full/exp_const_const_const.rds : ../scripts/estimate_secondary.R ./data/exp_const_const_const.rds ./data/estimation_params.RData
#	Rscript $^ $@

#./data/exp_const_const_const.rds : ./scripts/create_scenario.R ./data/synth_params.RData ./data/synth_data_functions.RData
#	Rscript $^ $@ d a a a

# common requirements

./data/synth_data_functions.RData : ./scripts/synth_data_functions.R
	Rscript $^ $@

./data/synth_params.RData : ./scripts/define_synth_params.R
	Rscript $^ $@

./data/estimation_params.RData : ./scripts/define_estimate_params.R
	Rscript $^ $@

../functions/plotting_functions.RData : ../main/functions/plotting_functions.R
	Rscript $^ $@
