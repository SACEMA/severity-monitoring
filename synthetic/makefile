
FIGDIR := ./outputs/figures
FULLOUTDIR := ./outputs/full
DATADIR := ./data


.PRECIOUS: ./outputs/full/%.rds 
.PRECIOUS: ./data/%.rds

#VAR1d := up down flat
#VAR2D := onevale twoval
#$(foreach v1,${VAR1D},...)
#${v1}_${v2}

#all : up_twovals_twovals_twovals.png down_const_const_twovals.png # why doesn't this work again?

#all: figure_nameso

#${FIGDIR}/%.png : thescript.R %.thedata

#./outputs/figures/%.png : ../scripts/plot_sec_est.R ./outputs/full/%.rds ../functions/plotting_functions.RData
#	$*

#rule1 = 5 21 2
#values1 = 5 7 9 11 13 15 17 19 21
#option 1
#scenariots.rds: create_scenario etc
#	Rscript $^ $@ rule1 rule2 rule3 rule4 rule5 rule6 rule7 rule8 rule9 rule10 rule11 rule12
#option 2
#for i in 1:length(values1)
#	scenariots_%_${i}.rds: create_scenario etc
#		Rscript $^ $@ value1 value2 value3 value4 value5 value6 value7 value8 value9 value10 value11 value12
#option 3
#scenariots_%_1.rds scen2 scen3 scen4 scen5 scen6 scen7: create_scenario params_combos_%_1.rds etc
#	Rscript $^ $@
#scenario_%_combo1: create_scenario params_combos_%_1.rds etc
	


# in the terminal, we type:
# make ./outputs/figures/hello_plot.png
# then make will check the dependencies:
# ../scripts/plot_sec_est.R ./outputs/full/hello_plot.rds ../functions/plotting_functions.RData
# ./outputs/full/hello_plot.rds will match with ./outputs/full/%.rds
# inside the shell commands for a pattern-matched make target, we can access "the pattern match" via $*
# we can also use $(subst _, ,$*) to get from (% =) d1_d2_d3_d4 to args d1 d2 d3 d4 (i.e. with spaces)

./outputs/figures/%.png : ../main/scripts/plot_sec_est.R ./outputs/full/%.rds ../main/data/plotting_functions.RData ./data/%.rds 
	Rscript $^ $@ # optional; title etc ...

./outputs/full/%.rds : ../main/scripts/estimate_secondary.R ./data/%.rds ./data/estimation_params.RData
	Rscript $^ $@ ...

./data/%.rds : ./scripts/create_scenario.R ./data/synth_params.RData ./data/synth_data_functions.RData
	Rscript $^ $@ $(subst _, ,$*) ...

# Generate parameter sweep space and save to file
#./data/%_paramcombo.rds: ...
#	RScript ...

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

../main/data/plotting_functions.RData : ../main/functions/plotting_functions.R
	Rscript $^ $@