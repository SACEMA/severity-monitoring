# up_twovals_twovals_twovals

./outputs/figures/up_twovals_twovals.png : ../scripts/plot_sec_est.R ./outputs/full/up_twovals_twovals_twovals.rds ../functions/plotting_functions.RData
	Rscript $^ $@

./outputs/full/up_twovals_twovals_twovals.rds : ../scripts/estimate_secondary.R ./data/up_twovals_twovals_twovals.rds ./data/estimation_params.RData
	Rscript $^ $@

./data/up_twovals_twovals_twovals.rds : ./scripts/create_scenario.R ./data/synth_params.RData ./data/synth_data_functions.RData
	Rscript $^ $@ b b b b

# flat_const_const_const

./outputs/figures/flat_const_const_const.png : ../scripts/plot_sec_est.R ./outputs/full/flat_const_const_const.rds ../functions/plotting_functions.RData
	Rscript $^ $@

./outputs/full/flat_const_const_const.rds : ../scripts/estimate_secondary.R ./data/flat_const_const_const.rds ./data/estimation_params.RData
	Rscript $^ $@

./data/flat_const_const_const.rds : ./scripts/create_scenario.R ./data/synth_params.RData ./data/synth_data_functions.RData
	Rscript $^ $@ a a a a

# up_const_const_const

./outputs/figures/up_const_const_const.png : ../scripts/plot_sec_est.R ./outputs/full/up_const_const_const.rds ../functions/plotting_functions.RData
	Rscript $^ $@

./outputs/full/up_const_const_const.rds : ../scripts/estimate_secondary.R ./data/up_const_const_const.rds ./data/estimation_params.RData
	Rscript $^ $@

./data/up_const_const_const.rds : ./scripts/create_scenario.R ./data/synth_params.RData ./data/synth_data_functions.RData
	Rscript $^ $@ b a a a

# exp_const_const_const

./outputs/figures/exp_const_const_const.png : ../scripts/plot_sec_est.R ./outputs/full/exp_const_const_const.rds ../functions/plotting_functions.RData
	Rscript $^ $@

./outputs/full/exp_const_const_const.rds : ../scripts/estimate_secondary.R ./data/exp_const_const_const.rds ./data/estimation_params.RData
	Rscript $^ $@

./data/exp_const_const_const.rds : ./scripts/create_scenario.R ./data/synth_params.RData ./data/synth_data_functions.RData
	Rscript $^ $@ d a a a

# common requirements

./data/synth_data_functions.RData : ../functions/synth_data_functions.R
	Rscript $^ $@

./data/synth_params.RData : ./scripts/define_synth_params.R
	Rscript $^ $@

./data/estimation_params.RData : ./scripts/define_estimate_params.R
	Rscript $^ $@

../functions/plotting_functions.RData : ../functions/plotting_functions.R
	Rscript $^ $@
