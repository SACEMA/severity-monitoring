
./synthetic/outputs/figures/flat_constant.png : ./script/plot.R ./synthetic/outputs/full/flat_constant.rds
	Rscript $^ $@ 

./synthetic/outputs/full/flat_constant.rds : ./scripts/estimate_secondary.R ./synthetic/data/flat_constant.rds ./synthetic/data/params.RData
	RScript $^ $@

./synthetic/data/params.RData : ./synthetic/scripts/define_params.R
	Rscript $^ $@
