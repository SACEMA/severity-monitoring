include makefiles/support.makefile

DATADIR ?= data

./synthetic/outputs/figures/flat_constant.png : ./script/plot.R ./synthetic/outputs/full/flat_constant.rds
	Rscript $^ $@ 

./synthetic/outputs/full/flat_constant.rds : ./scripts/estimate_secondary.R ./synthetic/data/flat_constant.rds ./synthetic/data/params.RData
	RScript $^ $@

./synthetic/data/params.RData : ./synthetic/scripts/define_params.R
	Rscript $^

${DATADIR}/weakly-informed-delays.rds: R/secondary-fraction-delay-priors.R
	$(call R)

${DATADIR}/sf_gp_utils.rda: R/secondary-fraction-utils.R
	$(call R)

${DATADIR}/example-incidence.rds ${DATADIR}/example-prevalence.rds: R/secondary-fraction-gp-simulation.R ${DATADIR}/sf_gp_utils.rda
	$(call R)

${DATADIR}/example-sf.rds: R/secondary-fraction.R ${DATADIR}/sf_gp_utils.rda ${DATADIR}/example-incidence.rds
	$(call R)

test: ${DATADIR}/example-incidence.rds ${DATADIR}/example-prevalence.rds