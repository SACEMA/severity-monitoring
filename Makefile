
# define local paths, settings, etc IFF local.make exists
-include local.make

DATADIR ?= data
FIGDIR ?= figs
OUTDIR ?= output
SYNDIR := ${OUTDIR}/synthetic

# enables directory manufacturing rules from support.make
MAKEDIRS := ${DATADIR} ${FIGDIR} ${OUTDIR} ${SYNDIR}

include makefiles/support.make

${FIGDIR}/%.png: R/plot_scenario.R ${OUTDIR}/synthetic/%.rds
	$(call R)

${SYNDIR}/%.rds: R/generate_synthetic.R ${SYNDIR}/%_params.rds
	$(call R)

${SYNDIR}/%_params.rds: R/parse_params.R ${DATADIR}/%.json

./synthetic/outputs/full/flat_constant.rds : ./scripts/estimate_secondary.R ./synthetic/data/flat_constant.rds ./synthetic/data/params.RData
	RScript $^ $@

./synthetic/data/params.RData : ./synthetic/scripts/define_params.R
	$(call R)

${DATADIR}/weakly-informed-delays.rds: R/secondary-fraction-delay-priors.R
	$(call R)

${DATADIR}/sf_gp_utils.rda: R/secondary-fraction-utils.R
	$(call R)

${DATADIR}/example-incidence.rds ${DATADIR}/example-prevalence.rds: R/secondary-fraction-gp-simulation.R ${DATADIR}/sf_gp_utils.rda
	$(call R)

${DATADIR}/example-sf.rds: R/secondary-fraction.R ${DATADIR}/sf_gp_utils.rda ${DATADIR}/example-incidence.rds
	$(call R)

test: ${DATADIR}/example-incidence.rds ${DATADIR}/example-prevalence.rds
