
default: targ

# define local paths, settings, etc IFF local.make exists
-include local.make

DATADIR ?= data
FIGDIR ?= figs
OUTDIR ?= output
SYNDIR := ${OUTDIR}/synthetic
ESTDIR := ${OUTDIR}/analysis

# enables directory manufacturing rules from support.make
MAKEDIRS := ${DATADIR} ${FIGDIR} ${OUTDIR} ${SYNDIR} ${ESTDIR}

include makefiles/support.make

${FIGDIR}/%.png: R/plot_scenario.R ${OUTDIR}/synthetic/%.rds | ${FIGDIR}
	$(call R)

${SYNDIR}/synthetic_funs.rda: R/synthetic_funs.R | ${SYNDIR}
	$(call R)

${DATADIR}/utils.rda: R/utils.R | ${DATADIR}
	$(call R)

${SYNDIR}/scenario_%.rds: R/create_demo_scenario.R ${SYNDIR}/synthetic_funs.rda ${DATADIR}/scenario_%.json | ${SYNDIR}
	$(call R)

.PRECIOUS: ${SYNDIR}/scenario_%.rds

${ESTDIR}/scenario_%.rds: R/sf_evaluate.R $(addprefix ${DATADIR}/,sf_gp_utils.rda est_config.json weakly-informed-delays.rds) ${SYNDIR}/scenario_%.rds | ${ESTDIR}
	$(call R)

${ESTDIR}/scenario_%_score.rds: ${ESTDIR}/scenario_%.rds

${ESTDIR}/plot_%.png: R/sf_plot.R ${DATADIR}/sf_gp_utils.rda ${DATADIR}/scenario_%.json ${ESTDIR}/scenario_%.rds | ${ESTDIR}
	$(call R)

${ESTDIR}/score_plot.png: R/score_plot.R $(wildcard ${ESTDIR}/scenario_*_score.rds) $(wildcard ${DATADIR}/scenario_*.json) | ${ESTDIR}
	$(call R)

targ: $(patsubst %,${ESTDIR}/plot_%.png,1 2 3 4 5)

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
