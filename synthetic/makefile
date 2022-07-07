FIGDIR := ./outputs/figures
OUTDIR := ./outputs/full
DATADIR := ./data
SCRIPTDIR := ./scripts
INPUTSDIR := ./inputs

MAINSCRIPTDIR := ../main/scripts
MAINFUNCDIR := ../main/functions
MAINDATADIR := ../main/data

.PRECIOUS: ./outputs/full/%.rds 
.PRECIOUS: ./data/%.rds
.SECONDEXPANSION:

$(FIGDIR)/scenario_%.pdf: $(SCRIPTDIR)/plot_scenario.R $(OUTDIR)/scenario_%.RDS $(INPUTSDIR)/scenario_%.json 
	Rscript $^ $@

$(OUTDIR)/scenario_%.RDS: $(SCRIPTDIR)/create_demo_scenario.R $(DATADIR)/synth_data_functions.RData $(INPUTSDIR)/scenario_%.json $(DATADIR)/utils.RData
	Rscript $^ 5 trashburn $@

all_scenarios: $(OUTDIR)/scenario_1.RDS $(OUTDIR)/scenario_2.RDS $(OUTDIR)/scenario_3.RDS $(OUTDIR)/scenario_4.RDS $(OUTDIR)/scenario_5.RDS

all_scenario_plots: $(FIGDIR)/scenario_1.pdf $(FIGDIR)/scenario_2.pdf $(FIGDIR)/scenario_3.pdf $(FIGDIR)/scenario_4.pdf $(FIGDIR)/scenario_5.pdf

$(DATADIR)/synth_data_functions.RData: $(SCRIPTDIR)/synth_data_functions.R
	Rscript $^ $@
	
$(DATADIR)/utils.RData: $(SCRIPTDIR)/utils.R
	Rscript $^ $@

$(OUTDIR)/compare_scenarios_%.pdf: $(SCRIPTDIR)/compare_scenarios.R $(OUTDIR)/scenario_$$(firstword $$(subst _, ,$$*)).RDS $(OUTDIR)/scenario_$$(lastword $$(subst _, ,$$*)).RDS $(DATADIR)/utils.RData
	Rscript $^ $@
