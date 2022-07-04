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


$(FIGDIR)/scenario_%.png: $(SCRIPTDIR)/plot_scenario.R $(OUTDIR)/scenario_%.RData $(INPUTSDIR)/scenario_%.json 
	Rscript $^ $@

$(OUTDIR)/scenario_%.RData: $(SCRIPTDIR)/create_demo_scenario.R $(DATADIR)/synth_data_functions.RData $(INPUTSDIR)/scenario_%.json
	Rscript $^ 5 $@

all_scenarios: $(OUTDIR)/scenario_1.RData $(OUTDIR)/scenario_2.RData $(OUTDIR)/scenario_3.RData $(OUTDIR)/scenario_4.RData $(OUTDIR)/scenario_5.RData

all_scenario_plots: $(FIGDIR)/scenario_1.png $(FIGDIR)/scenario_2.png $(FIGDIR)/scenario_3.png $(FIGDIR)/scenario_4.png $(FIGDIR)/scenario_5.png


$(DATADIR)/synth_data_functions.RData: $(SCRIPTDIR)/synth_data_functions.R
	Rscript $^ $@



