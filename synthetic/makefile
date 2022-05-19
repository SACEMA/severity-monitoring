FIGDIR := ./outputs/figures
OUTDIR := ./outputs/full
DATADIR := ./data
SCRIPTDIR := ./scripts
INPUTDIR := ./inputs

MAINSCRIPTDIR := ../main/scripts
MAINFUNCDIR := ../main/functions
MAINDATADIR := ../main/data

.PRECIOUS: ./outputs/full/%.rds 
.PRECIOUS: ./data/%.rds


$(DATADIR)/synth_data_functions.RData: $(SCRIPTDIR)/synth_data_functions.R
	Rscript $^ $@

$(DATADIR)/demo_params.RData: $(SCRIPTDIR)/init_scenario_params.R
	Rscript $^ $@

$(OUTDIR)/scenario_%.RData: $(SCRIPTDIR)/create_demo_scenario.R $(DATADIR)/synth_data_functions.RData $(DATADIR)/demo_params.RData
	Rscript $^ $@

all_scenarios: $(OUTDIR)/scenario_1.RData $(OUTDIR)/scenario_2.RData $(OUTDIR)/scenario_3.RData

#define scenarize =



#endef

#othertarget: otherscript.R combo_output_file.rds
	
# where combo_output_file.rds contains a dataframe which is the expanded_child_param_table %>% mutate to add summary outputs from estimate_secondary(). this still contains the column 'experiment', which has an ID matching the filename for the full outputs. 

# $(OUTDIR)/%_full.rds: estimate_secondary_multi.R $(INPUTDIR)/param_combos.rds: #need this to run m instances of estimate_secondary() where m = number of rules
#	Rscript $^ $@ 


## synthetic data for NICD demo 1


#$(MAINDATADIR)/plotting_functions.RData: $(MAINFUNCDIR)/plotting_functions.R
#	Rscript $^ $@


# in the terminal, we type:
# make ./outputs/figures/hello_plot.png
# then make will check the dependencies:
# ../scripts/plot_sec_est.R ./outputs/full/hello_plot.rds ../functions/plotting_functions.RData
# ./outputs/full/hello_plot.rds will match with ./outputs/full/%.rds
# inside the shell commands for a pattern-matched make target, we can access "the pattern match" via $*
# we can also use $(subst _, ,$*) to get from (% =) d1_d2_d3_d4 to args d1 d2 d3 d4 (i.e. with spaces)

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

#$(INPUTDIR)/param_combos.rds: $(SCRIPTDIR)/generate_param_combos.R $(DATADIR)/define_synth_params_mother_list.csv
#	Rscript $^ $@

#$(FIGDIR)/%.png: $(MAINSCRIPTDIR)/plot_sec_est.R $(FULLOUTDIR)/%.rds $(MAINDATADIR)/plotting_functions.RData $(DATADIR)/%.rds 
#	Rscript $^ $@ # optional; title etc ...

#$(FULLOUTDIR)/%.rds: $(MAINSCRIPTDIR)/estimate_secondary.R $(DATADIR)/%.rds $(DATADIR)/estimation_params.RData#
#	Rscript $^ $@ ...

#$(DATADIR)/%.rds: $(SCRIPTDIR)/create_scenario.R $(DATADIR)/synth_params.RData $(DATADIR)/synth_data_functions.RData
#	Rscript $^ $@ $(subst _, ,$*) ...
