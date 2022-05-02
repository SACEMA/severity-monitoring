library(tidyverse)
library(lubridate)

.args <- if (interactive()) {
    c("./synthetic/data/define_synth_params_mother_list.csv",
      "./synthetic/inputs/param_combos.rds"
      )
} else {
  commandArgs(trailingOnly = TRUE)
}


#Get param specifications
mother_params_raw <- read_csv(.args[[1]])


#Extract start date
start_date <- dmy((mother_params_raw %>% tail(1))$min)


#Create column of vectors for grid
mother_params <- mother_params_raw %>% 
  filter(param != 'start_date') %>% 
  mutate(min = as.numeric(min),
         max = as.numeric(max)
         ) %>% 
  rowwise() %>% 
  mutate(values = list(seq(from = min, to = max, by = n))
         )
  

#Create a list of vectors per parameter
mother_param_vec_list <- list()

for (i in 1:nrow(mother_params)) {
  mother_param_vec_list[[mother_params[[i, 'param']]]] <- seq(from = mother_params[[i, 'min']], 
                                          to = mother_params[[i, 'max']],
                                          length.out = mother_params[[i, 'n']]
                                          )
}



#Generate the parameter combinations
param_combos <- (expand.grid(mother_param_vec_list)
	%>% mutate(start_date = start_date, 
	           experiment = paste0("experiment_", row_number()))
	)
saveRDS(param_combos, file = tail(.args, 1))
