library(tidyverse)

.args <- if(interactive()){
c("./synthetic/data/synth_params.RData",
  "./synthetic/data/synth_data_functions.RData",
  "./synthetic/data/flat_const_const_const.rds",
  "a",
  "a",
  "a",
  "a"
  )
}else{commandArgs(trailingOnly = TRUE)}

load(.args[[1]])  # parameter names and values
load(.args[[2]])  # function names with default parameters named,

# generically rename generation and observation functions according to the scenario description received via commandArgs()
d1_fxn <- get(d1_functions[[.args[4]]]) # dimension 1 functional form
d2_fxn <- get(d2_functions[[.args[5]]]) # dimension 2 functional form
d3_fxn <- get(d3_functions[[.args[6]]]) # dimension 3 functional form
d4_fxn <- get(d4_functions[[.args[7]]]) # dimension 4 functional form

dd <- d1_fxn() %>% d2_fxn() %>% d3_fxn() %>% d4_fxn() %>% add_dates()

dd$primary = as.integer(dd$primary)
dd$secondary = as.integer(dd$secondary)

saveRDS(dd, .args[[3]])

