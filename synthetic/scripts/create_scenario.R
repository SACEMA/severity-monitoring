.args <- if(interactive){
c()
}else{commandArgs(trailingOnly = TRUE)}

# generically rename generation and observation functions according to the scenario description received via commandArgs()
d1_fxn <- get(d1_functions[[.args[[1]]]])
d2_fxn <- get(d2_functions[[.args[[2]]]])
d3_fxn <- get(d3_functions[[.args[[3]]]])
d4_fxn <- get(d4_functions[[.args[[4]]]])

load(.args[[5]])


dd <- d1_fxn() %>% d2_fxn() %>% d3_fxn() %>% d4_fxn()


saveRDS(dd, tail(.args,1))



