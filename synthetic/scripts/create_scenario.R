.args <- if(interactive){
c("a","a","a","a")
}else{commandArgs(trailingOnly = TRUE)}

load(.args[[5]])

# generically rename generation and observation functions according to the scenario description received via commandArgs()
d1_fxn <- get(d1_functions[[.args[[1]]]]) # dimension 1 functional form
d2_fxn <- get(d2_functions[[.args[[2]]]]) # dimension 2 functional form
d3_fxn <- get(d3_functions[[.args[[3]]]]) # dimension 3 functional form
d4_fxn <- get(d4_functions[[.args[[4]]]]) # dimension 4 functional form

dd <- d1_fxn() %>% d2_fxn() %>% d3_fxn() %>% d4_fxn()


saveRDS(dd, tail(.args,1))



