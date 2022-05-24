.args <- if(interactive()){
  c('./synthetic/outputs/full/scenario_1.RData',
    './synthetic/outputs/figures/scenario_1.png')
}else{
  commandArgs(trailingOnly = TRUE)
}

load(.args[1])

ts_combined %>%
    ggplot(aes(x = time, y = value, color = name))+
    geom_line()

ggsave(filename = tail(.args,1),
       device = 'png',
       dpi = 320)