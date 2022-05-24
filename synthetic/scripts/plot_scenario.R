.args <- if(interactive()){
  c('./synthetic/outputs/full/scenario_1.RData',
    './synthetic/outputs/figures/scenario_1.png')
}else{
  commandArgs(trailingOnly = TRUE)
}
library(tidyverse)

load(.args[1])

scenario_number <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(tail(.args,1)))  %>%
  sub(pattern = "scenario_", replacement = "")

ts_tmp <- ts_combined %>%
  ggplot(aes(x = time, y = value, color = name))+
  geom_line() + 
  scale_y_log10() +
  labs(y = "Daily count (log transformed)", x = "Day",
       title = sprintf("Scenario %s", scenario_number),
       color = "")

ggsave(plot = ts_tmp,
       filename = tail(.args,1),
       device = 'png',
       height = 10,
       width = 14,
       dpi = 320)