.args <- if(interactive()){
  c('./synthetic/outputs/full/scenario_2.RData',
    "./synthetic/inputs/scenario_2.json",
    './synthetic/outputs/figures/scenario_2.png')
}else{
  commandArgs(trailingOnly = TRUE)
}


suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(purrr)
  library(patchwork)
  library(jsonlite)
})

load(.args[1])

scenario_desc <- read_json(.args[2]) %>% 
  pluck('scen_desc')

# scenario_number <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(tail(.args,1)))  %>%
#   sub(pattern = "scenario_", replacement = "")

ts_tmp <- ts_combined %>%
  ggplot(aes(x = time, y = value, color = name))+
  geom_line() + 
  labs(y = "Daily count", x = "Day",
       title = scenario_desc,
       color = "")


ts_tmp_log <- ts_combined %>%
  ggplot(aes(x = time, y = value, color = name))+
  geom_line() + 
  scale_y_log10() +
  labs(y = "Daily count (log transformed)", x = "Day",
       color = "")

ggsave(plot = ts_tmp/ts_tmp_log,
       filename = tail(.args,1),
       device = 'png',
       height = 10,
       width = 14,
       dpi = 320)
