.args <- if (interactive()) {
  c(
    "./synthetic/outputs/full/scenario_1.RData",
    "./synthetic/outputs/full/scenario_2.RData",
    "./synthetic/outputs/figures/compare_scenarios_1_2.png"
  )
} else {
  commandArgs(trailingOnly = TRUE)
}


second_scenario <- 


## how should we organise this stuff?
## how does one view changes by person in git? as in, add it to a view of a file?


suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(stringr)
  library(patchwork)
  library(jsonlite)
  library(geomtextpath)
})

load(.args[1])

scenario_desc <- read_json(.args[2])[["scen_desc"]]

options(scipen = 9999)

plot_data <- ts_combined %>%
  mutate(name = str_replace(name, "_", " "))

# ts_tmp <- ggplot(plot_data, aes(x = time, y = value, label = name))+
#   geom_labelpath(size = 3.5) +
#   labs(y = "Daily count", x = "Day",
#        title = scenario_desc,
#        color = "") +
#   theme_minimal()


ts_tmp_log <- ggplot(
  plot_data,
  aes(
    x = time,
    y = value,
    groups = name
  )
) +
  geom_labelline(aes(label = name),
                 hjust = 0.6,
                 size = 3.5,
                 linewidth = 0.45,
                 straight = TRUE
  ) +
  scale_y_log10() +
  labs(
    y = "Daily count (log transformed)", x = "Day",
    color = ""
  ) +
  theme_minimal()

ggsave(
  plot = ts_tmp_log,
  filename = tail(.args, 1),
  device = "png",
  height = 10,
  width = 14,
  dpi = 320
)
