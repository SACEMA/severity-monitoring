.args <- if (interactive()) {
  c(
    "./synthetic/outputs/full/scenario_1.RDS",
    "./synthetic/outputs/full/scenario_3.RDS",
    './synthetic/inputs/scenario_1.json',
    './synthetic/inputs/scenario_3.json',
    "./synthetic/data/utils.RData",
    "./synthetic/outputs/figures/compare_scenarios_1_3.pdf"
  )
} else {
  commandArgs(trailingOnly = TRUE)
}

## how should we organise this stuff?
## how does one view changes by person in git? as in, add it to a view of a file?


suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(stringr)
  library(patchwork)
  library(cowplot)
  library(jsonlite)
  library(geomtextpath)
})

#Read in the scenario data and combine
first_scenario_data <- readRDS(.args[1])
second_scenario_data <- readRDS(.args[2])

plot_data <- rbind(first_scenario_data, second_scenario_data)

#Read in the scenario descriptions
first_scenario_desc <- read_json(.args[3])[["scen_desc"]]
second_scenario_desc <- read_json(.args[4])[["scen_desc"]]


#Load helper functions
load(.args[5])

#Don't print plot labels in scientific notation
options(scipen = 9999)


plot_data <- plot_data %>%
  mutate(name = str_replace(name, "_", " "),
         scenario_id = paste0('(scenario ', scenario_id, ')')
         )

plot_labs_data <- plot_data %>%
  group_by(name, scenario_id) %>%
  slice(1) %>%
  ungroup()

ts_tmp_log <- ggplot(data = plot_data) +
  geom_line(aes(
    x = time,
    y = value,
    group = interaction(sim_id, name, scenario_id, sep = " "),
    color = interaction(name, scenario_id, sep = " ")
  ), 
  size = 1
  ) +
  geom_labelline(
    data = plot_labs_data,
    aes(
      x = time,
      y = value,
      color = interaction(name, scenario_id, sep = " "),
      label = interaction(name, scenario_id, sep = " ")
    ),
    show.legend = FALSE,
    hjust = 0.10,
    size = 3,
    linewidth = 0.45,
    straight = TRUE,
    fontface = "bold",
    boxlinewidth = 1
  ) +
  scale_y_log10() +
  facet_wrap(~name) +
  labs(
    y = "Daily count (log transformed)", x = "Day",
    color = "",
    caption = paste("A comparison of", first_scenario_desc, "&", second_scenario_desc)
  ) +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_blank(), legend.position = "none")

ggsave(
  plot = ts_tmp_log,
  filename = tail(.args, 1),
  device = "pdf",
  height = 10,
  width = 14,
  dpi = 320
)
