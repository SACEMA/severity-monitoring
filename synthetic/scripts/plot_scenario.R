.args <- if (interactive()) {
  c(
    "./synthetic/outputs/full/scenario_1.RDS",
    "./synthetic/inputs/scenario_1.json",
    "./synthetic/outputs/figures/scenario_1.pdf"
  )
} else {
  commandArgs(trailingOnly = TRUE)
}


suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(patchwork)
  library(jsonlite)
  library(geomtextpath)
})

scenario_data <- readRDS(.args[1])

scenario_desc <- read_json(.args[2])[["scen_desc"]]

options(scipen = 9999) # remove scientific notation

plot_data <- scenario_data %>%
  mutate(name = str_replace(name, "_", " "))

plot_labels <- scenario_data %>% 
  group_by(name) %>% 
  slice(1)

ts_tmp_log <- ggplot(
  plot_data,
  aes(
    x = time,
    y = value,
    groups = factor(sim_id),
    color = name
  )
) +
  geom_line() +
  geom_labelline(
    data = plot_labels,
    aes(
      label = name,
      group = interaction(sim_id, name)
    ),
    hjust = 0,
    size = 3.5,
    linewidth = 0.45,
    straight = TRUE
  ) +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    x = "Day",
    y = "Daily count (log transformed)",
    color = "",
    title = scenario_desc
  )



ggsave(
  plot = ts_tmp_log,
  filename = tail(.args, 1),
  device = "pdf",
  height = 10,
  width = 14,
  dpi = 320
)
