.args <- if (interactive()) {
  c(
    "./synthetic/outputs/full/scenario_3.RDS",
    "./synthetic/inputs/scenario_3.json",
    "./synthetic/outputs/figures/scenario_3.pdf"
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

plot_labels <- plot_data %>% 
  group_by(name) %>% 
  slice(1) %>% 
  ungroup()

ts_tmp_log <- ggplot() +
  geom_line(data = plot_data, 
            aes(x = time,
            y = value,
            group = interaction(name, sim_id),
            color = name
            ),
            size = 0.85
            ) +
  geom_labelline(
    data = plot_labels,
    aes(x = time,
        y = value,
       label = name,
       colour = name
      ),
    hjust = 0,
    size = 3.5,
    linewidth = 0.45,
    straight = TRUE,
    fontface = "bold",
    boxlinewidth = 1
  ) +
  scale_y_log10() +
  theme_minimal(base_size = 16) +
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
