.args <- if (interactive()) {
  c(
    "./synthetic/outputs/full/scenario_1.RData",
    "./synthetic/inputs/scenario_1.json",
    "./synthetic/outputs/figures/scenario_1.png"
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

load(.args[1])

scenario_desc <- read_json(.args[2])[["scen_desc"]]

options(scipen = 9999)

plot_data <- sim_results %>%
  mutate(name = str_replace(name, "_", " "))

# ts_tmp <- ggplot(plot_data, aes(x = time, y = value, label = name))+
#   geom_labelpath(size = 3.5) +
#   labs(y = "Daily count", x = "Day",
#        title = scenario_desc,
#        color = "") +
#   theme_minimal()

plot_df <- plot_data %>% 
  pivot_wider(names_from = name, values_from = value) %>%
  janitor::clean_names()

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
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  labs(color = '', title = scenario_desc)
  # geom_labelline(aes(
  #   label = name,
  #   group = sim_id
  # ),
  # hjust = 0.6,
  # size = 3.5,
  # linewidth = 0.45,
  # straight = TRUE
  # ) +
  # scale_y_log10() +
  # labs(
#   y = "Daily count (log transformed)", x = "Day",
#   color = ""
# ) +
# theme_minimal()
  # geom_line() +
  # geom_line(aes(
  #   x = time,
  #   y = latent_primary,
  #   group = factor(sim_id),
  #   color = 'latent_primary'
  # )) +
  # geom_line(aes(
  #   x = time,
  #   y = primary,
  #   group = factor(sim_id)
  # )) +
  # geom_line(aes(
  #   x = time,
  #   y = secondary,
  #   group = factor(sim_id)
  # )) +
  # scale_y_log10() +
  # theme_minimal()


ggsave(
  plot = ts_tmp_log,
  filename = tail(.args, 1),
  device = "png",
  height = 10,
  width = 14,
  dpi = 320
)
