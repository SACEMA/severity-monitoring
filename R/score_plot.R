
c("ggplot2", "data.table") |> .req()

.args <- .fromArgs(c(
  file.path("data", "sf_gp_utils.rda"),
  list.files(file.path("output", "analysis"), "scenario_.+_score\\.rds", full.names = TRUE),
  list.files("data", "scenario_.+\\.json", full.names = TRUE),
  file.path("output", "analysis", "score_plot.png")
))

dt <- .args |> grep(pattern = "_score\\.rds$", x = _, value = TRUE) |>
  lapply(readRDS) |> rbindlist(idcol = "scenario")

dt[(.args |> grep(pattern = "\\.json$", x = _, value = TRUE) |>
  lapply(jsonlite::read_json) |> lapply(\(j) data.table(desc = j$scen_desc)) |>
  rbindlist(idcol = "scenario")), on=.(scenario), desc := desc]

load(.args[1])

p <- ggplot(dt) + aes(x=factor(scenario), y = relative_performance, color = desc) + 
  geom_point() + theme_minimal() + coord_cartesian(ylim = c(1, NA)) +
  theme(legend.position = "bottom", legend.direction = "vertical") +
  scale_color_discrete(name = NULL) + scale_y_continuous(name = "Relative Performance,\nOnly Window vs with Baseline") +
  scale_x_discrete(name = NULL)
# +
#   annotate("text", x = 3, y = 1, vjust = 0, label = "\u2193 NO CHANGE \u2193") +
#   annotate("text", x = 3, y = 1.5, label = "\u2191 LOTS OF CHANGE \u2191")

ggsave(tail(.args, 1), p, width = 6, height = 5, bg = "white")
