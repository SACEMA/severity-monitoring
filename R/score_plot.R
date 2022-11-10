
c("ggplot2") |> .req()

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
  geom_point() + theme_minimal() + coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom", legend.direction = "vertical") +
  scale_color_discrete(name = NULL) + scale_y_continuous(name = NULL) +
  scale_x_discrete(name = NULL) +
  annotate("text", x = 3, y = 0.95, vjust = 1, label = "\u2191 NO CHANGE \u2191") +
  annotate("text", x = 3, y = 0.05, vjust = 0, label = "\u2193 LOTS OF CHANGE \u2193")

ggsave(tail(.args, 1), p, width = 6, height = 5, bg = "white")
