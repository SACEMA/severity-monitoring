
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
  rbindlist(idcol = "scenario")), on=.(scenario), desc := gsub("Scenario ", "", gsub("severity ->", "->", desc))]

load(.args[1])

p <- ggplot(dt) + aes(x=factor(scenario), y = relative_performance, color = desc) + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey80") +
  geom_point() + geom_text(
    aes(y = relative_performance^(4/5), label = signif(relative_performance, 3),
    hjust = ifelse(relative_performance < 1, 0, 1)
  ), angle = 90) +
  theme_minimal() + coord_cartesian(ylim = c(0.5, 2)) +
  theme(legend.position = "bottom", legend.direction = "vertical") +
  scale_color_discrete(name = NULL) + scale_y_continuous(
    name = "Relative Performance,\nOnly Window vs with Baseline",
    trans = "log2", breaks = c(0.5, 1, 2), labels = c("1/2x", "1x", "2x")
  ) +
  scale_x_discrete(name = "Scenario")
# +
#   annotate("text", x = 3, y = 1, vjust = 0, label = "\u2193 NO CHANGE \u2193") +
#   annotate("text", x = 3, y = 1.5, label = "\u2191 LOTS OF CHANGE \u2191")

ggsave(tail(.args, 1), p, width = 6, height = 5, bg = "white")
