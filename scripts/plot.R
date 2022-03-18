# plot output from estimate_secondary

load('./functions/plotting_functions.RData')

.args <- if (interactive()) c(
  file.path('synthetic/outputs/full', 'flat_constant.rds'), # input
  file.path('synthetic/outputs/figures', 'flat_constant.png') # output
) else commandArgs(trailingOnly = TRUE)

dd <- readRDS(.args[1])

fig <- plot_est_sec_out(predictions = dd[['predictions']])

ggsave(.args[2], plot = fig, width = 12, height = 8)
