## common params
ts_len <- 150

## params strain1

## params strain2

##  create TS for strain1 including true and observed cases and admissions
## pipe functions from synth data functions to one another
##  create TS for strain2 including true and observed cases and admissions

## add TS_1 + TS_2 and save to RDS file.

## note: do we want to generate true primary in a stochastic fashion? YEs
## when do we want to start doing this?


# ---------------------------------------------------------------------------- #

# ts_len <- 1E2
# ts_tmp <- data.frame(time = 1:ts_len, infections = rep(1E5, ts_len))
# 
# set.seed(123410000)
# 
# p1 <- 0.1
# p2 <- 0.5
# p3 <- 0.2
# 
# df <- expand_ts(ts_tmp)
# df %>% head()
# 
# ll <- expand_ts(ts_tmp) %>% sample_outcomes(p1, p2, p3)
# 
# delays_df <- sample_delays(ll)
# 
# View(delays_df)
# 
# times_df <- compute_event_times_from_delays(delays)
# times_df |> View()
# 
# 
# compute_time_series_from_linelist(times_df) %>% View()
#   