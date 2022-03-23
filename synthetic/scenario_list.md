# Synthetic data scenarios

### Dimension names with corresponding alphabetical shortcuts

D1: flat (a), up (b), down (c), exp (d), decay (e)

D2: flat (a), twovals (b)

D3: flat (a), twovals (b)

D4: flat (a), twovals (b)

D1a : params.RData D1ascript.R


#### Dimension meanings
- flat: a flat/constant time series (same values from start to a time point)
- up: linearly increasing upwards
- down: linearly decreasing downwards
- exp: exponentially increasing
- decay: exponentially decaying
- twovals: changes linearly at two "rates" at two time points


Data scenarios are created so that for each D1 option we have:

D1 (a) & combinations of options from D2, D3, D4:

| D1 | D2 | D3 | D4 | scenario_name |
:---|:--|:--|:--|:------------|
| a | a | a | a | flat_flat_flat_flat |
| a | a | a | b | flat_flat_flat_twovals |
| a | a | b | b | flat_flat_twovals_twovals |
| a | a | b | a | flat_flat_twovals_flat |
| a | b | a | a | flat_twovals_flat_flat |
| a | b | b | a | flat_twovals_twovals_flat |
| a | b | b | b | flat_twovals_twovals_twovals |
| a | b | a | b | flat_twovals_flat_twovals | 

D1 (b) & combinations of options from D2, D3, D4:

| D1 | D2 | D3 | D4 | scenario_name |
:---|:--|:--|:--|:------------|
| b | a | a | a | up_flat_flat_flat |
| b | a | a | b | up_flat_flat_twovals |
| b | a | b | b | up_flat_twovals_twovals |
| b | a | b | a | up_flat_twovals_flat |
| b | b | a | a | up_twovals_flat_flat |
| b | b | b | a | up_twovals_twovals_flat |
| b | b | b | b | up_twovals_twovals_twovals |
| b | b | a | b | up_twovals_flat_twovals | 

D1 (c) & combinations of options from D2, D3, D4:

| D1 | D2 | D3 | D4 | scenario_name |
:---|:--|:--|:--|:------------|
| c | a | a | a | down_flat_flat_flat |
| c | a | a | b | down_flat_flat_twovals |
| c | a | b | b | down_flat_twovals_twovals |
| c | a | b | a | down_flat_twovals_flat |
| c | b | a | a | down_twovals_flat_flat |
| c | b | b | a | down_twovals_twovals_flat |
| c | b | b | b | down_twovals_twovals_twovals |
| c | b | a | b | down_twovals_flat_twovals | 

D1 (d) & combinations of options from D2, D3, D4:

| D1 | D2 | D3 | D4 | scenario_name |
:---|:--|:--|:--|:------------|
| d | a | a | a | exp_flat_flat_flat |
| d | a | a | b | exp_flat_flat_twovals |
| d | a | b | b | exp_flat_twovals_twovals |
| d | a | b | a | exp_flat_twovals_flat |
| d | b | a | a | exp_twovals_flat_flat |
| d | b | b | a | exp_twovals_twovals_flat |
| d | b | b | b | exp_twovals_twovals_twovals |
| d | b | a | b | exp_twovals_flat_twovals | 

D1 (e) & combinations of options from D2, D3, D4:

| D1 | D2 | D3 | D4 | scenario_name |
:---|:--|:--|:--|:------------|
| e | a | a | a | decay_flat_flat_flat |
| e | a | a | b | decay_flat_flat_twovals |
| e | a | b | b | decay_flat_twovals_twovals |
| e | a | b | a | decay_flat_twovals_flat |
| e | b | a | a | decay_twovals_flat_flat |
| e | b | b | a | decay_twovals_twovals_flat |
| e | b | b | b | decay_twovals_twovals_twovals |
| e | b | a | b | decay_twovals_flat_twovals | 
