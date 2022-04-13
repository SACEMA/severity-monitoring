# Synthetic data scenarios

### Dimension names 

D1: const, up, down, exp, decay 

D2: const, twovals

D3: const, twovals

D4: const, twovals

D1a : params.RData D1ascript.R


#### Dimension meanings
- const: a const/constant time series (same values from start to a time point)
- up: linearly increasing upwards
- down: linearly decreasing downwards
- exp: exponentially increasing
- decay: exponentially decaying
- twovals: changes linearly at two "rates" at two time points


Data scenarios are created so that for each D1 option we have:

D1 (const) & combinations of options from D2, D3, D4:

| D1 | D2 | D3 | D4 | scenario_name |
:---|:--|:--|:--|:------------|
| const | const | const | const | const_const_const_const |
| const | const | const | twovals | const_const_const_twovals |
| const | const | twovals | twovals | const_const_twovals_twovals |
| const | const | twovals | const | const_const_twovals_const |
| const | twovals | const | const | const_twovals_const_const |
| const | twovals | twovals | const | const_twovals_twovals_const |
| const | twovals | twovals | twovals | const_twovals_twovals_twovals |
| const | twovals | const | twovals | const_twovals_const_twovals | 

D1 (up) & combinations of options from D2, D3, D4:

| D1 | D2 | D3 | D4 | scenario_name |
:---|:--|:--|:--|:------------|
| up | const | const | const | up_const_const_const |
| up | const | const | twovals | up_const_const_twovals |
| up | const | twovals | twovals | up_const_twovals_twovals |
| up | const | twovals | const | up_const_twovals_const |
| up | twovals | const | const | up_twovals_const_const |
| up | twovals | twovals | const | up_twovals_twovals_const |
| up | twovals | twovals | twovals | up_twovals_twovals_twovals |
| up | twovals | const | twovals | up_twovals_const_twovals | 

D1 (const) & combinations of options from D2, D3, D4:

| D1 | D2 | D3 | D4 | scenario_name |
:---|:--|:--|:--|:------------|
| down | const | const | const | down_const_const_const |
| down | const | const | twovals | down_const_const_twovals |
| down | const | const | twovals | down_const_const_twovals |
| down | const | twovals | twovals | down_const_twovals_twovals |
| down | const | twovals | const | down_const_twovals_const |
| down | twovals | const | const | down_twovals_const_const |
| down | twovals | twovals | const | down_twovals_twovals_const |
| down | twovals | twovals | twovals | down_twovals_twovals_twovals |
| down | twovals | const | twovals | down_twovals_const_twovals | 

D1 (exp) & combinations of options from D2, D3, D4:

| D1 | D2 | D3 | D4 | scenario_name |
:---|:--|:--|:--|:------------|
| exp | const | const | const | exp_const_const_const |
| exp | const | const | twovals | exp_const_const_twovals |
| exp | const | const | twovals | exp_const_const_twovals |
| exp | const | twovals | twovals | exp_const_twovals_twovals |
| exp | const | twovals | const | exp_const_twovals_const |
| exp | twovals | const | const | exp_twovals_const_const |
| exp | twovals | twovals | const | exp_twovals_twovals_const |
| exp | twovals | twovals | twovals | exp_twovals_twovals_twovals |
| exp | twovals | const | twovals | exp_twovals_const_twovals | 

D1 (decay) & combinations of options from D2, D3, D4:

| D1 | D2 | D3 | D4 | scenario_name |
:---|:--|:--|:--|:------------|
| decay | const | const | const | decay_const_const_const |
| decay | const | const | twovals | decay_const_const_twovals |
| decay | const | const | twovals | decay_const_const_twovals |
| decay | const | twovals | twovals | decay_const_twovals_twovals |
| decay | const | twovals | const | decay_const_twovals_const |
| decay | twovals | const | const | decay_twovals_const_const |
| decay | twovals | twovals | const | decay_twovals_twovals_const |
| decay | twovals | twovals | twovals | decay_twovals_twovals_twovals |
| decay | twovals | const | twovals | decay_twovals_const_twovals | 
