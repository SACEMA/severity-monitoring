Scenario list:

options:
D1: a, b, c, d, e
D2: a,b
D3: a,b
D4: a,b

D1: flat, up, down, exp, decay
D2: flat, twovals
D3: flat, twovals
D4: flat, twovals

D1a : params.RData D1ascript.R

for each D1 option we have:
D1:a
D2-D4:
[] a,a,a
[] a,a,b
[] a,b,b
[] a,b,a
[] b,a,a
[] b,b,a
[] b,b,b
[] b,a,b

D1:b
D2-D4:
[] a,a,a
[] a,a,b
[] a,b,b
[] a,b,a
[] b,a,a
[] b,b,a
[] b,b,b
[] b,a,b

D1: c
D2-D4:
[] a,a,a
[] a,a,b
[] a,b,b
[] a,b,a
[] b,a,a
[] b,b,a
[] b,b,b
[] b,a,b

D1: d
D2-D4:
[] a,a,a
[] a,a,b
[] a,b,b
[] a,b,a
[] b,a,a
[] b,b,a
[] b,b,b
[] b,a,b

D1: e
D2-D4:
[] a,a,a
[] a,a,b
[] a,b,b
[] a,b,a
[] b,a,a
[] b,b,a
[] b,b,b
[] b,a,b