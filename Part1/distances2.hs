distance3 f d s ss = filter (\z -> (f s z) <= d) ss

\ on lamda eli functio ilman nimeä

distance3 f d s ss = [y | y <- ss, (f s y) <= d]

molemmat toimii