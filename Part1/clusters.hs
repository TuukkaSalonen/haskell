clusters f d ss = [distance3 f d s ss | s <- ss]

distance3 f d s ss = filter (\z -> (f s z) <= d) ss