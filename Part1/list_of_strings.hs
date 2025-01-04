headOrLast x y = [xs | xs <- x, head xs == y || last xs == y]

