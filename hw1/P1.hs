izvod f = \x -> (f (x + 0.001) - f x) / 0.001
    where deltax = 0.001