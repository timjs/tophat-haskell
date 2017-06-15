module Main

import StdEnv

import itasks

add x y = edit (x + y)

//Start world = normalize (edit "foo" -&&- edit True -&&- edit 10) world
//Start world = normalize (edit 10 >>= add 5 >>= add 3) world
//Start world = normalize (edit 10 >>= \x -> edit (x + 5) >>= \x -> edit (x + 3)) world
//Start world = normalize (edit 10 >>= \x -> edit (x + 5) -&&- edit (x + 3)) world
Start world = normalize (retrn 10 >>= edit) world
