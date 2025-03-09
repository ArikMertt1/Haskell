square x = x * x
cube x = x * square x

main = putStr "Hallo Welt!"

max3 x y z
 | x>y && x>z = x
 | y>x && y>z = y
 |otherwise = z