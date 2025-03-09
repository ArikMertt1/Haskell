module Sort where
insert x [] = [x]
insert x (y : ys)
    | x<= y = x : y : ys
    | otherwise = y : insert x ys

insertSort [] = []
insertSort (x : xs) = insert x (insertSort xs)

merge xs [] = xs 
merge [] ys = ys

merge (x : xs) (y : ys)
    | x <= y    = x : merge xs (y : ys)
    | otherwise = y : merge (x : xs) ys

mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge
   (mergeSort (take (length xs `div` 2) xs))
   (mergeSort (drop (length xs `div` 2) xs))

bubbleSort xs =
  let ys = bubble xs []
  in if xs == ys then xs else bubbleSort ys

bubble [] acc = reverse acc
bubble [x] acc = reverse (x : acc)
bubble (x : y : xs) acc
     |x>y = bubble (x : xs) (y : acc) 
     | otherwise = bubble (y : xs) (x : acc)

     

