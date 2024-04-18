double :: Num a => a ->a
double x = 2 * x

products :: Num a => [a] -> a
products [] = 1
products (x:xs) = x * products xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =  qsort larger ++ [x] ++ qsort smaller
                where
                    smaller = [a | a <- xs, a <= x]
                    larger = [a | a <- xs, a > x]
                     