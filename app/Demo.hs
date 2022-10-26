
method :: Int -> Int
method 0 = 0
method 1 = 1
method n = method(n-1) + method(n-2)

-- 获取列表最大值

getMaxValue :: (Ord a) => [a] -> a
getMaxValue [] = error "empty list"
getMaxValue [x] = x
getMaxValue (x:xs)
        | x > maxTail = x
        | otherwise = maxTail
        where maxTail = getMaxValue xs