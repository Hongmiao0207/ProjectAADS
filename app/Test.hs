
-- data Color = Red | Black
-- data RBTree = Leaf | Node Color RBTree Key RBTree

-- flatten :: RBTree a -> [a]
-- flatten Leaf = []
-- flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- ins :: RBTree → RBTree → RBTree
-- ins a Leaf = Node Red Leaf a Leaf
-- ins a t@(Node color t1 x t2)
--         | a < x = balance color (ins a t1) x t2
--         | a > x = balance color t1 x (ins a t2)
--         | otherwise = t

doubleSmallNumber x = if x > 100
        then x
        else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

lostNumbers = [4,8,15,16,23,48]

-- 入参：数组；出参：数组，二者中间用 ->表示
removeNoneUppercase :: [Char] -> [Char]
removeNoneUppercase st = [c | c <- st, c `elem` ['A'..'Z']] -- 接收数组，处理中，c属于st，c又是A到Z的元素，返回符合条件的c的st集合

-- [Char] ['a','b'] = String ab
removeNoneUppercase' :: String -> String
removeNoneUppercase' st = [c | c <- st, c `elem` ['A'..'Z']]

-- ->在这里表示分割，逗号，和返回值没有区别，最后一个就是返回值，前三个就是参数
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Integer在这里表示无界的整数，可以存放非常大的数，所以效率比int低
factorial :: Integer -> Integer
factorial n = product [1..n]  -- product[1..n] 从1乘到n

-- head类似java中的泛型，它表示他可以取任何类型的list作参数，并返回其中的第一个元素
-- fst 取的是包含两个可以是不同类型的变量的tuple作参数，并返回第一项作返回值；
-- Typeclass != class, but = interface in java
-- ==, /=
-- => 类型约束，(==) :: (Eq a) => a -> a -> Bool，这段表示：相等函数，取两个相同类别的值作为参数，返回一个布尔值
-- Eq接口，能判断相等性的类型，必实现了Eq接口
-- elem函数，类型为：(Eq a) => a -> [a] -> Bool；表示，检测值是否存在于一个list，返回布尔值
-- Ord接口 表示可以比较大小；compare函数比较时就必须要取Ord接口的值作参数，返回比较结果，结果为 GT(右边大), LT(左边大), EQ(相等)
-- Show接口表示可以用字符串表示
-- Read 将字符串转成Read的某个成员类型；read "5" :: Int；read "5" :: [Int]
-- Bounded 取上下限；minBound :: Int，return -21...；

-- ----------------- 模式匹配：通过检查数据的特定结构来检查其是否匹配，并按模式从中取得数据 -------------------------------------
lucky :: (Integral a) => a -> String    -- Integral和Integer有啥区别
lucky 7 = "LUCKY NUMBER SEVEN"          -- 当输入的是7，输出这个
lucky x = "Sorry"                       -- 输入其他的，输出这个
-- 类似 if-else

-- 类似 if-else树
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5" -- 如果这一行移到第二行，那么不论参数是多少，都只会输出这个

-- 递归实现阶乘
factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial'(n - 1) -- f(3) = 3 * f(2)；f(2) = 2 * f(1)；f(1) = 1* f(0)；f(0)=1

-- 失败的模式匹配，没有字符去调用它，只能输入abc
charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"

-- 二维数组相加
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)         -- 可行，但是有更好的替换方式

addVectors' :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a,b,c) -> a
first (x,_,_) = x       -- 也可以用a，可能为了默认习惯就是a用作定义方法，里面实现时用的参数就用其他字母

second :: (a,b,c) -> b
second (_,y,_) = y

third :: (a,b,c) -> c
third (_,_,z) = z

-- [(1,2),(3,4)] return [3,7]
listCom :: (Num a) => [(a,a)] -> [a]
listCom xs = [a+b|(a,b) <- xs]

-- 手动实现head
head' :: [a] -> a
head' [] = error "null!"
head' (x:_) = x                 -- :可以匹配list，因为[1,2,3] 等价于 1:2:3:[]，所以取首位就是 (x:_)；或者 x:xs

head'' :: [a] -> a
head'' (x:xs) = x      -- 其中 x是第一个元素，xs表示后面的元素，如果list只有一个元素，那就xs为NULL；其中x和xs可以用别的字母代替，最主要的是格式

-- x:xs这种格式，可以将list头部绑定为，尾部绑定为xs，如果想取前几个元素就 x:y:z:xs

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

tell' :: (Show a) => [a] -> String
tell' [] = "The list is empty"
tell' [x] = "The list has one element: " ++ show x
tell' [x,y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell' (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- 总结， (x:y:[]) = [x,y]，(x:[]) = [x]，(x:y:_)没有可以替换的

-- 也是一个递归
length' :: (Num b) => [a] -> b          -- 写成 b => [a] -> b 的形式可以查询字符串长度；a => [a] -> a 只能查询 list
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs

-- as模式，名字@放到模式前，可以在分割的时候保留整体，比如 xs@(x:y:ys)，此时x,y,ys是分割出来的，xs是分割之前的整体
capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]     -- x一定要用[x]，因为字符串拆出来的字符是通过字符数组的形式接收的
-- as通常就是为了在较大的模式中保留对整体的引用，减少重复性的工作
-- 模式匹配中不能使用 ++

-- 模式匹配是检车一个值是否合适并从中取值
-- Guards 则是用来检查一个值的某项属性是否为真，等同于 if
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
   | bmi <= 18.5 = "You're underweight, you emo, you!"
   | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
   | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
   | otherwise   = "You're a whale, congratulations!"
-- 一个guard就是一个布尔表达式，为返回对应函数体，为假就去下一个guard

bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height                                  -- 在guards中，参数后面不需要接 =
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                   = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' x y
    | x > y     = x
    | otherwise = y

-- 也可以写在一行
-- max' :: (Ord a) => a -> a -> a  
-- max' a b | a > b = a | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
myCompare x y   -- 其中 myCompare x y 可以写成 x `myCompare` y，这样可能更易读，但是调用的时候也可以输入 1 `myCompare` 2形式
    | x > y     = GT
    | x < y     = LT
    | otherwise = EQ
-- `method` 叫 中缀形式调用

-- 关键字 Where
-- 上面测体脂方法 bmiTell改写
bmiTell'' :: (RealFloat a) => a -> a -> String  
bmiTell'' weight height                                  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                   = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2

-- where就是定义变量的
bmiTell''' :: (RealFloat a) => a -> a -> String  
bmiTell''' weight height                                  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise                   = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat    = 30.0

-- where在模式匹配中的使用
bmiTell'''' :: (RealFloat a) => a -> a -> String  
bmiTell'''' weight height                                  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise                   = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- 取两个名字的首字符组合起来
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- 取第一个名字的首字符，取第二名字的除了首字符以外的全部字符组合
initials' :: String -> String -> String
initials' firstname lastname = [f] ++ ". " ++ xs ++ "."
    where (f:_) = firstname
          (x:xs) = lastname

-- 把bmi从一个变量，搞成可以给函数，参数就是weight 和 height
-- 传一个list，里面是二维元祖，前面是体重，后面是身高，
calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w,h) <- xs]
    where bmi weight height = weight / height ^ 2

-- 关键字 let，其绑定和where很像，where绑定是在函数底部定义名字；let绑定表达式，允许在任何位置定义局部变量
-- let 定义变量 in 执行表达式
-- let和where的区别就是，let是个表达式，where是个语法结构

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
        let sideArea = 2 * pi * r * h
            topArea  = pi * r ^ 2
        in  sideArea + 2 * topArea    


-- Case expressions，case语句
-- head' :: [a] -> a  
-- head' [] = error "No head for empty lists!"  
-- head' (x:_) = x
-- 等同于
-- head' :: [a] -> a  
-- head' xs = case xs of [] -> error "No head for empty lists!"  
--                       (x:_) -> x

-- 格式                     
-- case expression of pattern -> result  
--                    pattern -> result  
--                    pattern -> result  
--                    ...

-- 模式匹配的本质就是 case表达式的语法糖，所以模式匹配可以完全用case替代
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of []  -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs  -> "a longer list."

describeList' :: [a] -> String  
describeList' xs = "The list is " ++ what xs  
        where what []  = "empty."  
              what [x] = "a singleton list."  
              what xs  = "a longer list."


-- 递归
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "null"
maximum' [x] = x
maximum' (x:xs)
   | x > maxTail = x
   | otherwise = maxTail
   where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "null"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []                    -- 迭代完成
    | otherwise = x:replicate' (n-1) x  -- 返回以 x 为首个元素并后接重复n-1的x的list

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ 
    | n <= 0 = []
take' _ []   = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]    -- 反转的时候，无法想 x:xs一样，只能通过++来拼接

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys)
  | x == y = True
  | otherwise = x `elem'` ys

-- 快排
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
        let smallerSorted = quickSort [a| a <- xs, a <= x]
            biggerSorted = quickSort [a|a <- xs, a > x]
        in smallerSorted ++ [x] ++ biggerSorted