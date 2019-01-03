h = head [1,2,3]
t = tail [1,2,3]

a = 1:[]
xs = 1:2:3:4:[]
ys = (1,2):(3,4):(5,6):[]
zs = 1:[2,3,4]

b = [1] ++ [2,3,4]

range = [1,3 .. 10]
unending = [1 .. ]

elemPrefix = elem 'p' "cheese"
elemInfix  = 'p' `elem` "cheese"

myRepeat n = cycle [n]

subseq start end list = take difference (drop start list)
  where difference = end - start

inFirstHalf val list = elem val firstHalf
  where midpoint = (length list) `div` 2
        firstHalf = take midpoint list
