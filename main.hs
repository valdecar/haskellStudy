import Data.List
quickSort []     = []
quickSort (x:xs) = quickSort[t|t<-xs,t<x]++[x]++quickSort [t|t<-xs,t>=x]

fact 0   = 1
fact n   = n*fact(n-1)

binom n k   = (if n>k then fact(n)/(fact(n-k)*fact(k)) else -1)
 
findSumsElements y = [[t1,t2]|t1<-t1s,t2<-t2s,t1+t2==y]
 where t1s = [t|t<-[0..y]]
       t2s = [t|t<-[0..y]]
--[[t1,t2]|t1<-[1..5],t2<-[1..5],t1+t2==5]

binomSist y = [[c1,c2]|c1<-[1..y],c2<-[1..y],x1<-[1..y],x2<-[1..y],x1+x2==y, binom c1 1 ==x1,binom c2 2 ==x2]
-- where x1s = [t !! 1|t<-g y]
--       x2s = [t !! 2|t<-g y] 
--g y = [[t1,t2]|t1+t2==y,(t1:t2)<-ts]
-- where ts = [[t1,t2]|t1<-[0..y],t2<-[0..y]]
--sumM [[]] = [[]]
sumM []    = []
sumM a     = foldl (\ acc x->acc++[x]) [[]]  a
changeElmOfList list k = take k list ++[list!!k+1]++drop (k+1) list
--addList  list []     = [[]]
--addList  list (s:ss) = (changeElmOfList list s) ++ (addList list ss) 
f x list 
        |(sum list)==x = [b x list [] 1]--[list]
        |all (<x) list = addList f x list [0 .. (length list)-1]
        |otherwise     = [[]]
        where addList f x list []     = []
              addList f x list (s:ss) = f x (changeElmOfList list s) ++ addList f x list ss  
-- condithions for binom's
-- c<-[0..x]
-- k<-[1..length (s:ss)]
-- list for saving c
-- add after |(sum list)==x = in function f
--b :: (Enum a, Fractional a, Num [a],Num a, Ord a) => a -> [a] -> [a] -> a -> [a]
b x [] list k   = list
b x (s:ss) list k
--          |True          = cc
          |(length cc)>0 = b x ss (list++cc) (k+1)
          |otherwise     = []
          where cc = [c|c<-[1..s],binom c k ==s]
