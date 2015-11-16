import Data.List
quickSort []     = []
quickSort (x:xs) = quickSort[t|t<-xs,t<x]++[x]++quickSort [t|t<-xs,t>=x]

fact 0   = 1
fact n   = n*fact(n-1)

binom n k   = (if n>k then fact(n)/(fact(n-k)*fact(k)) else -1)

--for y find t1,t2:t1+t2=y 
findSumsElements y = [[t1,t2]|t1<-t1s,t2<-t2s,t1+t2==y]
 where t1s = [t|t<-[0..y]]
       t2s = [t|t<-[0..y]]
--or easiest way
--[[t1,t2]|t1<-[1..5],t2<-[1..5],t1+t2==5]

--for y find c1,c2: y=binom(c1,1)+binom(c2,2)
binomNumericalSystemFor2 y = [[c1,c2]|c1<-xs,c2<-xs,x1<-xs,x2<-xs,x1+x2==y, binom c1 1 ==x1,binom c2 2 ==x2]
 where xs = [1..y]

--sum element of list
--for demonstrate foldl application 
sumM []    = 0
sumM a     = foldl (\ acc x->acc+x) 0  a

--we need it because, in haskell, no chance for list[i]=someValue 
changeElmOfList list k = take k list ++[list!!k+1]++drop (k+1) list

-- addList List1 List2 adding one to List1[List2[i]] elements for every i
-- and return sum of this list's (i.e. list of lists)
--addList  list []     = [[]]
--addList  list (s:ss) = (changeElmOfList list s) ++ (addList list ss) 

--find  [c_1,..,c_n|binom(c_1,1)+..+binom(c_n,n)=X]
--use like
-- binomNumericalSystem 15 [0,0]
-- return [9,4] i.e. 15 = binom(9,1)+binom(4,2)
binomNumericalSystem x list 
        |(sum list)==x = [seqBinoms x list [] 1]--[list]
        |all (<x) list = addList binomNumericalSystem x list [0 .. (length list)-1]
        |otherwise     = [[]]
        where addList f x list []     = []
              addList f x list (s:ss) = f x (changeElmOfList list s) ++ addList f x list ss  

-- condithions for binom's
-- find  C1,...,Ck : binom(C1,1)=s1,...,binom(Ck,k)=sk
-- c<-[0..x]
-- k<-[1..length (s:ss)]
-- list for saving c
-- add after |(sum list)==x = in function f
--b :: (Enum a, Fractional a, Num [a],Num a, Ord a) => a -> [a] -> [a] -> a -> [a]
--init k=1
--use like
-- seqBinoms 15 [9,6] [] 1  (where 15=9+6)
-- [9,4] i.e. 15 = binom(9,1)+binom(4,2)
seqBinoms x [] list k   = list
seqBinoms x (s:ss) list k
--          |True          = cc
          |(length cc)>0 = seqBinoms x ss (list++cc) (k+1)
          |otherwise     = []
          where cc = [c|c<-[1..s],binom c k ==s]
