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
