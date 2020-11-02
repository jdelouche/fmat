import Prelude
m=[[1,2,3],[4,5,6],[7,8,9]]
i=[[1,0,0],[0,1,0],[0,0,1]]
r=[[0,0,1],[0,1,0],[1,0,0]]
reduce coprod prod h v = coprod (zipWith (prod) h v)
(*+!) = reduce (sum) (*)
(==!) = reduce (and) (==)
rot m = fmap (\i -> fmap (!!(i-1)) m) [1..(length (m!!0))]
cell i j m n = m!!i *+! ((rot n)!!j)
hcell i m n = fmap (\j -> cell i (j-1) m n) [1..(length n)]
mul m n = fmap (\i -> hcell (i-1) m n) [1..(length m)]
main = do print (mul m i);
