import Prelude
m23 = [[ 1, 2, 3],
       [11,12,13]]
m32 = [[ 1,11],
       [ 2,12],
       [ 3,13]]
m22 = [[ 1, 2],
       [11,12]]
i   = [[1,0,0],
       [0,1,0],
       [0,0,1]]
sh  = [[0,1,0],
       [1,0,0],
       [0,0,1]]
sv t = rot ((***) sh (rot t))
t   = [[1,2,3],
       [4,5,6],
       [7,8,9]]
sub = fmap (tail)
subm n = (foldr (.) id (fmap (\x -> sub) [1..n]))
rot m = fmap (\i -> fmap (head) (subm (i-1) m)) [1..(length (m!!0))]
cell i j m23 m32 = sum (zipWith (*) (m23!!i) ((rot m32)!!j))
cellp i j m23 m32 = (zipWith (,) (m23!!i) ((rot m32)!!j))
mul c m23 m32 = [ [c j i m23 m32|i<-[0..((length m23)-1)]] | j <- [0..((length m23)-1)] ]
(***)::[[Integer]] -> [[Integer]] -> [[Integer]];(***) = (mul cell);
main = do print (mul cell m23 m32);print (mul cell m22 m32);print (mul cell m22 m23)
