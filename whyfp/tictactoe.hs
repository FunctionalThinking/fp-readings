import Data.List 

data Tree a = Node a [Tree a] deriving (Show,Eq)
foldtree f g a (Node label subtrees)= f label (foldr g a (map (foldtree f g a) subtrees))
maptree f = foldtree (Node . f) (:) []
reptree f a =  Node a (map (reptree f) (f a)) -- will be defined in TicTacToe example

data Cell = E | O | X deriving (Show, Eq, Ord)
type Position = [Cell]

xflip (a:b:c:d:e:f:g:h:i:[]) = [c,b,a,f,e,d,i,h,g]
yflip (a:b:c:d:e:f:g:h:i:[]) = [g,h,i,d,e,f,a,b,c]
xyflip = yflip . rot90
yxflip = xflip . rot90

rot90 (a:b:c:d:e:f:g:h:i:[]) = [g,d,a,h,e,b,i,f,c]
rot180 = rot90 . rot90
rot270 = rot180 . rot90

equiv p1 p2 = p1 `elem` sequence[id,xflip,yflip,xyflip,yxflip,rot90,rot180,rot270]p2

moves p = nubBy equiv $ go p 
            where go [] = []
                  go (E:rest) = (player:rest) : map (E:) (go rest)
                  go (x:rest) = map (x:) (go rest)
                  player = turn p

turn p = if no == nx then X else O
            where f O (no,nx) = (no+1, nx)
                  f X (no,nx) = (no, nx+1)
                  f E (no,nx) = (no, nx)
                  (no,nx) = foldr f (0,0) p

gametree p = reptree moves p

static :: Position -> Int
static position 
    | win X position = 1
    | win O position = -1
    | otherwise = 0

win :: Cell -> Position -> Bool
win player position = any (all (== player)) $ allLines position

allLines [a,b,c,d,e,f,g,h,i] = [[a,b,c],[d,e,f],[g,h,i]
                               ,[a,d,g],[b,e,h],[c,f,i]
                               ,[a,e,i],[c,e,g]]
maximize = maximum . maximize' 

maximize' (Node x []) = [x]
maximize' (Node x sub) = mapmin (map minimize' sub)

mapmin :: [[Int]] -> [Int]
mapmin (nums:rest) = minimum nums : omit (minimum nums) rest

omit pot [] = []
omit pot (nums:rest) 
    | minleq nums pot = omit pot rest
    | otherwise = minimum nums : omit (minimum nums) rest

minleq [] pot = False
minleq (n:rest) pot 
    | n <= pot = True
    | otherwise = minleq rest pot

minimize = minimum . minimize' 

minimize' (Node x [])  = [x]
minimize' (Node x sub) = map maximize sub

evaluate :: Position -> Int
evaluate = maximum . maximize' . maptree static . prune 8 . gametree
-- evaluate = maximize . maptree static . prune 5 . gametree
--evaluate = maximize . maptree static . gametree


prune 0 (Node x sub) = Node x []
prune n (Node x sub) = Node x (map (prune (n-1)) sub)








