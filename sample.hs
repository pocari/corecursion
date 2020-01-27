module Sample where

data Tree a = Leaf | Node (Tree a) a (Tree a)
            deriving Show

t1 :: Tree Int
t1 =
  (Node (Node (Node Leaf 3 Leaf) 2 (Node Leaf 4 Leaf))
        1
        (Node (Node Leaf 6 Leaf) 5 (Node Leaf 7 Leaf))
  )

bfs :: Tree a -> [a]
bfs t = postprocess queue
 where
  queue       = t : walk 1 queue
  postprocess = map extract . filter isNode

--                 1(t0_0)
--                / \
--               /   \
--              /     \
--             /       \
--            /         \
--           2(t1_0)     5(t1_1)
--          / \         / \
--         /   \       /   \
--        3     4     6     7
--       / \   / \   / \   / \
--      L   L L   L L   L L   L

-- queue = t0_0 : walk 1 queue
-- queue = t0_0 : walk 1 (t0_0 : walk 1 queue)
-- queue = t0_0 : walk 1 ((Node t1_0 1 t1_1) : walk 1 queue)
-- queue = t0_0 : t1_0 : t1_1 : (walk 2 (walk 1 queue))
-- queue = t0_0 : t1_0 : t1_1 : (walk 2 (walk 1 t0_0 : t1_0 : t1_1 : (walk 2 (walk 1 queue))))
-- queue = t0_0 : t1_0 : t1_1 : (walk 2 (walk 1 (Node t1_0 1 t1_1) : q))
-- q1 = t1_0 : t1_1 : (walk 2 (walk 1 queue))
-- として
-- queue = t0_0 : t1_0 : t1_1 : (walk 2 t1_0 : t1_1 : walk 2 q1)
-- queue = t0_0 : t1_0 : t1_1 : (walk 2 (Node t2_0 2 t2_1) : t1_1 : walk 2 q1)
-- q2 = t1_1 : walk 2 q1
-- として
-- queue = t0_0 : t1_0 : t1_1 : (walk 2 (Node t2_0 2 t2_1) : q2)
-- queue = t0_0 : t1_0 : t1_1 : t2_0 : t2_1 : walk 3 q2
-- queue = t0_0 : t1_0 : t1_1 : t2_0 : t2_1 : walk 3 t1_1 : walk 2 q1
-- queue = t0_0 : t1_0 : t1_1 : t2_0 : t2_1 : walk 3 (Node t2_2 5 t2_3) : walk 2 q1
-- queue = t0_0 : t1_0 : t1_1 : t2_0 : t2_1 : t2_2 : t2_3 : walk 4 (walk 2 q1)
-- queue = t0_0 : t1_0 : t1_1 : t2_0 : t2_1 : t2_2 : t2_3 : walk 4 (walk 2 t1_0 : t1_1 : (walk 2 (walk 1 queue)))
-- queue = t0_0 : t1_0 : t1_1 : t2_0 : t2_1 : t2_2 : t2_3 : walk 4 (walk 2 (Node t2_0 2 t2_1) : t1_1 : (walk 2 (walk 1 queue)))
-- q3 = t1_1 : (walk 2 (walk 1 queue))
-- queue = t0_0 : t1_0 : t1_1 : t2_0 : t2_1 : t2_2 : t2_3 : walk 4 (walk 2 (Node t2_0 2 t2_1) : q3)
-- queue = t0_0 : t1_0 : t1_1 : t2_0 : t2_1 : t2_2 : t2_3 : walk 4 (t2_0 : t2_1 : walk 3 q3)
-- queue = t0_0 : t1_0 : t1_1 : t2_0 : t2_1 : t2_2 : t2_3 : walk 4 ((Node Leaf 3 Leaf) : t2_1 : walk 3 q3)
-- q4 = t2_1 : walk 3 q3
-- queue = t0_0 : t1_0 : t1_1 : t2_0 : t2_1 : t2_2 : t2_3 : walk 4 ((Node Leaf 3 Leaf) : q4)
-- queue = t0_0 : t1_0 : t1_1 : t2_0 : t2_1 : t2_2 : t2_3 : walk 4 (Node Leaf 3 Leaf) : q4
-- queue = t0_0 : t1_0 : t1_1 : t2_0 : t2_1 : t2_2 : t2_3 : Leaf : Leaf : walk 5 q4
-- queue = t0_0 : t1_0 : t1_1 : t2_0 : t2_1 : t2_2 : t2_3 : Leaf : Leaf : walk 5 t2_1 : walk 3 q3
-- queue = t0_0 : t1_0 : t1_1 : t2_0 : t2_1 : t2_2 : t2_3 : Leaf : Leaf : walk 5 (Leaf 4 Leaf) : walk 3 q3
-- queue = t0_0 : t1_0 : t1_1 : t2_0 : t2_1 : t2_2 : t2_3 : Leaf : Leaf : Leaf : Leaf : walk 6 (walk 3 t1_1 : (walk 2 (walk 1 queue)))
walk :: Int -> [Tree a] -> [Tree a]
walk 0 _                = []
walk n (Leaf       : q) = walk (n - 1) q
-- ここで展開した l, r に対してまた walk n' (Node l' _ r') がマッチすることで、どんどんレベル毎にノードを並べていくことになる
-- で最終的にLeafも含めて レベル毎に1段下がって 左 -> 右また一段下がって 左 -> 右 という順でLeafまで含めて並び
-- t0_0 : t1_0 : t1_1 : t2_0 : t2_1 : t2_2 : t2_3 : Leaf(親3) : Leaf(親3) : Leaf(親4) : Leaf(親4) : Leaf(親6) : Leaf(親6) : Leaf(親7) : Leaf(親7) : []
-- になる
walk n (Node l _ r : q) = l : r : walk (n + 1) q
walk _ _                = error "walk: invalid pattern"

extract :: Tree t -> t
extract (Node _ x _) = x
extract _            = error "extract: invalid pattern"

isNode :: Tree a -> Bool
isNode (Node _ _ _) = True
isNode _            = False

