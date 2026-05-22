module (
    BST,
    insertBST,
    deleteBST,
    splitMinBST,
    splitMaxBST,
    esBST,
    elMaximoMenorA,
    elMinimoMayorA,
    balanceado
) where 

data BST a = Tree a



belongsBST :: Ord a => a-> Tree a -> Bool
-- Costo: O(log N)
insertBST :: Ord a => a-> Tree a-> Tree a
-- Costo: O(log N)
deleteBST :: Ord a => a-> Tree a-> Tree a
-- Costo: O(log N)
splitMinBST :: Ord a => Tree a-> (a, Tree a)
-- Costo: O(log N)
splitMaxBST :: Ord a => Tree a-> (a, Tree a)
-- Costo: O(log N)
esBST :: Tree a-> Bool
--Costo: O(N2)
elMaximoMenorA :: Ord a => a-> Tree a-> Maybe a
-- Costo: O(log N)
elMinimoMayorA :: Ord a => a-> Tree a-> Maybe a
-- Costo: O(log N)
balanceado :: Tree a-> Bool
--Costo: O(N2)