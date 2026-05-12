module MultiSet (
    MultiSet,
    emptyMS addMS,
    ocurrencesMS,
    unionMS,
    intersectionMS,
    multiSetToList
) where

data MultiSet k = MS (Map k v)

emptyMS :: MultiSet a
addMS :: Ord a => a -> MultiSet a -> MultiSet a
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a -- (opcional)
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a -- (opcional)
multiSetToList :: MultiSet a -> [(a, Int)]

emptyMS = emptyM
addMS k (MS map) = MS (assocM k (justOrCero (lookupM k map) +1) map)
ocurrencesMS k (MS map) = lookupM k map
-- unionMS (MS map1) (MS map2) 
-- intersectionMS
multiSetToList (MS map) = mapToList map

justOrCero :: Maybe Int -> Int
justOrCero (Just x) = x
justOrCero Nothing = 0


mapToList :: Eq k => Map k v -> [(k, v)]
mapToList map = mapToListK (keysM map) map

mapToListK :: Eq k => [k] -> Map k v -> [(k, v)]
mapToListK (k:ks) map = (k, (lookupM k map)) : mapToListK ks map
mapToListK _      _   = []


