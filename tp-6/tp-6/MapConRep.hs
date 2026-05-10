module MapConRep
    (Map, emptyM, assocM, lookupM, deleteM, keys) 
where

data Map k v = M [(k,v)]

emptyM  :: Map k v                              -- O(1)
assocM  :: Eq k => k -> v -> Map k v -> Map k v -- O(n) n = cant.elementos
lookupM :: Eq k => k -> Map k v -> Maybe v      -- O(n) n = cant.elementos
deleteM :: Eq k => k -> Map k v -> Map k v      -- O(n) n = cant.elementos
keys    :: Map k v -> [k]                       -- O(n) n = cant.elementos

emptyM = (M [])
assocM nk nv (M bs) = (M ((k,v):bs))
lookupM k (M bs) = lookupML k bs
deleteM k (M bs) = (M (deleteML k bs))
keys (M bs) = keysM bs

lookupML :: Eq k => k -> [(k,v)] -> Maybe v
lookupML k' ((k,v):bs) = if (k' == k) then Just v else lookupML k' bs
lookupML _  _ = Nothing

deleteML :: Eq k => k -> [(k,v)] -> [(k,v)]
deleteML k' ((k,v):bs) = if (k' == k) then bs else (k,v):(deleteML k' bs) 
deleteML _  _          = []

keysM :: [(k,v)] -> [k]
keysM ((k:_):bs) = k:(keysM bs)
keysM _          = []