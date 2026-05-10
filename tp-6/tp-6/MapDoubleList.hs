module MapDoubleL
    (Map, emptyM, assocM, lookupM, deleteM, keys) 
where

data Map k v = M [k] [v]

emptyM  :: Map k v                              -- 
assocM  :: Eq k => k -> v -> Map k v -> Map k v -- 
lookupM :: Eq k => k -> Map k v -> Maybe v      -- 
deleteM :: Eq k => k -> Map k v -> Map k v      -- 
keys    :: Map k v -> [k]                       -- 

emptyM = (M [] [])
assocM nk nv (M ks vs) = let assoccedLists = assocDL nk nv (ks, vs)
                            in (M (first assoccedLists) (second assoccedLists))

lookupM k (M bs) = lookupML k bs
deleteM k (M bs) = (M (deleteML k bs))
keys (M bs) = keysM bs

assocDL :: Eq k => k -> v -> ([k], [v]) -> ([k], [v])
assocDL k' v' ((k:ks),(v:vs)) = if (k' == k)
                                    then ((k':ks), (v':vs))
                                    else consACada (k, v) assocDL k' v' ((ks),(vs))
assocDL k' v' _ = ([k'], [v'])

consACada :: (a,b) -> ([a], [b]) -> ([a], [b])
consACada (a,b) (as,bs) = ((a:as),(b:bs)) 
consACada (a,b) ([],_) = ([a],[b]) 