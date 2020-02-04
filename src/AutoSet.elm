module AutoSet exposing 
    ( 
        Set
        , empty, singleton, insert, remove
        , isEmpty, member, size, eq
        , toList, fromList
        , map, foldl, foldr, filter, partition
        , union, intersect, diff
        , replaceMapping
    )

{-|

@docs Set
# Build
@docs empty, singleton, insert, remove

# Query
@docs isEmpty, member, size, eq

# Lists
@docs toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

# Combine
@docs union, intersect, diff

# Hashing related
@docs replaceMapping
-}

import ManualSet as BaseSet

{-|
-}
type Set comparable v
    = Set (v -> comparable) (BaseSet.Set comparable v)

{-|
-}
empty : (v -> comparable) -> Set comparable v
empty f = Set f BaseSet.empty

{-|
-}
singleton : (v -> comparable) -> v -> Set comparable v
singleton f value = Set f (BaseSet.singleton f value)

{-|
-}
insert : v -> Set comparable v -> Set comparable v
insert value (Set f set) = Set f <| BaseSet.insert f value set

{-|
-}
remove : v -> Set comparable v -> Set comparable v
remove value (Set f set) = Set f <| BaseSet.remove f value set

{-|
-}
isEmpty : Set comparable v -> Bool
isEmpty (Set _ set) = BaseSet.isEmpty set

{-|
-}
member : v -> Set comparable v -> Bool
member value (Set f set) = BaseSet.member f value set

{-|
-}
size : Set comparable v -> Int
size (Set _ set) = BaseSet.size set

{-|
-}
eq : Set comparable1 v -> Set comparable2 v -> Bool
eq leftSet rightSet
    = (diff leftSet rightSet |> isEmpty)
    && (diff rightSet leftSet |> isEmpty)

{-|
-}
toList : Set comparable v -> List v
toList (Set _ set) = BaseSet.toList set

{-|
-}
fromList : (v -> comparable) -> List v -> Set comparable v
fromList f list = Set f <| BaseSet.fromList f list

{-|
-}
map : 
    (v2 -> comparable) 
    -> (v1 -> v2) 
    -> Set comparable v1 
    -> Set comparable v2
map f updateFunc set
    = foldl (\v s -> insert (updateFunc v) s) (empty f) set

{-|
-}
foldl : (v -> a -> a) -> a -> Set comparable v -> a
foldl combine start (Set _ set)
    = BaseSet.foldl combine start set

{-|
-}
foldr : (v -> a -> a) -> a -> Set comparable v -> a
foldr combine start (Set _ set)
    = BaseSet.foldr combine start set

{-|
-}
filter : (v -> Bool) -> Set comparable v -> Set comparable v
filter pred (Set f set)
    = Set f <| BaseSet.filter pred set

{-|
-}
partition : 
    (v -> Bool) 
    -> Set comparable v 
    -> (Set comparable v, Set comparable v)
partition pred (Set f set)
    = BaseSet.partition pred set
    |> Tuple.mapBoth (Set f) (Set f)

{-|
-}
union : 
    Set comparable1 v 
    -> Set comparable2 v 
    -> Set comparable2 v
union leftSet rightSet
    = foldl (\v s -> insert v s) rightSet leftSet

{-|
-}
intersect : Set comparable1 v -> Set comparable2 v -> Set comparable1 v
intersect leftSet rightSet
    = filter (\v -> member v rightSet) leftSet

{-|
-}
diff : Set comparable1 v -> Set comparable2 v -> Set comparable1 v
diff leftSet rightSet
    = foldl (\v s -> remove v s) leftSet rightSet

{-|
-}
replaceMapping : (v -> comparable2) -> Set comparable1 v -> Set comparable2 v
replaceMapping f set
    = foldl (\v s -> insert v s) (empty f) set