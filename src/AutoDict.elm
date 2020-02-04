module AutoDict exposing 
    (
        Dict
        , empty, singleton, insert, update, remove
        , isEmpty, member, get, size, eq
        , keys, values, toList, fromList
        , map, foldl, foldr, filter, partition
        , union, intersect, diff, merge
        , replaceMapping
    )

{-|

# Dictionaries
@docs Dict

# Build
@docs empty, singleton, insert, remove, update

# Query
@docs isEmpty, member, get, size, eq

# Lists
@docs keys, values, toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

# Combine
@docs union, intersect, diff, merge

# Hashing relatet
@docs replaceMapping
-}

import ManualDict as BaseDict

{-|
-}
type Dict comparable k v
    = Dict (k -> comparable) (BaseDict.Dict comparable k v)

-- helper

mapInternal : 
    (BaseDict.Dict comparable k a -> BaseDict.Dict comparable k b) 
    -> Dict comparable k a 
    -> Dict comparable k b
mapInternal mapFunc (Dict f dict)
    = Dict f (mapFunc dict)

{-|
-}
empty : (k -> comparable) -> Dict comparable k v
empty f = Dict f BaseDict.empty

{-|
-}
singleton : (k -> comparable) -> k -> v -> Dict comparable k v
singleton f key value
    = Dict f (BaseDict.singleton f key value)

{-|
-}
insert : k -> v -> Dict comparable k v -> Dict comparable k v
insert key value (Dict f dict)
    = Dict f (BaseDict.insert f key value dict)

{-|
-}
update : k -> (Maybe v -> Maybe v) -> Dict comparable k v -> Dict comparable k v
update key updateFunc (Dict f dict)
    = Dict f (BaseDict.update f key updateFunc dict)

{-|
-}
remove : k -> Dict comparable k v -> Dict comparable k v
remove key (Dict f dict)
    = Dict f (BaseDict.remove f key dict)

{-|
-}
isEmpty : Dict comparable k v -> Bool
isEmpty (Dict _ dict) = BaseDict.isEmpty dict

{-|
-}
member : k -> Dict comparable k v -> Bool
member key (Dict f dict)
    = BaseDict.member f key dict

{-|
-}
get : k -> Dict comparable k v -> Maybe v
get key (Dict f dict)
    = BaseDict.get f key dict

{-|
-}
size : Dict comparable k v -> Int
size (Dict _ dict) = BaseDict.size dict

{-|
-}
eq : Dict comparable1 k v -> Dict comparable2 k v -> Bool
eq leftDict rightDict
    = (diff leftDict rightDict |> isEmpty)
    && (diff rightDict leftDict |> isEmpty)

{-|
-}
keys : Dict comparable k v -> List k
keys (Dict _ dict) = BaseDict.keys dict

{-|
-}
values : Dict comparable k v -> List v
values (Dict _ dict) = BaseDict.values dict

{-|
-}
toList : Dict comparable k v -> List (k,v)
toList (Dict _ dict) = BaseDict.toList dict

{-|
-}
fromList : (k -> comparable) -> List (k,v) -> Dict comparable k v
fromList f list 
    = Dict f (BaseDict.fromList f list)

{-|
-}
map : (k -> a -> b) -> Dict comparable k a -> Dict comparable k b
map updateFunc dict
    = mapInternal (BaseDict.map updateFunc) dict

{-|
-}
foldl : (k -> v -> b -> b) -> b -> Dict comparable k v -> b
foldl combine start (Dict _ dict)
    = BaseDict.foldl combine start dict

{-|
-}
foldr : (k -> v -> b -> b) -> b -> Dict comparable k v -> b
foldr combine start (Dict _ dict)
    = BaseDict.foldr combine start dict

{-|
-}
filter : (k -> v -> Bool) -> Dict comparable k v -> Dict comparable k v
filter pred dict
    = mapInternal (BaseDict.filter pred) dict

{-|
-}
partition: 
    (k -> v -> Bool) 
    -> Dict comparable k v 
    -> (Dict comparable k v, Dict comparable k v)
partition pred (Dict f dict)
    = dict 
    |> BaseDict.partition pred
    |> Tuple.mapBoth (Dict f) (Dict f)

{-|
-}
union : Dict comparable1 k v -> Dict comparable2 k v -> Dict comparable2 k v
union dictLeft dictRight
    = foldl insert dictRight dictLeft

{-|
-}
intersect : Dict comparable1 k v -> Dict comparable2 k v -> Dict comparable1 k v
intersect dictLeft dictRight
    = filter (\k _ -> member k dictRight) dictLeft


{-|
-}
diff : Dict comparable1 k v -> Dict comparable2 k v -> Dict comparable1 k v
diff dictLeft dictRight
    = foldl (\k _ d -> remove k d) dictLeft dictRight

{-|
-}
merge : 
    (k -> k -> Order)
    -> (k -> a -> r -> r)
    -> (k -> a -> b -> r -> r)
    -> (k -> b -> r -> r)
    -> Dict comparable1 k a
    -> Dict comparable2 k b
    -> r
    -> r
merge ord leftAccu bothAccu rightAccu (Dict _ leftDict) (Dict _ rightDict) start 
    = BaseDict.merge 
        ord leftAccu bothAccu rightAccu
        leftDict rightDict start

{-|
-}
replaceMapping : (k -> comparable2) -> Dict comparable1 k v -> Dict comparable2 k v
replaceMapping f (Dict _ dict)
    = Dict f (BaseDict.reHash f dict)