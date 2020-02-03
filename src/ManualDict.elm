module ManualDict exposing 
    (
        Dict
        , empty, singleton, insert, update, remove
        , isEmpty, member, get, size, eq
        , keys, values, toList, fromList
        , map, foldl, foldr, filter, partition
        , union, intersect, diff, merge
        , reHash
    )

import Dict as CoreDict
import Maybe

type Dict comparable k v
    = Dict (CoreDict.Dict comparable (k,v))

empty : Dict comparable k v
empty = Dict CoreDict.empty

singleton : (k -> comparable) -> k -> v -> Dict comparable k v
singleton f key value
    = Dict (CoreDict.singleton (f key) (key, value))

insert : 
    (k -> comparable) 
    -> k 
    -> v 
    -> Dict comparable k v
    -> Dict comparable k v
insert f key value (Dict dict)
    = Dict (CoreDict.insert (f key) (key, value) dict)

update : 
    (k -> comparable) 
    -> k 
    -> (Maybe v -> Maybe v)
    -> Dict comparable k v 
    -> Dict comparable k v
update f key updateFunc (Dict dict) =
    let
        updater tuple 
            = tuple
            |> Maybe.map Tuple.second
            |> updateFunc
            |> Maybe.map (\value -> (key, value))
    in
        Dict (CoreDict.update (f key) updater dict) 

remove : (k -> comparable) -> k -> Dict comparable k v -> Dict comparable k v
remove f key (Dict dict) 
    = Dict (CoreDict.remove (f key) dict)

isEmpty : Dict comparable k v -> Bool
isEmpty (Dict dict) = CoreDict.isEmpty dict

member : (k -> comparable) -> k -> Dict comparable k v -> Bool
member f key (Dict dict)
    = CoreDict.member (f key) dict

get : (k -> comparable) -> k -> Dict comparable k v -> Maybe v
get f key (Dict dict)
    = CoreDict.get (f key) dict
    |> Maybe.map Tuple.second

size : Dict comparable k v -> Int
size (Dict dict) = CoreDict.size dict

eq : 
    (k -> comparable1) 
    -> (k -> comparable2) 
    -> Dict comparable1 k v 
    -> Dict comparable2 k v 
    -> Bool
eq leftHash rightHash leftDict rightDict
    = (diff leftHash leftDict rightDict |> isEmpty)
    && (diff rightHash rightDict leftDict |> isEmpty)

keys : Dict comparable k v -> List k
keys dict
    = toList dict
    |> List.map Tuple.first

values : Dict comparable k v -> List v
values dict
    = toList dict
    |> List.map Tuple.second

toList : Dict comparable k v -> List (k,v)
toList (Dict dict)
    = CoreDict.values dict

fromList : (k -> comparable) -> List (k,v) -> Dict comparable k v
fromList f list 
    = list 
    |> List.map (\t -> (f (Tuple.first t), t))
    |> CoreDict.fromList
    |> Dict

map : (k -> a -> b) -> Dict comparable k a -> Dict comparable k b
map updateFunc (Dict dict) =
    let
        updater : comparable -> (k,a) -> (k,b)
        updater _ (key,value) 
            = (key, updateFunc key value)
    in
        Dict (CoreDict.map updater dict)

foldl : (k -> v -> b -> b) -> b -> Dict comparable k v -> b
foldl f start (Dict dict) =
    let
        combine _ (key,value) prev = f key value prev
    in
        CoreDict.foldl combine start dict

foldr : (k -> v -> b -> b) -> b -> Dict comparable k v -> b
foldr f start (Dict dict) =
    let
        combine _ (key,value) prev = f key value prev
    in
        CoreDict.foldr combine start dict 
    
filter : (k -> v -> Bool) -> Dict comparable k v -> Dict comparable k v
filter pred (Dict dict) =
    let
        predicate _ (key,value) = pred key value
    in
        Dict (CoreDict.filter predicate dict)

partition : 
    (k -> v -> Bool) 
    -> Dict comparable k v 
    -> (Dict comparable k v, Dict comparable k v)
partition pred (Dict dict) =
    let
        predicate _ (key,value) = pred key value
    in
        CoreDict.partition predicate dict
        |> Tuple.mapBoth Dict Dict

union : 
    (k -> comparable3) 
    -> Dict comparable1 k v  
    -> Dict comparable2 k v  
    -> Dict comparable3 k v 
union f (dict1) (dict2) =
    let 
        newDict = toList dict2 |> fromList f
    in
        foldl (insert f) newDict dict1

intersect : 
    (k -> comparable2)
    -> Dict comparable1 k v  
    -> Dict comparable2 k v  
    -> Dict comparable1 k v 
intersect f (dict1) (dict2)
    = filter (\k _ -> member f k dict2) dict1

diff : 
    (k -> comparable1)
    -> Dict comparable1 k v  
    -> Dict comparable2 k v  
    -> Dict comparable1 k v 
diff f dict1 dict2
    = foldl (\k _ d -> remove f k d) dict1 dict2

merge : 
    (k -> k -> Order)
    -> (k -> a -> r -> r)
    -> (k -> a -> b -> r -> r)
    -> (k -> b -> r -> r)
    -> Dict comparable1 k a
    -> Dict comparable2 k b
    -> r
    -> r
merge ord leftAccu bothAccu rightAccu leftDict rightDict start =
    let
        step rKey rValue (list, result) =
            case list of
                [] ->
                    (list, rightAccu rKey rValue result)
                (lKey, lValue) :: rest ->
                    if ord lKey rKey == LT then
                        step rKey rValue (rest, leftAccu lKey lValue result)
                    else if ord lKey rKey == GT then
                        (list, rightAccu rKey rValue result)
                    else
                        (rest, bothAccu lKey lValue rValue result)
        
        (leftovers, intermediateResult) 
            = foldl step (toList leftDict, start) rightDict

    in
        List.foldl (\(k,v) result -> leftAccu k v result) intermediateResult leftovers

reHash : (k -> comparable2) -> Dict comparable1 k v -> Dict comparable2 k v
reHash f dict
    = foldl (\k v d -> insert f k v d) empty dict