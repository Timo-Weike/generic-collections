module ManualSet exposing 
    ( 
        Set
        , empty, singleton, insert, remove
        , isEmpty, member, size, eq
        , toList, fromList
        , map, foldl, foldr, filter, partition
        , union, intersect, diff
        , reHash
    )

{-| This module provides an implementation for a Set which can contain values
of any type. This implementation is a wrapper on the `Dict` from `elm/Dict`, for this the values
contained in this Set are stored as key-value-pairs where the key is a string
representation of the value and the value is just the value.

@docs Set
# Build
@docs empty, singleton, insert, update, remove

# Query
@docs isEmpty, member, get, size

# Lists
@docs oList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

# Combine
@docs union, intersect, diff, merge

-}

import Dict exposing (Dict)

{-|
    A set of values. The values can be of any type.
-}
type Set comparable v 
    = Set (Dict comparable v)

{-|
    Creates an empty set.
-}
empty : Set comparable v
empty 
    = Set Dict.empty

{-| Creates an set, containing only one value.^
-}
singleton : (v -> comparable) -> v -> Set comparable v
singleton f v 
    = Set (Dict.singleton (f v) v)
{-| Inserts a value into the set. 
A value might be replaced if it's String representation
collides with some other value.
-}
insert : (v -> comparable) -> v -> Set comparable v -> Set comparable v
insert f v (Set dict)
    = Set (Dict.insert (f v) v dict)

{-| Removes a value from the set.
If the mapping to Strings is not injective this function
might removes the wrong value (so if x != y => f(x) != f(x)).
-}
remove : (v -> comparable) -> v -> Set comparable v -> Set comparable v
remove f v (Set dict)
    = Set (Dict.remove (f v)  dict)

{-| Determine if a set is empty.
-}
isEmpty : Set comparable v -> Bool
isEmpty (Set dict)
    = Dict.isEmpty dict

{-| Determine if a value is in a set.
-}
member : (v -> comparable) -> v -> Set comparable v -> Bool
member f v (Set dict) 
    = Dict.member (f v) dict
{-| Determine the number of elements in a set.
-}
size : Set comparable v -> Int
size (Set dict) 
    = Dict.size dict

eq : (v -> comparable1) 
    -> (v -> comparable2) 
    -> Set comparable1 v 
    -> Set comparable2 v 
    -> Bool
eq leftHash rightHash leftSet rightSet
    = (diff leftHash leftSet rightSet |> isEmpty)
    && (diff rightHash rightSet leftSet |> isEmpty)

{-| Creates the union of two sets.
If two values have the same comparable representation then the value from the first
set is in the resulting set.
-}
union : 
    (v -> comparable3) 
    -> Set comparable1 v 
    -> Set comparable2 v 
    -> Set comparable3 v
union f set1 set2 =
    let
        newSet = fromList f (toList set2)
    in
        foldl (insert f) newSet set1

{-| Keeps all values which comparable representation is contained in both sets.
Preference is given to values in the first set.
-}
intersect : 
    (v -> comparable2) 
    -> Set comparable1 v 
    -> Set comparable2 v 
    -> Set comparable1 v
intersect f set1 set2 
    = filter (\v -> member f v set2) set1

{-| Keeps all values from the first set which don't appear in the second set.
-}
diff : 
    (v -> comparable1) 
    -> Set comparable1 v 
    -> Set comparable2 v 
    -> Set comparable1 v
diff f set1 set2
    = foldl (\v s -> remove f v s) set1 set2

{-| Converts the set into a list of its values.
-}
toList : Set comparable v -> List v
toList (Set dict)
    = Dict.values dict

{-| Converts a list of values into a set.
-}
fromList : (v -> comparable) -> List v -> Set comparable v
fromList f list =
    let
        mappedList = List.map (\v -> (f v, v)) list
    in
        Set (Dict.fromList mappedList)

{-| Applies a function to all values in a set.
-}
map : 
    (v2 -> comparable) 
    -> (v1 -> v2) 
    -> Set comparable v1 
    -> Set comparable v2
map f updateFunc set 
    = foldl (\v s -> insert f (updateFunc v) s) empty set

{-| Folds over the values in a set from lowest to highest (depending on the
string representation).
-}
foldl : (v -> a -> a) -> a -> Set comparable v -> a
foldl f start (Set dict) 
    = Dict.foldl (\_ -> f) start dict

{-| Folds over the values in a set from  highest to lowest (depending on the
string representation).
-}
foldr : (v -> a -> a) -> a -> Set comparable v -> a
foldr f start (Set dict)
    = Dict.foldr (\_ -> f) start dict

{-| Keeps all the value for which the function yields True.
-}
filter : (v -> Bool) -> Set comparable v -> Set comparable v
filter isGood (Set dict)
    = Set (Dict.filter (\_ -> isGood) dict)

{-| Partitions a set into two subsets, according to some function.
The first set will contain the values for which the function yields True
and the second set will contain the values for which the function yields False.
-}
partition : 
    (v -> Bool) 
    -> Set comparable v 
    -> (Set comparable v, Set comparable v)
partition f (Set dict) 
    = Dict.partition (\_ -> f) dict 
    |> Tuple.mapBoth Set Set

reHash : (v -> comparable2) -> Set comparable1 v -> Set comparable2 v
reHash f set
    = foldl (\v s -> insert f v s) empty set