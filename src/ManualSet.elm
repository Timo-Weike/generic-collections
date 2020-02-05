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

{-| A version of `Set` from `elm/core` that is implemented with a `Dict`
from `elm/core` as storage.
The values can be any type but the user needs to specify a 
conversion/hash-function for every action that needs to compare values with
each other.
The hash function can map the values to any comparable type, that is `Int`,
`Float`, `Time`, `Char`, `Bool` and tuples or list of comparable types.

# Sets
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
@docs reHash
-}

import Dict exposing (Dict)

{-| A set of values. The values can be of any type.

    import ManualSet as Set exposing (Set)

    pets : Set String Pet
    pets 
        = Set.fromList petToString
            [ Dog "Max"
            , Cat "Jerry"
            , Bird "Ace"
            ]

    type Pet 
        = Dog String
        | Cat String
        | Bird String

    petToString : Pet -> String
    petToString pet = case pet of
        Dog name -> "Dog: " ++ name
        Cat name -> "Cat: " ++ name
        Bird name -> "Bird: " ++ name
-}
type Set comparable v 
    = Set (Dict comparable v)

{-| Creates an empty set.

Complexity: *O(1)*
-}
empty : Set comparable v
empty 
    = Set Dict.empty

{-| Creates an set, containing only one value.

Complexity: *O(1)*
-}
singleton : (v -> comparable) -> v -> Set comparable v
singleton f v 
    = Set (Dict.singleton (f v) v)

{-| Inserts a value into the set. 
A value might be replaced if it's hash
collides with the hash of some other value.

Complexity: *O(log n)*
-}
insert : (v -> comparable) -> v -> Set comparable v -> Set comparable v
insert f v (Set dict)
    = Set (Dict.insert (f v) v dict)

{-| Removes a value from the set. 

Complexity: *O(log n)*
-}
remove : (v -> comparable) -> v -> Set comparable v -> Set comparable v
remove f v (Set dict)
    = Set (Dict.remove (f v)  dict)

{-| Determine if a set is empty.

    isEmpty empty == True

Complexity: *O(1)*
-}
isEmpty : Set comparable v -> Bool
isEmpty (Set dict)
    = Dict.isEmpty dict

{-| Determine if a value is in a set.

    member petToString (Cat "Jerry") pets == True
    member petToString (Bird "Jerry") pets == False

Complexity: *O(log n)*
-}
member : (v -> comparable) -> v -> Set comparable v -> Bool
member f v (Set dict) 
    = Dict.member (f v) dict

{-| Determine the number of elements in a set.

Complexity: *O(n)*
-}
size : Set comparable v -> Int
size (Set dict) 
    = Dict.size dict

{-| Checks if the two sets contain the same elements.

    set1 = fromList abs [1]
    set2 = fromList abs [-1]

    eq identity identity set1 set2 == False
    eq abs abs set1 set2 == True

Complexity: *O(n &ast; log n)*
-}
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

Complexity: *O(n &ast; log n)*
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

Complexity: *O(n &ast; log n)*
-}
intersect : 
    (v -> comparable2) 
    -> Set comparable1 v 
    -> Set comparable2 v 
    -> Set comparable1 v
intersect f set1 set2 
    = filter (\v -> member f v set2) set1

{-| Keeps all values from the first set which don't appear in the second set.

Complexity: *O(n &ast; log n)*
-}
diff : 
    (v -> comparable1) 
    -> Set comparable1 v 
    -> Set comparable2 v 
    -> Set comparable1 v
diff f set1 set2
    = foldl (\v s -> remove f v s) set1 set2

{-| Converts the set into a list of its values.

Complexity: *O(n)*
-}
toList : Set comparable v -> List v
toList (Set dict)
    = Dict.values dict

{-| Converts a list of values into a set.

Complexity: *O(n &ast; log n)*
-}
fromList : (v -> comparable) -> List v -> Set comparable v
fromList f list =
    let
        mappedList = List.map (\v -> (f v, v)) list
    in
        Set (Dict.fromList mappedList)

{-| Applies a function to all values in a set.

Complexity: *O(n)*
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

Complexity: *O(n)*
-}
foldl : (v -> a -> a) -> a -> Set comparable v -> a
foldl f start (Set dict) 
    = Dict.foldl (\_ -> f) start dict

{-| Folds over the values in a set from  highest to lowest (depending on the
string representation).

Complexity: *O(n)*
-}
foldr : (v -> a -> a) -> a -> Set comparable v -> a
foldr f start (Set dict)
    = Dict.foldr (\_ -> f) start dict

{-| Keeps all the value for which the function yields True.

Complexity: *O(n)*
-}
filter : (v -> Bool) -> Set comparable v -> Set comparable v
filter isGood (Set dict)
    = Set (Dict.filter (\_ -> isGood) dict)

{-| Partitions a set into two subsets, according to some function.
The first set will contain the values for which the function yields True
and the second set will contain the values for which the function yields False.

Complexity: *O(n &ast; log n)*
-}
partition : 
    (v -> Bool) 
    -> Set comparable v 
    -> (Set comparable v, Set comparable v)
partition f (Set dict) 
    = Dict.partition (\_ -> f) dict 
    |> Tuple.mapBoth Set Set

{-| Creates a new set where all values are rehashed with the given function.
If two keys have a collision under the new hashing the key-value-pair with the 
higher value under the old hashing is kept.

    (fromList identity [-1,1] |> reHash abs |> toList) == [1]
    (fromList negate [-1,1] |> reHash abs |> toList) == [-1]
-}
reHash : (v -> comparable2) -> Set comparable1 v -> Set comparable2 v
reHash f set
    = foldl (\v s -> insert f v s) empty set