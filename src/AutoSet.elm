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

{-| A wrapping for `ManualSet.Set` which stores a hash-function for later use.
So after creating a `Set` there is not need to always specify the hash-function.
This makes it more reliable and less error prone to use.

Like `ManualSet.Set` can this `Set` hold value of any type.

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
@docs replaceMapping
-}

import ManualSet as BaseSet

{-| A set of values. The values can be of any type.

    import AutoSet as Set exposing (Set)

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
    = Set (v -> comparable) (BaseSet.Set comparable v)

{-| Creates an empty set that uses the given function for hashing.

Complexity: *O(1)*
-}
empty : (v -> comparable) -> Set comparable v
empty f = Set f BaseSet.empty

{-| Creates an set containing only one value and the hash-function.

Complexity: *O(1)*
-}
singleton : (v -> comparable) -> v -> Set comparable v
singleton f value = Set f (BaseSet.singleton f value)

{-| Inserts a value into the set. 
A value might be replaced if it's hash
collides with the hash of some other value.

Complexity: *O(log n)*
-}
insert : v -> Set comparable v -> Set comparable v
insert value (Set f set) = Set f <| BaseSet.insert f value set

{-| Removes a value from the set. 

Complexity: *O(log n)*
-}
remove : v -> Set comparable v -> Set comparable v
remove value (Set f set) = Set f <| BaseSet.remove f value set

{-| Determine if a set is empty.

    isEmpty (empty someFunction) == True

Complexity: *O(1)*
-}
isEmpty : Set comparable v -> Bool
isEmpty (Set _ set) = BaseSet.isEmpty set

{-| Determine if a value is in a set.

    member (Cat "Jerry") pets == True
    member (Bird "Jerry") pets == False

Complexity: *O(log n)*
-}
member : v -> Set comparable v -> Bool
member value (Set f set) = BaseSet.member f value set

{-| Determine the number of elements in a set.

    size pets == 3
-}
size : Set comparable v -> Int
size (Set _ set) = BaseSet.size set

{-| Checks if the two sets contain the same elements.

Complexity: *O(n &ast; log n)*
-}
eq : Set comparable1 v -> Set comparable2 v -> Bool
eq leftSet rightSet
    = (diff leftSet rightSet |> isEmpty)
    && (diff rightSet leftSet |> isEmpty)

{-| Converts the set into a list of its values.

Complexity: *O(n)*
-}
toList : Set comparable v -> List v
toList (Set _ set) = BaseSet.toList set

{-| Converts a list of values into a set.

Complexity: *O(n &ast; log n)*
-}
fromList : (v -> comparable) -> List v -> Set comparable v
fromList f list = Set f <| BaseSet.fromList f list

{-| Applies a function to all values in a set.

Complexity: *O(n)*
-}
map : 
    (v2 -> comparable) 
    -> (v1 -> v2) 
    -> Set comparable v1 
    -> Set comparable v2
map f updateFunc set
    = foldl (\v s -> insert (updateFunc v) s) (empty f) set

{-| Folds over the values in a set from lowest to highest (depending on the
string representation).

Complexity: *O(n)*
-}
foldl : (v -> a -> a) -> a -> Set comparable v -> a
foldl combine start (Set _ set)
    = BaseSet.foldl combine start set

{-| Folds over the values in a set from  highest to lowest (depending on the
string representation).

Complexity: *O(n)*
-}
foldr : (v -> a -> a) -> a -> Set comparable v -> a
foldr combine start (Set _ set)
    = BaseSet.foldr combine start set

{-| Keeps all the value for which the function yields True.

Complexity: *O(n)*
-}
filter : (v -> Bool) -> Set comparable v -> Set comparable v
filter pred (Set f set)
    = Set f <| BaseSet.filter pred set

{-| Partitions a set into two subsets, according to some function.
The first set will contain the values for which the function yields True
and the second set will contain the values for which the function yields False.

Complexity: *O(n &ast; log n)*
-}
partition : 
    (v -> Bool) 
    -> Set comparable v 
    -> (Set comparable v, Set comparable v)
partition pred (Set f set)
    = BaseSet.partition pred set
    |> Tuple.mapBoth (Set f) (Set f)

{-| Creates the union of two sets.
If two values have the same comparable representation then the value from the first
set is in the resulting set.

Complexity: *O(n &ast; log n)*
-}
union : 
    Set comparable1 v 
    -> Set comparable2 v 
    -> Set comparable2 v
union leftSet rightSet
    = foldl (\v s -> insert v s) rightSet leftSet

{-| Keeps all values which comparable representation is contained in both sets.
Preference is given to values in the first set.

Complexity: *O(n &ast; log n)*
-}
intersect : Set comparable1 v -> Set comparable2 v -> Set comparable1 v
intersect leftSet rightSet
    = filter (\v -> member v rightSet) leftSet

{-| Keeps all values from the first set which don't appear in the second set.

Complexity: *O(n &ast; log n)*
-}
diff : Set comparable1 v -> Set comparable2 v -> Set comparable1 v
diff leftSet rightSet
    = foldl (\v s -> remove v s) leftSet rightSet

{-| Creates a new set where all value are rehashed using the new hash-function.
If two keys have a collision under the new hashing the key-value-pair with the 
higher value under the old hashing is kept.

    (fromList identity [-1,1] |> replaceMapping abs |> toList) == [1]
    (fromList negate [-1,1] |> replaceMapping abs |> toList) == [-1]
-}
replaceMapping : (v -> comparable2) -> Set comparable1 v -> Set comparable2 v
replaceMapping f set
    = foldl (\v s -> insert v s) (empty f) set