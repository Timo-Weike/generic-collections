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

{-| A wrapping of `Dict` from the package `elm/core`. The keys can be any type
but the user has to specify a conversion/hash function for every action on the
dictionary that needs to compare keys with each other.
The hash function can map the keys to any comparabel type, that is `Int`,
`Float`, `Time`, `Char`, `Bool` and tuples or list of comparable types.

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
@docs reHash

-}

import Dict as CoreDict
import Maybe

{-| A dictonary of keys and values that uses a comparable type as hash-key.
So a `Dict String Id User` is a dictonary that lets you map an `Id` to the
corresponding `User`.

    import ManualDict as Dict exposing (Dict)

    users : Dict String Id User
    users 
        = Dict.fromList idToString
            [ (Name "Alice", User "Alice" 28)
            , (Name "Bob", User "Bob" 19)
            , (Name "Chuck", User "Chuck" 33)
            ]
    
    type Id = Name String
    idToString (Name s) = s

    type alias User = 
        { name : String
        , age : Int
        }
-}
type Dict comparable k v
    = Dict (CoreDict.Dict comparable (k,v))

{-| Creates an empty dictionary. 

Complexity: *O(1)*
-}
empty : Dict comparable k v
empty = Dict CoreDict.empty

{-| Creates a dictionary with one key-value pair. 
Using the given function as hash-function for the key

Complexity: *O(1)*
-}
singleton : (k -> comparable) -> k -> v -> Dict comparable k v
singleton f key value
    = Dict (CoreDict.singleton (f key) (key, value))

{-| Inserts a key-value-pair into the a dictionary using the given function
as hash-function. If there is a collison with the hash the old key-value-pair
is replaced with the new.

newUsers = 
    Dict.insert 
    (always "Alice") 
    (Name "Carl")
    (User "Carl" 5)
    users

In newUsers will be no key-value-pair for `(Name "Alice", User "Alice" 28)`. 

Because of this behavior it is strongly recomended to always use the same
hash-function.
For making this easier you can use the `Dict` from the modele `AutoDict`.

Complexity: *O(log n)*
-}
insert : 
    (k -> comparable) 
    -> k 
    -> v 
    -> Dict comparable k v
    -> Dict comparable k v
insert f key value (Dict dict)
    = Dict (CoreDict.insert (f key) (key, value) dict)

{-| Updates the value for given key with the given function. 
The argument for the update function will be `Nothing` if there is no value
corresponding to the key and will be `Just v` where `v` is the value
corresponding to the key.

If the function returns `Just v2` the value will be stored for the given key, so

    insert idToString (Name "Carl") (User "Carl" 5) users

and

    update idToString (Name "Carl") (always <| Just <| User "Carl" 5) users

are equivalent.

If the function return `Nothing` the key-value-pair will be removed, so
    
    remove idToString (Name "Alice") users

and 

    update idToString (Name "Alice") (always Nothing) users

are equivalent.

Complexity: *O(log n)*
-}
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

{-| Removes a key-value-pair from a dictionary. 
If the key is not found, no changes are made.  

Complexity: *O(log n)*
-}
remove : (k -> comparable) -> k -> Dict comparable k v -> Dict comparable k v
remove f key (Dict dict) 
    = Dict (CoreDict.remove (f key) dict)

{-| Determine if a dictionary is empty.

    isEmpty empty == True

Complexity: *O(1)*
-}
isEmpty : Dict comparable k v -> Bool
isEmpty (Dict dict) = CoreDict.isEmpty dict

{-| Determine if a key is in a dictionary. 

    member idToString (Name "Alice") == True
    member idToString (Name "Carl") == False
    member (always "") (Name "Alice") == False

Complexity: *O(log n)*
-}
member : (k -> comparable) -> k -> Dict comparable k v -> Bool
member f key (Dict dict)
    = CoreDict.member (f key) dict

{-| Gets the value associated with the key.
If the key is not found, `Nothing` is retured.

    get idToString (Name "Alice") users == Just { name = "Alice", age = 28 }
    get idToString (Name "Carl") users  == Nothing

Complexity: *O(log n)*
-}
get : (k -> comparable) -> k -> Dict comparable k v -> Maybe v
get f key (Dict dict)
    = CoreDict.get (f key) dict
    |> Maybe.map Tuple.second

{-| Determine the number of key-value pairs in the dictionary.

Complexity: *O(log n)*
-}
size : Dict comparable k v -> Int
size (Dict dict) = CoreDict.size dict

{-| Checks if the two dictionarys contains the same set of keys.
This function ignores the values assosiated with the keys and only checks if
every key contained in the one dictonary is also a key in the other.

    dict1 = fromList abs [(1,1)]
    dict2 = fromList abs [(-1,-1)]

    eq abs abs dict1 dict2 == True

Complexity: *O(n &ast; log n)*
-}
eq : 
    (k -> comparable1) 
    -> (k -> comparable2) 
    -> Dict comparable1 k v 
    -> Dict comparable2 k v 
    -> Bool
eq leftHash rightHash leftDict rightDict
    = (diff leftHash leftDict rightDict |> isEmpty)
    && (diff rightHash rightDict leftDict |> isEmpty)

{-| Get all of the keys in a dictionary, sorted from lowest to highest
according to the order of the hash-type.

    key users == [Name "Alice", Name "Bob", Name "Chuck"]

Complexity: *O(n)*
-}
keys : Dict comparable k v -> List k
keys dict
    = toList dict
    |> List.map Tuple.first

{-| Get all of the values in a dictionary, in the order of their keys
according to the order of the hash-type.

    values users == [{ age = 28, name = "Alice" },{ age = 19, name = "Bob" },{ age = 33, name = "Chuck" }]

Complexity: *O(n)*
-}
values : Dict comparable k v -> List v
values dict
    = toList dict
    |> List.map Tuple.second

{-| Convert a dictionary into an association list of key-value pairs, 
sorted by keys.

Complexity: *O(n)*
-}
toList : Dict comparable k v -> List (k,v)
toList (Dict dict)
    = CoreDict.values dict

{-| Convert an association list into a dictionary. 

Complexity: *O(n  &ast;  log n)*
-}
fromList : (k -> comparable) -> List (k,v) -> Dict comparable k v
fromList f list 
    = list 
    |> List.map (\t -> (f (Tuple.first t), t))
    |> CoreDict.fromList
    |> Dict

{-| Apply a function to all values in a dictionary.

Complexity: *O(n)*
-}
map : (k -> a -> b) -> Dict comparable k a -> Dict comparable k b
map updateFunc (Dict dict) =
    let
        updater : comparable -> (k,a) -> (k,b)
        updater _ (key,value) 
            = (key, updateFunc key value)
    in
        Dict (CoreDict.map updater dict)

{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.

Complexity: *O(n)*
-}
foldl : (k -> v -> b -> b) -> b -> Dict comparable k v -> b
foldl f start (Dict dict) =
    let
        combine _ (key,value) prev = f key value prev
    in
        CoreDict.foldl combine start dict

{-| Fold over the key-value pairs in a dictionary from highest key to lowest key.

Complexity: *O(n)*
-}
foldr : (k -> v -> b -> b) -> b -> Dict comparable k v -> b
foldr f start (Dict dict) =
    let
        combine _ (key,value) prev = f key value prev
    in
        CoreDict.foldr combine start dict 
    
{-| Keep only the key-value pairs that pass the given test. 

Complexity: *O(n)*
-}
filter : (k -> v -> Bool) -> Dict comparable k v -> Dict comparable k v
filter pred (Dict dict) =
    let
        predicate _ (key,value) = pred key value
    in
        Dict (CoreDict.filter predicate dict)

{-| Partition a dictionary according to some test. 
The first dictionary contains all key-value pairs which passed the test, 
and the second contains the pairs that did not.

Complexity: *O(n &ast; log n)*
-}
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

{-| Combine two dictionaries. 
If there is a collision, preference is given to the first dictionary.

Complexity: *O(n &ast; log n)*
-}
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

{-| Keep a key-value pair when its key appears in the second dictionary. 
Preference is given to values in the first dictionary.

Note: The hash-function needs to return the comparable type that the second
dictionary uses because the function checks for each key of the first dictionary
if it is a member of the second.

Complexity: *O(n &ast; log n)*
-}
intersect : 
    (k -> comparable2)
    -> Dict comparable1 k v  
    -> Dict comparable2 k v  
    -> Dict comparable1 k v 
intersect f (dict1) (dict2)
    = filter (\k _ -> member f k dict2) dict1

{-| Keep a key-value pair when its key does not appear in the second dictionary.

Complexity: *O(n &ast; log n)*
-}
diff : 
    (k -> comparable1)
    -> Dict comparable1 k v  
    -> Dict comparable2 k v  
    -> Dict comparable1 k v 
diff f dict1 dict2
    = foldl (\k _ d -> remove f k d) dict1 dict2

{-| The most general way of combining two dictionaries. 
You provide three accumulators for when a given key appears:

1. Only in the left dictionary.
2. In both dictionaries.
3. Only in the right dictionary.

You then traverse all the keys from lowest to highest, 
building up whatever you want.

This function is essentially a foldl on two dictionaries at once.

Complexity: *O(n)*
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

{-| Creates a new dictionary where all the keys are hashed using the given
hash-function.
If two keys have a collision under the new hashing the key-value-pair with the 
higher value under the old hashing is keept.

    (fromList identity [(-1,-1),(1,1)] |> reHash abs |> toList) == [(1,1)]
    (fromList negate [(-1,-1),(1,1)] |> reHash abs |> toList) == [(-1,-1)]

Complexity: *O(n &ast; log n)*
-}
reHash : (k -> comparable2) -> Dict comparable1 k v -> Dict comparable2 k v
reHash f dict
    = foldl (\k v d -> insert f k v d) empty dict