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

{-| A wrapping for the `Dict` from `ManualDict`.
The `Dict` in this module stores a hashing-function for later use inside the
Date structure. So that after creating a `Dict` there is no need to always
specify the hash-function.
This makes it more reliable and less error prone to use.

Like the `ManualDict.Dict` can this `Dict` hold keys of any type.

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

# Hashing related
@docs replaceMapping
-}

import ManualDict as BaseDict

{-| A dict of key-value-pairs that also stores a hash-function.
So a `Dict String Id User` is a dictionary that lets you map an `Id` to the
corresponding `User`.

    import AutoDict as Dict exposing (Dict)

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
    = Dict (k -> comparable) (BaseDict.Dict comparable k v)

-- helper

mapInternal : 
    (BaseDict.Dict comparable k a -> BaseDict.Dict comparable k b) 
    -> Dict comparable k a 
    -> Dict comparable k b
mapInternal mapFunc (Dict f dict)
    = Dict f (mapFunc dict)

{-| Creates an empty dictionary that uses the given hash-function for the 
hashing of the keys.

Complexity: *O(1)*
-}
empty : (k -> comparable) -> Dict comparable k v
empty f = Dict f BaseDict.empty

{-| Creates a dictionary with one key-value pair. 
Using and storing the given function as hash-function for the key

Complexity: *O(1)*
-}
singleton : (k -> comparable) -> k -> v -> Dict comparable k v
singleton f key value
    = Dict f (BaseDict.singleton f key value)

{-| Inserts a key-value-pair into the dictionary. 
If there is a collision the old key-value-pair will be replaced with the new
pair.

Complexity: *O(log n)*
-}
insert : k -> v -> Dict comparable k v -> Dict comparable k v
insert key value (Dict f dict)
    = Dict f (BaseDict.insert f key value dict)

{-| Updates the value for given key with the given function. 
The argument for the update function will be `Nothing` if there is no value
corresponding to the key and will be `Just v` where `v` is the value
corresponding to the key.

If the function returns `Just v2` the value will be stored for the given key, so

    insert (Name "Carl") (User "Carl" 5) users

and

    update (Name "Carl") (always <| Just <| User "Carl" 5) users

are equivalent.

If the function return `Nothing` the key-value-pair will be removed, so
    
    remove (Name "Alice") users

and 

    update (Name "Alice") (always Nothing) users

are equivalent.

Complexity: *O(log n)*
-}
update : k -> (Maybe v -> Maybe v) -> Dict comparable k v -> Dict comparable k v
update key updateFunc (Dict f dict)
    = Dict f (BaseDict.update f key updateFunc dict)

{-| Removes a key-value-pair from a dictionary. 
If the key is not found, no changes are made.  

Complexity: *O(log n)*
-}
remove : k -> Dict comparable k v -> Dict comparable k v
remove key (Dict f dict)
    = Dict f (BaseDict.remove f key dict)

{-| Determine if a dictionary is empty.

    isEmpty <| empty someFunction == True

Complexity: *O(log n)*
-}
isEmpty : Dict comparable k v -> Bool
isEmpty (Dict _ dict) = BaseDict.isEmpty dict

{-|Determine if a key is in a dictionary. 

    member (Name "Alice") == True
    member (Name "Carl") == False

Complexity: *O(log n)*
-}
member : k -> Dict comparable k v -> Bool
member key (Dict f dict)
    = BaseDict.member f key dict

{-| Gets the value associated with the key.
If the key is not found, `Nothing` is returned.

    get (Name "Alice") users == Just { name = "Alice", age = 28 }
    get (Name "Carl") users  == Nothing

Complexity: *O(log n)*
-}
get : k -> Dict comparable k v -> Maybe v
get key (Dict f dict)
    = BaseDict.get f key dict

{-| Determine the number of key-value pairs in the dictionary.

Complexity: *O(log n)*
-}
size : Dict comparable k v -> Int
size (Dict _ dict) = BaseDict.size dict

{-| Checks if the two dictionaries contains the same set of keys.
This function ignores the values associated with the keys and only checks if
every key contained in the one dictionary is also a key in the other.

    dict1 = fromList abs [(1,1)]
    dict2 = fromList abs [(-1,-1)]

    eq abs abs dict1 dict2 == True
-}
eq : Dict comparable1 k v -> Dict comparable2 k v -> Bool
eq leftDict rightDict
    = (diff leftDict rightDict |> isEmpty)
    && (diff rightDict leftDict |> isEmpty)

{-| Get all of the keys in a dictionary, sorted from lowest to highest
according to the order of the hash-type.

    key users == [Name "Alice", Name "Bob", Name "Chuck"]

Complexity: *O(n)*
-}
keys : Dict comparable k v -> List k
keys (Dict _ dict) = BaseDict.keys dict

{-| Get all of the values in a dictionary, in the order of their keys
according to the order of the hash-type.

    values users == [{ age = 28, name = "Alice" },{ age = 19, name = "Bob" },{ age = 33, name = "Chuck" }]

Complexity: *O(n)*
-}
values : Dict comparable k v -> List v
values (Dict _ dict) = BaseDict.values dict

{-| Convert a dictionary into an association list of key-value pairs, 
sorted by keys.

Complexity: *O(n)*
-}
toList : Dict comparable k v -> List (k,v)
toList (Dict _ dict) = BaseDict.toList dict

{-| Convert an association list into a dictionary. 

Complexity: *O(n  &ast;  log n)*
-}
fromList : (k -> comparable) -> List (k,v) -> Dict comparable k v
fromList f list 
    = Dict f (BaseDict.fromList f list)

{-| Apply a function to all values in a dictionary.

Complexity: *O(n)*
-}
map : (k -> a -> b) -> Dict comparable k a -> Dict comparable k b
map updateFunc dict
    = mapInternal (BaseDict.map updateFunc) dict

{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.

Complexity: *O(n)*
-}
foldl : (k -> v -> b -> b) -> b -> Dict comparable k v -> b
foldl combine start (Dict _ dict)
    = BaseDict.foldl combine start dict

{-| Fold over the key-value pairs in a dictionary from highest key to lowest key.

Complexity: *O(n)*
-}
foldr : (k -> v -> b -> b) -> b -> Dict comparable k v -> b
foldr combine start (Dict _ dict)
    = BaseDict.foldr combine start dict

{-| Keep only the key-value pairs that pass the given test. 

Complexity: *O(n)*
-}
filter : (k -> v -> Bool) -> Dict comparable k v -> Dict comparable k v
filter pred dict
    = mapInternal (BaseDict.filter pred) dict

{-| Partition a dictionary according to some test. 
The first dictionary contains all key-value pairs which passed the test, 
and the second contains the pairs that did not.

Complexity: *O(n &ast; log n)*
-}
partition: 
    (k -> v -> Bool) 
    -> Dict comparable k v 
    -> (Dict comparable k v, Dict comparable k v)
partition pred (Dict f dict)
    = dict 
    |> BaseDict.partition pred
    |> Tuple.mapBoth (Dict f) (Dict f)

{-| Combine two dictionaries. 
If there is a collision, preference is given to the first dictionary.

Complexity: *O(n &ast; log n)*
-}
union : Dict comparable1 k v -> Dict comparable2 k v -> Dict comparable2 k v
union dictLeft dictRight
    = foldl insert dictRight dictLeft

{-| Keep a key-value pair when its key appears in the second dictionary. 
Preference is given to values in the first dictionary.

Complexity: *O(n &ast; log n)*
-}
intersect : Dict comparable1 k v -> Dict comparable2 k v -> Dict comparable1 k v
intersect dictLeft dictRight
    = filter (\k _ -> member k dictRight) dictLeft


{-| Keep a key-value pair when its key does not appear in the second dictionary.

Complexity: *O(n &ast; log n)*
-}
diff : Dict comparable1 k v -> Dict comparable2 k v -> Dict comparable1 k v
diff dictLeft dictRight
    = foldl (\k _ d -> remove k d) dictLeft dictRight

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
merge ord leftAccu bothAccu rightAccu (Dict _ leftDict) (Dict _ rightDict) start 
    = BaseDict.merge 
        ord leftAccu bothAccu rightAccu
        leftDict rightDict start

{-| Creates a new dictionary which uses the new hash-function to 
rehash all keys and also stores the new hash-function.
If two keys have a collision under the new hashing the key-value-pair with the 
higher value under the old hashing is kept.

    fromList identity [(-1,-1),(1,1)] |> replaceMapping abs |> toList == [(1,1)]
    fromList negate [(-1,-1),(1,1)] |> replaceMapping abs |> toList == [(-1,-1)]

Complexity: *O(n &ast; log n)*
-}
replaceMapping : (k -> comparable2) -> Dict comparable1 k v -> Dict comparable2 k v
replaceMapping f (Dict _ dict)
    = Dict f (BaseDict.reHash f dict)