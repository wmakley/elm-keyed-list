module KeyedList exposing
    ( KeyedList
    , empty, fromList, fromListBy
    , toList, keys, values, mapToList, unorderedMap, toUnorderedList, mapAndUnzip
    , update, set, get, insert, updateWithCommand
    , append, remove, prepend
    , mapToHtml, mapToTableBody
    , sortBy, sortByKey
    , length, isEmpty, map
    , filter, itemToCommand, reverse
    )

{-| Implements some Dict functions on top of a list.
Useful for building UI's of collections
of elements that can be updated individually and
added, removed, or reordered. This is implemented
using key's, NOT the position of each element, so
messages always go to the right place even if the list
is reordered.

Unfortunately there is no one Elm data structure that
can handle this efficiently, but at least List is
okay at iteration and mapping, and plays well with Html.


## List Operations

Many have been provided, but toList should be quite fast, so
just use that if you don't need to modify the KeyedList!


## Alternative Implementations Considered

**Array:** Great at updating individual elements,
inefficient to map to HTML. Also, uses position-based
keys.

**Dict:** Would be perfect, but it doesn't preserve order.

Combining these wouldn't make sense, because you
just get the worst of both worlds!


# Types

@docs KeyedList


# Initialization

@docs empty, fromList, fromListBy


# Conversions

@docs toList, keys, values, mapToList, unorderedMap, toUnorderedList, mapAndUnzip


# Updating elements by key

@docs update, set, get, insert, updateWithCommand


# Adding and removing elements

@docs append, remove, prepend


# Building UIs

@docs mapToHtml, mapToTableBody


# Sorting

@docs sortBy, sortByKey


# Other List/Dict functions

@docs length, isEmpty, map

-}

import Dict exposing (Dict)
import Html
import Html.Keyed
import Json.Decode as Json


type alias KeyedList comparable a =
    { items : Dict comparable a
    , order : List comparable
    }


{-| Create an empty KeyedList
-}
empty : KeyedList comparable a
empty =
    { items = Dict.empty
    , order = []
    }


{-| Create a KeyedList from a list of (keys, element) tuples.
-}
fromList : List ( comparable, a ) -> KeyedList comparable a
fromList itemsWithKeys =
    { items = Dict.fromList itemsWithKeys
    , order = List.map Tuple.first itemsWithKeys
    }


fromListBy : (a -> comparable) -> List a -> KeyedList comparable a
fromListBy getKey list =
    let
        itemsWithKeys =
            List.map
                (\elt ->
                    ( getKey elt, elt )
                )
                list

        order =
            List.map Tuple.first itemsWithKeys
    in
    { items = Dict.fromList itemsWithKeys
    , order = order
    }


{-| Get the length of the List.
-}
length : KeyedList comparable a -> Int
length { items } =
    Dict.size items


{-| Check if the list is empty.
-}
isEmpty : KeyedList comparable a -> Bool
isEmpty { order } =
    List.isEmpty order


{-| Add an item to the end of the list.
-}
append : comparable -> a -> KeyedList comparable a -> KeyedList comparable a
append key item ({ items, order } as keyedList) =
    { keyedList
        | items = Dict.insert key item items
        , order = List.append order [ key ]
    }


{-| Add an item to the front of the list.
-}
prepend : comparable -> a -> KeyedList comparable a -> KeyedList comparable a
prepend key item ({ items, order } as keyedList) =
    { keyedList
        | items = Dict.insert key item items
        , order = key :: order
    }


{-| Get the list of items with their keys.
-}
toList : KeyedList comparable a -> List ( comparable, a )
toList { items, order } =
    order
        |> List.filterMap
            (\key ->
                items
                    |> Dict.get key
                    |> Maybe.map (\item -> ( key, item ))
            )


{-| Maybe faster than toList + List.map?

Avoids second List.foldl, but doubles the Maybe checks.

-}
mapToList : (comparable -> a -> b) -> KeyedList comparable a -> List b
mapToList func { items, order } =
    order
        |> List.filterMap
            (\key ->
                items
                    |> Dict.get key
                    |> Maybe.map (func key)
            )


{-| Forget about the custom order and do a Dict.map
-}
unorderedMap : (comparable -> a -> b) -> KeyedList comparable a -> KeyedList comparable b
unorderedMap func { items, order } =
    { items = Dict.map func items
    , order = order
    }


{-| Map over an AutoIncList, splitting off a seconary list of each second tuple element
and its key. Unordered operation. The second list is in key order.

Probably extremely inefficient.

-}
mapAndUnzip :
    (comparable -> a -> ( b, c ))
    -> KeyedList comparable a
    -> ( KeyedList comparable b, List c )
mapAndUnzip toTuple keyedList =
    let
        originalOrder =
            keyedList.order

        ( listA, listB ) =
            keyedList.items
                |> Dict.toList
                |> List.map
                    (\( key, value ) ->
                        let
                            ( valueA, valueB ) =
                                toTuple key value
                        in
                        ( ( key, valueA ), valueB )
                    )
                |> List.unzip
    in
    ( { items = Dict.fromList listA
      , order = originalOrder
      }
    , listB
    )


{-| Forget about the custom order and do a Dict.toList
-}
toUnorderedList : KeyedList comparable a -> List ( comparable, a )
toUnorderedList { items } =
    Dict.toList items


keys : KeyedList comparable a -> List comparable
keys keydList =
    keydList.order


{-| Get the list of items without their keys.
-}
values : KeyedList comparable a -> List a
values { items, order } =
    order
        |> List.filterMap (\key -> Dict.get key items)


{-| Shortcut for common use case. Uses Html.Keyed.
-}
mapToHtml :
    (comparable -> String)
    -> (comparable -> a -> Html.Html msg)
    -> String
    -> List (Html.Attribute msg)
    -> KeyedList comparable a
    -> Html.Html msg
mapToHtml keyToString toHtml tagName attributes keyedList =
    let
        nodes =
            keyedList
                |> mapToList
                    (\key item ->
                        ( keyToString key, toHtml key item )
                    )
    in
    Html.Keyed.node tagName attributes nodes


{-| toHtml should return an entire table row (tr) tag
-}
mapToTableBody :
    (comparable -> String)
    -> (comparable -> a -> Html.Html msg)
    -> List (Html.Attribute msg)
    -> KeyedList comparable a
    -> Html.Html msg
mapToTableBody keyToString toHtml attributes list =
    mapToHtml keyToString toHtml "tbody" attributes list


{-| Same as Dict.map
-}
map : (comparable -> a -> b) -> KeyedList comparable a -> KeyedList comparable b
map func ({ items, order } as keyedList) =
    { items = Dict.map func items
    , order = order
    }


{-| Update an invidual element by its key.
-}
update : comparable -> (Maybe a -> Maybe a) -> KeyedList comparable a -> KeyedList comparable a
update key func ({ items } as keyedList) =
    { keyedList
        | items =
            Dict.update
                key
                func
                items
    }


{-| Add or replace an element at a given key and position.
-}
insert : comparable -> a -> KeyedList comparable a -> KeyedList comparable a
insert key item ({ items, order } as keyedList) =
    case Dict.get key items of
        Just _ ->
            -- Just update it in-place
            { keyedList
                | items = Dict.insert key item items
            }

        Nothing ->
            -- We have to both insert it into the Dict and append it to the order
            { keyedList
                | items = Dict.insert key item items
                , order = List.append order [ key ]
            }


{-| Update an item and return a Cmd.
-}
updateWithCommand : comparable -> (a -> ( a, Cmd msg )) -> KeyedList comparable a -> ( KeyedList comparable a, Cmd msg )
updateWithCommand key func ({ items } as keyedList) =
    case Dict.get key items of
        Just item ->
            let
                ( updatedItem, cmd ) =
                    func item
            in
            ( { keyedList | items = Dict.insert key updatedItem items }
            , cmd
            )

        Nothing ->
            ( keyedList, Cmd.none )


itemToCommand : comparable -> (a -> Cmd msg) -> KeyedList comparable a -> Cmd msg
itemToCommand key func { items } =
    items
        |> Dict.get key
        |> Maybe.map func
        |> Maybe.withDefault Cmd.none


{-| Replace an element. Does nothing if not found.
-}
set : comparable -> a -> KeyedList comparable a -> KeyedList comparable a
set key newValue keyedList =
    update key (Maybe.map (\_ -> newValue)) keyedList


{-| Get an element in the list by its key.
-}
get : comparable -> KeyedList comparable a -> Maybe a
get key { items } =
    Dict.get key items


{-| Remove an element from the list by its key. Does nothing
if no element has the key.
-}
remove : comparable -> KeyedList comparable a -> KeyedList comparable a
remove key ({ items, order } as keyedList) =
    { keyedList
        | items = Dict.remove key items
        , order = List.filter (\elt -> elt /= key) order
    }


filter : (comparable -> a -> Bool) -> KeyedList comparable a -> KeyedList comparable a
filter func ({ items, order } as keyedList) =
    let
        filteredItems =
            Dict.filter func items
    in
    { keyedList
        | items = filteredItems
        , order = List.filter (\key -> Dict.member key filteredItems) order
    }


{-| Re-implementation of List.sortBy
-}
sortBy : (comparable -> a -> comparable1) -> KeyedList comparable a -> KeyedList comparable a
sortBy func ({ items, order } as keyedList) =
    { keyedList
        | order =
            keyedList
                |> toList
                |> List.sortBy (\( key, item ) -> func key item)
                |> List.map Tuple.first
    }


reverse : KeyedList comparable a -> KeyedList comparable a
reverse ({ order } as keyedList) =
    { keyedList | order = List.reverse order }


{-| Convenience function to sort by key.
-}
sortByKey : KeyedList comparable a -> KeyedList comparable a
sortByKey ({ items } as keyedList) =
    { keyedList | order = Dict.keys items }
