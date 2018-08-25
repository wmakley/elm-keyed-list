module KeyedList.AutoIncList exposing
    ( AutoIncList, UID
    , empty, fromList,
    , toList, values, mapToList
    , update, set, get, updateWithCommand
    , appendItem, remove, prependItem
    , mapToHTML, mapToTableBody
    , sortBy, sortByUID
    , length, isEmpty, map
    , uidToString, uidDecoder
    )

{-| Implements some Dict functions on top of a list.
Useful for building UI's of collections
of elements that can be updated individually and
added, removed, or reordered. This is implemented
using UID's, NOT the position of each element, so
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

@docs AutoIncList, UID


# Initialization

@docs empty, fromList


# Conversions

@docs toList, values, mapToList


# Updating elements by UID

@docs update, set, get, updateWithCommand


# Adding and removing items

@docs appendItem, remove, prependItem


# Building UIs

@docs mapToHTML, mapToTableBody


# Sorting

@docs sortBy, sortByUID


# Other List functions

@docs length, isEmpty, map


# UIDs

@docs uidToString, uidDecoder

-}

import KeyedList exposing KeyedList
import Dict exposing (Dict)
import Html
import Html.Keyed
import Json.Decode as Json


{-| Represents an AutoIncList. Stores the next UID,
and the internal List of ( UID, element ) tuples.
-}
type alias AutoIncList a =
    { nextUID : UID
    , keyedList : KeyedList UID a
    }


{-| Treat as black box, may change in future.
-}
type alias UID =
    Int


{-| Convert a UID to a String
-}
uidToString : UID -> String
uidToString uid =
    String.fromInt uid


{-| Decode a UID. May most often be used with ports.
-}
uidDecoder : Json.Decoder UID
uidDecoder =
    Json.oneOf
        [ Json.int
        , Json.string
            |> Json.andThen
                (\str ->
                    case String.toInt str of
                        Just uid ->
                            Json.succeed uid

                        Nothing ->
                            Json.fail ("invalid int string: " ++ str)
                )
        ]


{-| Create an empty AutoIncList
-}
empty : AutoIncList a
empty =
    { nextUID = 1
    , items = Dict.empty
    , order = []
    }


{-| Create a AutoIncList from a list of elements, generating a UID for each.
-}
fromList : List a -> AutoIncList a
fromList list =
    let
        itemsWithKeys =
            List.indexedMap
                (\index elt ->
                    ( index + 1, elt )
                )
                list
    in
    { nextUID = List.length list + 1
    , items = Dict.fromList itemsWithKeys
    , order = List.map Tuple.first itemsWithKeys
    }


{-| Get the length of the List.
-}
length : AutoIncList a -> Int
length { items } =
    Dict.size items


{-| Check if the list is empty.
-}
isEmpty : AutoIncList a -> Bool
isEmpty { items } =
    Dict.isEmpty items


{-| Get a UID for an item, and return the list with incremented nextUID.
-}
genUID : AutoIncList a -> ( UID, AutoIncList a )
genUID ({ nextUID } as list) =
    ( list.nextUID, { list | nextUID = nextUID + 1 } )


{-| Recalculate the next UID if you don't have it any more.
-}
calculateNextUID : AutoIncList a -> UID
calculateNextUID { order } =
    List.foldl
        (\uid nextUID ->
            if uid >= nextUID then
                uid + 1

            else
                nextUID
        )
        1
        order


{-| Add a single item to the end of the list. Deliberately
not named the same as List.append, because it works differently.
-}
appendItem : a -> AutoIncList a -> ( AutoIncList a, UID )
appendItem item ({ items, order } as autoIncList) =
    let
        ( uid, autoIncList_ ) =
            genUID autoIncList
    in
    ( { autoIncList_
        | items = Dict.insert uid item items
        , order = List.append order [ uid ]
      }
    , uid
    )


{-| Use this if you can, e.g. if the item goes at the top anyway,
or you are going to sort the AutoIncList after adding it.
-}
prependItem : a -> AutoIncList a -> ( AutoIncList a, UID )
prependItem item ({ items, order } as autoIncList) =
    let
        ( uid, autoIncList_ ) =
            genUID autoIncList
    in
    ( { autoIncList_
        | items = Dict.insert uid item items
        , order = uid :: order
      }
    , uid
    )


{-| Return the internal list of keyed items with their UID.
-}
toList : AutoIncList a -> List ( UID, a )
toList { items, order } =
    order
        |> List.filterMap
            (\uid ->
                items
                    |> Dict.get uid
                    |> Maybe.map (\item -> ( uid, item ))
            )


{-| Maybe faster than toList + List.map?

Avoids second List fold, but doubles the Maybe checks.

-}
mapToList : (UID -> a -> b) -> AutoIncList a -> List b
mapToList func { items, order } =
    order
        |> List.filterMap
            (\uid ->
                items
                    |> Dict.get uid
                    |> Maybe.map (func uid)
            )


{-| Get the list of items without their UID
-}
values : AutoIncList a -> List a
values { items, order } =
    order
        |> List.filterMap (\uid -> Dict.get uid items)


{-| Shortcut for common use case. Uses Html.Keyed.
-}
mapToHTML : (UID -> a -> Html.Html msg) -> String -> List (Html.Attribute msg) -> AutoIncList a -> Html.Html msg
mapToHTML func tagName attributes ({ items, order } as autoIncList) =
    let
        nodes =
            autoIncList
                |> mapToList
                    (\uid item ->
                        ( uidToString uid, func uid item )
                    )
    in
    Html.Keyed.node tagName attributes nodes


{-| func should return an entire table row (tr) tag
-}
mapToTableBody : (UID -> a -> Html.Html msg) -> List (Html.Attribute msg) -> AutoIncList a -> Html.Html msg
mapToTableBody func attributes list =
    mapToHTML func "tbody" attributes list


{-| Same as Dict.map
-}
map : (UID -> a -> b) -> AutoIncList a -> AutoIncList b
map func ({ items } as autoIncList) =
    { items = Dict.map func items
    , nextUID = autoIncList.nextUID
    , order = autoIncList.order
    }


{-| Update an invidual element by its UID. Does nothing if UID
does not exist.

Unlike Dict, we assume that the user doesn't care if the item doesn't exist.

-}
update : UID -> (a -> a) -> AutoIncList a -> AutoIncList a
update uid func ({ items } as autoIncList) =
    { autoIncList
        | items =
            Dict.update
                uid
                (Maybe.map func)
                items
    }


{-| Update an item and return a Cmd.
-}
updateWithCommand : UID -> (a -> ( a, Cmd msg )) -> AutoIncList a -> ( AutoIncList a, Cmd msg )
updateWithCommand uid func ({ items } as autoIncList) =
    case Dict.get uid items of
        Just item ->
            let
                ( updatedItem, cmd ) =
                    func item
            in
            ( { autoIncList | items = Dict.insert uid updatedItem items }
            , cmd
            )

        Nothing ->
            ( autoIncList, Cmd.none )


itemToCommand : UID -> (a -> Cmd msg) -> AutoIncList a -> Cmd msg
itemToCommand uid func { items } =
    items
        |> Dict.get uid
        |> Maybe.map func
        |> Maybe.withDefault Cmd.none


{-| Replace an element in the list. Does nothing if not found.
-}
set : UID -> a -> AutoIncList a -> AutoIncList a
set uid elt ({ items } as autoIncList) =
    { autoIncList | items = Dict.insert uid elt items }


{-| Get an element in the list by its UID.
-}
get : UID -> AutoIncList a -> Maybe a
get uid { items } =
    Dict.get uid items


{-| Remove an element from the list by its UID. Does nothing
if no element has the UID.
-}
remove : UID -> AutoIncList a -> AutoIncList a
remove uid ({ items, order } as autoIncList) =
    { autoIncList
        | items = Dict.remove uid items
        , order = List.filter (\elt -> elt /= uid) order
    }


{-| Re-implementation of List.sortBy
-}
sortBy : (UID -> a -> comparable) -> AutoIncList a -> AutoIncList a
sortBy func ({ items, order } as autoIncList) =
    { autoIncList
        | order =
            autoIncList
                |> toList
                |> List.sortBy (\( uid, item ) -> func uid item)
                |> List.map Tuple.first
    }


{-| Convenience function to sort by UID
-}
sortByUID : AutoIncList a -> AutoIncList a
sortByUID ({ items } as autoIncList) =
    { autoIncList | order = Dict.keys items }
