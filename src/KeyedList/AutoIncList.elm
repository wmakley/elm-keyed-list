module KeyedList.AutoIncList exposing
    ( AutoIncList, UID
    , empty, fromList
    , toList, keys, values, mapToList, unorderedMap, toUnorderedList, mapAndUnzip
    , update, set, get, updateWithCommand
    , append, remove, prepend
    , mapToHtml, mapToTableBody
    , sortBy, sortByUID
    , length, isEmpty, map
    , uidToString, uidDecoder
    , filter, reverse
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

@docs toList, keys, values, mapToList, unorderedMap, toUnorderedList, mapAndUnzip


# Updating elements by UID

@docs update, set, get, updateWithCommand


# Adding and removing items

@docs append, remove, prepend


# Building UIs

@docs mapToHtml, mapToTableBody


# Sorting

@docs sortBy, sortByUID


# Other List functions

@docs length, isEmpty, map


# UIDs

@docs uidToString, uidDecoder

-}

import Dict exposing (Dict)
import Html
import Html.Keyed
import Json.Decode as Json
import KeyedList exposing (KeyedList)


{-| Represents an AutoIncList. Stores the next UID,
and the internal List of ( UID, element ) tuples.
-}
type alias AutoIncList a =
    { nextUID : UID
    , list : KeyedList UID a
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
    , list = KeyedList.empty
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
    , list = KeyedList.fromList itemsWithKeys
    }


{-| Get the length of the List.
-}
length : AutoIncList a -> Int
length { list } =
    KeyedList.length list


{-| Check if the list is empty.
-}
isEmpty : AutoIncList a -> Bool
isEmpty { list } =
    KeyedList.isEmpty list


{-| Get a UID for an item, and return the list with incremented nextUID.
-}
genUID : AutoIncList a -> ( UID, AutoIncList a )
genUID ({ nextUID } as autoIncList) =
    ( autoIncList.nextUID, { autoIncList | nextUID = nextUID + 1 } )


{-| Recalculate the next UID if you don't have it any more.
-}
calculateNextUID : AutoIncList a -> UID
calculateNextUID { list } =
    (List.maximum (KeyedList.keys list) |> Maybe.withDefault 0) + 1


{-| Add a single item to the end of the list. Deliberately
not named the same as List.append, because it works differently.
-}
append : a -> AutoIncList a -> ( AutoIncList a, UID )
append item ({ list } as autoIncList) =
    let
        ( uid, autoIncList_ ) =
            genUID autoIncList
    in
    ( { autoIncList_
        | list = KeyedList.append uid item list
      }
    , uid
    )


{-| Use this if you can, e.g. if the item goes at the top anyway,
or you are going to sort the AutoIncList after adding it.
-}
prepend : a -> AutoIncList a -> ( AutoIncList a, UID )
prepend item autoIncList =
    let
        ( uid, autoIncList_ ) =
            genUID autoIncList
    in
    ( { autoIncList_
        | list = KeyedList.prepend uid item autoIncList.list
      }
    , uid
    )


{-| Return the internal list of keyed items with their UID.
-}
toList : AutoIncList a -> List ( UID, a )
toList { list } =
    KeyedList.toList list


{-| Maybe faster than toList + List.map?

Avoids second List fold, but doubles the Maybe checks.

-}
mapToList : (UID -> a -> b) -> AutoIncList a -> List b
mapToList func { list } =
    KeyedList.mapToList func list


keys : AutoIncList a -> List UID
keys { list } =
    KeyedList.keys list


{-| Get the list of items without their UID
-}
values : AutoIncList a -> List a
values { list } =
    KeyedList.values list


{-| Shortcut for common use case. Uses Html.Keyed.
-}
mapToHtml :
    (UID -> a -> Html.Html msg)
    -> String
    -> List (Html.Attribute msg)
    -> AutoIncList a
    -> Html.Html msg
mapToHtml toHtml tagName attributes autoIncList =
    KeyedList.mapToHtml
        uidToString
        toHtml
        tagName
        attributes
        autoIncList.list


{-| func should return an entire table row (tr) tag
-}
mapToTableBody : (UID -> a -> Html.Html msg) -> List (Html.Attribute msg) -> AutoIncList a -> Html.Html msg
mapToTableBody func attributes list =
    mapToHtml func "tbody" attributes list


{-| Same as Dict.map
-}
map : (UID -> a -> b) -> AutoIncList a -> AutoIncList b
map func ({ list, nextUID } as autoIncList) =
    { list = KeyedList.map func list
    , nextUID = nextUID
    }


{-| Forget about the custom order and just do a Dict.map
-}
unorderedMap : (UID -> a -> b) -> AutoIncList a -> AutoIncList b
unorderedMap func { nextUID, list } =
    { nextUID = nextUID
    , list = KeyedList.unorderedMap func list
    }


{-| Map over an AutoIncList, splitting off a seconary list of each second tuple element
and its key. Unordered operation. The second list is in key order.

Probably extremely inefficient.

-}
mapAndUnzip :
    (UID -> a -> ( b, c ))
    -> AutoIncList a
    -> ( AutoIncList b, List c )
mapAndUnzip toTuple autoIncList =
    let
        ( listA, listB ) =
            KeyedList.mapAndUnzip toTuple autoIncList.list
    in
    ( { nextUID = autoIncList.nextUID
      , list = listA
      }
    , listB
    )


{-| Forget about the custom order and just do a Dict.toList
-}
toUnorderedList : AutoIncList a -> List ( UID, a )
toUnorderedList { list } =
    KeyedList.toUnorderedList list


{-| Update an invidual element by its UID.
-}
update : UID -> (Maybe a -> Maybe a) -> AutoIncList a -> AutoIncList a
update uid func ({ list } as autoIncList) =
    { autoIncList
        | list = KeyedList.update uid func list
    }


{-| Update an item and return a Cmd.
-}
updateWithCommand : UID -> (a -> ( a, Cmd msg )) -> AutoIncList a -> ( AutoIncList a, Cmd msg )
updateWithCommand uid func ({ list } as autoIncList) =
    let
        ( updatedList, cmd ) =
            KeyedList.updateWithCommand uid func list
    in
    ( { autoIncList | list = updatedList }, cmd )


itemToCommand : UID -> (a -> Cmd msg) -> AutoIncList a -> Cmd msg
itemToCommand uid func { list } =
    KeyedList.itemToCommand uid func list


{-| Replace an element in the list. Does nothing if not found.
-}
set : UID -> a -> AutoIncList a -> AutoIncList a
set uid newValue ({ list } as autoIncList) =
    { autoIncList | list = KeyedList.set uid newValue list }


{-| Get an element in the list by its UID.
-}
get : UID -> AutoIncList a -> Maybe a
get uid { list } =
    KeyedList.get uid list


filter : (UID -> a -> Bool) -> AutoIncList a -> AutoIncList a
filter func ({ list } as autoIncList) =
    { autoIncList | list = KeyedList.filter func list }


{-| Remove an element from the list by its UID. Does nothing
if no element has the UID.
-}
remove : UID -> AutoIncList a -> AutoIncList a
remove uid ({ list } as autoIncList) =
    { autoIncList
        | list = KeyedList.remove uid list
    }


{-| Re-implementation of List.sortBy
-}
sortBy : (UID -> a -> comparable) -> AutoIncList a -> AutoIncList a
sortBy func ({ list } as autoIncList) =
    { autoIncList | list = KeyedList.sortBy func list }


{-| Convenience function to sort by UID
-}
sortByUID : AutoIncList a -> AutoIncList a
sortByUID ({ list } as autoIncList) =
    { autoIncList | list = KeyedList.sortByKey list }


reverse : AutoIncList a -> AutoIncList a
reverse ({ list } as autoIncList) =
    { autoIncList | list = KeyedList.reverse list }
