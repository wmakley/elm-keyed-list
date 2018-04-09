module KeyedList
    exposing
        ( KeyedList
        , UID
        , uidToString
        , empty
        , fromList
        , toList
        , values
        , unzip
        , length
        , isEmpty
        , update
        , set
        , get
        , remove
        , filter
        , appendItem
        , prependItem
        , mapToHTML
        , mapToTableBody
        , map
        , sortBy
        , sortByUID
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

@docs KeyedList, UID


# Initialization

@docs empty, fromList


# Conversions

@docs toList, values, unzip


# Updating elements by UID

@docs update, set, get


# Adding and removing items

@docs appendItem, remove, prependItem, filter


# Building UIs

@docs mapToHTML, mapToTableBody


# Sorting

@docs sortBy, sortByUID


# Other List functions

@docs length, isEmpty, append, map

-}

import Dict exposing (Dict)
import Html
import Html.Keyed


{-| Represents a KeyedList. Stores the next UID,
and the internal List of ( UID, element ) tuples.
-}
type alias KeyedList a =
    { nextUID : UID
    , orderedItems : List (KeyedElement a)
    }


{-| Treat as black box, may change in future.
-}
type alias UID =
    Int


uidToString : UID -> String
uidToString uid =
    toString uid


{-| Just a Tuple of a UID and the element.
-}
type alias KeyedElement a =
    ( UID, a )


empty : KeyedList a
empty =
    { nextUID = 1
    , orderedItems = []
    }


fromList : List a -> KeyedList a
fromList list =
    { nextUID = (List.length list) + 1
    , orderedItems =
        List.indexedMap
            (\index elt ->
                wrap (index + 1) elt
            )
            list
    }


length : KeyedList a -> Int
length { orderedItems } =
    List.length orderedItems


isEmpty : KeyedList a -> Bool
isEmpty { orderedItems } =
    List.isEmpty orderedItems


{-| Get a UID for an item, and return the list with incremented nextUID.
-}
genUID : KeyedList a -> ( UID, KeyedList a )
genUID ({ nextUID } as list) =
    ( list.nextUID, { list | nextUID = nextUID + 1 } )


{-| Put two KeyedLists together, much in the manner of List.append
-}
append : KeyedList a -> KeyedList a -> KeyedList a
append a b =
    let
        newList =
            List.append a.orderedItems b.orderedItems
    in
        { nextUID = calculateNextUID newList
        , orderedItems = newList
        }


{-| Recalculate the next UID if you don't have it any more.
-}
calculateNextUID : List (KeyedElement a) -> UID
calculateNextUID list =
    List.foldl
        (\( uid, _ ) nextUID ->
            if uid >= nextUID then
                uid + 1
            else
                nextUID
        )
        1
        list


{-| Add a single item to the end of the list. Deliberately
not named the same as List.append, because it works differently.
Slow, use prependItem if you can.
-}
appendItem : a -> KeyedList a -> ( KeyedList a, UID )
appendItem item ({ orderedItems } as list) =
    let
        ( uid, list_ ) =
            genUID list
    in
        ( { list_ | orderedItems = List.append orderedItems [ wrap uid item ] }
        , uid
        )


{-| Use this if you can, e.g. if the item goes at the top anyway,
or you are going to sort the KeyedList after adding it.
-}
prependItem : a -> KeyedList a -> KeyedList a
prependItem item ({ orderedItems } as list) =
    let
        ( uid, list_ ) =
            genUID list
    in
        { list_ | orderedItems = (wrap uid item) :: orderedItems }


{-| Wrap an item with its UID.
-}
wrap : UID -> a -> KeyedElement a
wrap uid item =
    ( uid, item )


{-| Return the internal list of keyed items.
Is just accessing a record member, so should be pretty fast.
-}
toList : KeyedList a -> List (KeyedElement a)
toList { orderedItems } =
    orderedItems


{-| Get the list of items without their UID
-}
values : KeyedList a -> List a
values { orderedItems } =
    List.map Tuple.second orderedItems


{-| Shortcut for common use case. Uses Html.Keyed.
-}
mapToHTML : (UID -> a -> Html.Html msg) -> String -> List (Html.Attribute msg) -> KeyedList a -> Html.Html msg
mapToHTML func tagName attributes { orderedItems } =
    let
        nodes =
            List.map
                (\( uid, item ) ->
                    ( uidToString uid, func uid item )
                )
                orderedItems
    in
        Html.Keyed.node tagName attributes nodes


{-| func should return an entire table row (tr) tag
-}
mapToTableBody : (UID -> a -> Html.Html msg) -> List (Html.Attribute msg) -> KeyedList a -> Html.Html msg
mapToTableBody func attributes list =
    mapToHTML func "tbody" attributes list


map : (UID -> a -> b) -> KeyedList a -> KeyedList b
map func ({ orderedItems } as keyedList) =
    { keyedList
        | orderedItems =
            List.map
                (\( uid, item ) ->
                    ( uid, func uid item )
                )
                orderedItems
    }


{-| Update an invidual element by its UID. Does nothing if UID
does not exist.
-}
update : UID -> (a -> a) -> KeyedList a -> KeyedList a
update uid func ({ orderedItems } as keyedList) =
    { keyedList
        | orderedItems =
            List.map
                (\(( uid_, item ) as elt) ->
                    if uid_ == uid then
                        ( uid_, func item )
                    else
                        elt
                )
                orderedItems
    }


{-| Replace an element in the list. Does nothing if not found.
-}
set : UID -> a -> KeyedList a -> KeyedList a
set uid elt keyedList =
    update uid (\_ -> elt) keyedList


{-| Get an element in the list by its UID. Try not to use with set or update,
because all of these operations iterate over the entire list.
-}
get : UID -> KeyedList a -> Maybe a
get uid { orderedItems } =
    orderedItems
        |> List.filterMap
            (\( uid_, elt ) ->
                if uid_ == uid then
                    Just elt
                else
                    Nothing
            )
        |> List.head


{-| Remove an element from the list by its UID. Does nothing
if no element has the UID.
-}
remove : UID -> KeyedList a -> KeyedList a
remove uid ({ orderedItems } as keyedList) =
    { keyedList
        | orderedItems =
            List.filter
                (\( uid_, item ) -> uid_ /= uid)
                orderedItems
    }


filter : (UID -> a -> Bool) -> KeyedList a -> KeyedList a
filter func ({ orderedItems } as keyedList) =
    { keyedList
        | orderedItems =
            List.filter
                (\( uid, item ) -> func uid item)
                orderedItems
    }


{-| Re-implementation of List.sortBy
-}
sortBy : (UID -> a -> comparable) -> KeyedList a -> KeyedList a
sortBy func ({ orderedItems } as keyedList) =
    { keyedList
        | orderedItems =
            List.sortBy
                (\( uid, item ) -> func uid item)
                orderedItems
    }


{-| Convenience function to sort by UID
-}
sortByUID : KeyedList a -> KeyedList a
sortByUID ({ orderedItems } as keyedList) =
    { keyedList | orderedItems = List.sortBy Tuple.first orderedItems }


unzip : KeyedList a -> ( List UID, List a )
unzip keyedList =
    keyedList |> toList |> List.unzip
