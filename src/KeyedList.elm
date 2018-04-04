module KeyedList
    exposing
        ( KeyedList
        , KeyedElement
        , UID
        , empty
        , appendItem
        , toList
        , mapToList
        , mapToHTML
        , mapToTableBody
        , map
        , update
        , remove
        , sortBy
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

Array: Great at updating individual elements, hilariously
inefficient to map to HTML. Also, uses position-based
keys.

Dict: Would be perfect, but it doesn't preserve order.

Combining any of these wouldn't make sense, because you
just get the worst of both worlds!

-}

import Dict exposing (Dict)
import Html
import Html.Keyed


type alias KeyedList a =
    { nextUID : UID
    , orderedItems : List (KeyedElement a)
    }


type alias UID =
    Int


type alias KeyedElement a =
    ( UID, a )


empty : KeyedList a
empty =
    { nextUID = 1
    , orderedItems = []
    }


length { orderedItems } =
    List.length orderedItems


{-| Get a UID for an item, and return the list with incremented UID.
-}
genUID : KeyedList a -> ( UID, KeyedList a )
genUID ({ nextUID } as list) =
    ( list.nextUID, { list | nextUID = nextUID + 1 } )


appendItem : a -> KeyedList a -> KeyedList a
appendItem item ({ orderedItems } as list) =
    let
        ( uid, list_ ) =
            genUID list
    in
        { list_ | orderedItems = List.append orderedItems [ wrap uid item ] }


{-| Wrap an item with its UID.
-}
wrap : UID -> a -> KeyedElement a
wrap uid item =
    ( uid, item )


toList : KeyedList a -> List (KeyedElement a)
toList keyedList =
    keyedList.orderedItems


mapToList : (UID -> a -> b) -> KeyedList a -> List b
mapToList func keyedList =
    List.map
        (\( uid, item ) ->
            func uid item
        )
        keyedList.orderedItems


{-| Shortcut for common use case. Uses Html.Keyed.
-}
mapToHTML : (UID -> a -> Html.Html msg) -> String -> List (Html.Attribute msg) -> KeyedList a -> Html.Html msg
mapToHTML func tagName attributes { orderedItems } =
    let
        nodes =
            List.map
                (\( uid, item ) ->
                    ( toString uid, func uid item )
                )
                orderedItems
    in
        Html.Keyed.node tagName attributes nodes


mapToTableBody : (UID -> a -> Html.Html msg) -> List (Html.Attribute msg) -> KeyedList a -> Html.Html msg
mapToTableBody func attributes list =
    mapToHTML func "tbody" attributes list



-- let
--     rows =
--         List.map
--             (\( uid, item ) ->
--                 ( toString uid, func uid item )
--             )
--             orderedItems
-- in
--     Html.Keyed.node "tbody" attributes rows


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


remove : UID -> KeyedList a -> KeyedList a
remove uid ({ orderedItems } as keyedList) =
    { keyedList
        | orderedItems =
            List.filter
                (\( uid_, item ) -> uid_ /= uid)
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


sortByUID : KeyedList a -> KeyedList a
sortByUID ({ orderedItems } as keyedList) =
    { keyedList | orderedItems = List.sortBy Tuple.first orderedItems }


unzip : KeyedList a -> ( List UID, List a )
unzip keyedList =
    keyedList |> toList |> List.unzip
