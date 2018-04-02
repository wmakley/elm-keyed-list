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
        , map
        , update
        , remove
        , sortBy
        )

{-| Implements some Dict functions on top of a list,
that are useful for building tabular UI's.

Slow, but if I also use a Dict I get the worst of both worlds.

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


{-| Get a UID for an item, and return the list with incremented UID.
-}
genUID : KeyedList a -> ( UID, KeyedList a )
genUID ({ nextUID } as list) =
    ( list.nextUID, { list | nextUID = nextUID + 1 } )


appendItem : a -> KeyedList a -> ( KeyedList a, UID )
appendItem item ({ orderedItems } as list) =
    let
        ( uid, list_ ) =
            genUID list
    in
        ( { list_ | orderedItems = List.append orderedItems [ wrap uid item ] }
        , uid
        )


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
