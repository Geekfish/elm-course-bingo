module Entry
    exposing
        ( Entry
        , markEntryWithId
        , sumMarkedPoints
        , entryListDecoder
        , viewEntryList
        , getEntries
        )

import Http
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


markEntryWithId : List Entry -> Int -> List Entry
markEntryWithId entries id =
    let
        markEntry e =
            if e.id == id then
                { e | marked = (not e.marked) }
            else
                e
    in
        List.map markEntry entries


entryDecoder : Decoder Entry
entryDecoder =
    -- Decode.map4 Entry
    --     (field "id" Decode.int)
    --     (field "phrase" Decode.string)
    --     (field "points" Decode.int)
    --     (succeed False)
    decode Entry
        |> required "id" Decode.int
        |> required "phrase" Decode.string
        |> optional "points" Decode.int 100
        |> hardcoded False


entryListDecoder : Decoder (List Entry)
entryListDecoder =
    Decode.list entryDecoder


getEntries : (Result Http.Error (List Entry) -> msg) -> String -> Cmd msg
getEntries msg url =
    entryListDecoder
        |> Http.get url
        |> Http.send msg


viewEntryItem : (Int -> msg) -> Entry -> Html msg
viewEntryItem msg entry =
    li
        [ classList [ ( "marked", entry.marked ) ]
        , onClick (msg entry.id)
        ]
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "points" ] [ text (toString entry.points) ]
        ]


viewEntryList : (Int -> msg) -> List Entry -> Html msg
viewEntryList msg entries =
    -- ul [] (List.map viewEntryItem entries)
    -- -- OR
    -- entries
    --     |> List.map viewEntryItem
    --     |> ul []
    let
        listOfEntries =
            List.map (viewEntryItem msg) entries
    in
        ul [] listOfEntries


sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.foldl (\e sum -> sum + e.points) 0
