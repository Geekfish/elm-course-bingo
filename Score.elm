module Score exposing (Score, viewScore, postScore)

import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Http
import Html exposing (span, text, div, Html)
import Html.Attributes exposing (class)
import Entry


type alias Score =
    { id : Int
    , name : String
    , score : Int
    }


scoreDecoder : Decoder Score
scoreDecoder =
    decode Score
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "score" Decode.int


scoreEncoder : { a | entries : List Entry.Entry, name : String } -> Encode.Value
scoreEncoder model =
    Encode.object
        [ ( "name", Encode.string model.name )
        , ( "score", Encode.int (Entry.sumMarkedPoints model.entries) )
        ]


viewScore : Int -> Html msg
viewScore sum =
    div
        [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]


postScore : (Result Http.Error Score -> msg) -> String -> { a | entries : List Entry.Entry, name : String } -> Cmd msg
postScore msg url model =
    let
        body =
            model
                |> scoreEncoder
                |> Http.jsonBody

        request =
            Http.post url body scoreDecoder
    in
        Http.send msg request
