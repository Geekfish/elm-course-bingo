module ViewHelpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


primaryButton : msg -> String -> List (Html.Attribute msg) -> Html msg
primaryButton msg name extraAttributes =
    let
        attributes =
            List.append
                [ class "primary", onClick msg ]
                extraAttributes
    in
        button attributes [ text name ]


alert : msg -> Maybe String -> Html msg
alert msg alertMessage =
    case alertMessage of
        Just message ->
            div [ class "alert" ]
                [ span [ class "close", onClick msg ] [ text "X" ]
                , text message
                ]

        Nothing ->
            text ""
