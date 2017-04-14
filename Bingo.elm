module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes
    exposing
        ( class
        , href
        , classList
        , id
        , disabled
        , type_
        , placeholder
        , autofocus
        , value
        )
import Html.Events exposing (onClick, onInput, onFocus)
import Random
import Http
import ViewHelpers exposing (primaryButton, alert)
import Entry
import Score


-- Config


apiUrlPrefix : String
apiUrlPrefix =
    "http://localhost:3000"


entriesUrl : String
entriesUrl =
    apiUrlPrefix ++ "/random-entries"


scoreUrl : String
scoreUrl =
    apiUrlPrefix ++ "/scores"



-- MODEL


type GameState
    = EnteringName
    | Playing


type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry.Entry
    , alertMessage : Maybe String
    , validationError : Maybe String
    , nameInput : String
    , gameState : GameState
    }


initialModel : Model
initialModel =
    { name = "Anonymous"
    , gameNumber = 1
    , entries = []
    , alertMessage = Nothing
    , validationError = Nothing
    , nameInput = ""
    , gameState = EnteringName
    }



-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | NewRandom Int
    | NewEntries (Result Http.Error (List Entry.Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score.Score)
    | SetNameInput String
    | SaveName
    | CancelName
    | ChangeGameState GameState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandom randomNumber ->
            -- { model | gameNumber = randomNumber }, Cmd.none )
            -- -- SAME AS:
            { model | gameNumber = randomNumber } ! []

        ChangeGameState state ->
            { model | gameState = state } ! []

        SetNameInput name ->
            let
                message =
                    if String.isEmpty name then
                        Just "You need to enter a name"
                    else
                        Nothing
            in
                { model | nameInput = name, validationError = message } ! []

        SaveName ->
            if model.validationError == Nothing then
                { model
                    | name = model.nameInput
                    , alertMessage = Nothing
                    , nameInput = ""
                    , gameState = Playing
                }
                    ! []
            else
                { model
                    | alertMessage = model.validationError
                }
                    ! []

        CancelName ->
            { model
                | nameInput = ""
                , gameState = Playing
            }
                ! []

        ShareScore ->
            ( model, Score.postScore NewScore scoreUrl model )

        NewScore (Ok score) ->
            let
                message =
                    "Your score of "
                        ++ (toString score.score)
                        ++ " was successfully shared!"
            in
                { model | alertMessage = Just message } ! []

        NewScore (Err error) ->
            { model | alertMessage = Just (httpErrorToMessage error) } ! []

        NewGame ->
            -- ( { model | entries = initialEntries }, generateRandomNumber )
            -- -- SAME AS:
            { model | gameNumber = model.gameNumber + 1 } ! [ getEntries ]

        NewEntries (Ok randomEntries) ->
            { model | entries = (List.sortBy .points randomEntries) } ! []

        NewEntries (Err error) ->
            { model | alertMessage = Just (httpErrorToMessage error) } ! []

        CloseAlert ->
            { model | alertMessage = Nothing } ! []

        Mark id ->
            { model | entries = (Entry.markEntryWithId model.entries id) } ! []


httpErrorToMessage : Http.Error -> String
httpErrorToMessage error =
    case error of
        Http.NetworkError ->
            "Is the server running?"

        Http.BadStatus response ->
            (toString response.status)

        Http.BadPayload message _ ->
            "Decoding Failed: " ++ message

        _ ->
            (toString error)



-- COMMANDS


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate (\num -> NewRandom num) (Random.int 1 100)


getEntries : Cmd Msg
getEntries =
    Entry.getEntries NewEntries entriesUrl



-- VIEW


viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    h2 [ id "info", class "classy" ]
        [ a
            [ href "#"
            , onClick (ChangeGameState EnteringName)
            ]
            [ text name ]
        , text (" - Game #" ++ (toString gameNumber))
        ]


viewHeader : String -> Html Msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ a [ href "http://elm-lang.org" ]
            [ text "Powered by Elm" ]
        ]


shareButtonDisabled : Model -> Bool
shareButtonDisabled model =
    (Entry.sumMarkedPoints model.entries) == 0


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "Buzzword Bingo"
        , viewPlayer model.name model.gameNumber
        , alert CloseAlert model.alertMessage
        , viewNameInput model
        , Entry.viewEntryList Mark model.entries
        , Score.viewScore (Entry.sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ primaryButton NewGame "New Game" []
            , primaryButton ShareScore "Share Score" [ disabled (shareButtonDisabled model) ]
            ]
        , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]


viewNameInput : Model -> Html Msg
viewNameInput model =
    case model.gameState of
        EnteringName ->
            div [ class "name-input" ]
                [ input
                    [ type_ "text"
                    , placeholder "Who's playing?"
                    , autofocus True
                    , onInput SetNameInput
                    , onFocus (SetNameInput model.nameInput)
                    , value model.nameInput
                    ]
                    []
                , primaryButton SaveName "Save" []
                , primaryButton CancelName "Cancel" []
                ]

        Playing ->
            text ""



-- main : Html Msg
-- main =
--     view initialModel


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getEntries )
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
