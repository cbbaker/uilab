module Main exposing (..)

import Html exposing (..)
import Http
import UI


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Model
    = Loading
    | Loaded UI.Model


type Msg
    = Load (Result Http.Error UI.Model)
    | UIMsg UI.Msg


init : ( Model, Cmd Msg )
init =
    Loading ! [ Http.send Load <| Http.get "/root.json" UI.decodeModel ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Load (Err err), Loading ) ->
            (Debug.log "load failed" err) |> (always (model ! []))

        ( Load (Ok ui), _ ) ->
            Loaded ui ! []

        ( UIMsg uiMsg, Loaded uiModel ) ->
            let
                ( newUI, cmd ) =
                    UI.update uiMsg uiModel
            in
                Loaded newUI ! [ Cmd.map UIMsg cmd ]

        _ ->
            model ! []


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            h1 [] [ text "loading..." ]

        Loaded ui ->
            Html.map UIMsg <| UI.view ui
