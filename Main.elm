module Main exposing (..)

import Navigation exposing (program)
import Html exposing (Html, h1, text)
import Http
import UI


main : Program Never Model Msg
main =
    Navigation.program UrlUpdate
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Loading
    | Loaded UI.Model


type Msg
    = Load (Result Http.Error UI.Model)
    | UIMsg UI.Msg
    | UrlUpdate Navigation.Location


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading ->
            Sub.none

        Loaded uiModel ->
            uiModel |> UI.subscriptions |> Sub.map UIMsg


init : Navigation.Location -> ( Model, Cmd Msg )
init { hash } =
    getAjax hash Loading


ajaxUrl : String -> String
ajaxUrl hash =
    let
        tag =
            if String.length (hash) > 1 then
                String.dropLeft 1 hash
            else
                "root"
    in
        "/" ++ tag ++ ".json" |> (Debug.log "loading")


getAjax : String -> Model -> ( Model, Cmd Msg )
getAjax hash model =
    model ! [ Http.send Load <| Http.get (ajaxUrl hash) UI.decodeModel ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Load (Err err), Loading ) ->
            (Debug.log "load failed" err) |> (always <| getAjax "" model)

        ( Load (Ok ui), _ ) ->
            Loaded (Debug.log "ui" ui) ! []

        ( UIMsg uiMsg, Loaded uiModel ) ->
            let
                ( newUI, cmd ) =
                    UI.update uiMsg uiModel
            in
                Loaded newUI ! [ Cmd.map UIMsg cmd ]

        ( UrlUpdate { hash }, _ ) ->
            getAjax hash model

        _ ->
            model ! []


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            h1 [] [ text "loading..." ]

        Loaded ui ->
            Html.map UIMsg <| UI.view ui
