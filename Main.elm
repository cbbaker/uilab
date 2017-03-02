module Main exposing (..)

import Navigation exposing (program)
import Html exposing (Html, h1, text)
import Http
import Json.Decode exposing (..)

import UI.Types as UI
import UI.Subscriptions as UI
import UI.Decoders as UI
import UI.Updaters as UI
import UI.Viewers as UI
import Layout
import Pane


main : Program Value Model Msg
main =
    Navigation.programWithFlags UrlUpdate
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Loading
    | Loaded (UI.Model Pane.Model Pane.Msg)


type Msg
    = Load (Result Http.Error (UI.Model Pane.Model Pane.Msg))
    | UIMsg (UI.Msg Pane.Model Pane.Msg)
    | UrlUpdate Navigation.Location


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading ->
            Sub.none

        Loaded uiModel ->
            uiModel |> UI.subscriptions Layout.layouts Pane.decodeModel Pane.subscriptions |> Sub.map UIMsg

uiDecoder : Decoder (UI.Model Pane.Model Pane.Msg)
uiDecoder =
    UI.decodeModel Layout.layouts Pane.decodeModel

init : Value -> Navigation.Location -> ( Model, Cmd Msg )
init value { hash } =
    case decodeValue uiDecoder value of
        Ok ui ->
            Loaded ui ! []

        Err err ->
            (Debug.log "error parsing flags" err) |> always (getAjax hash Loading)


ajaxUrl : String -> Maybe String
ajaxUrl hash =
    if String.length hash > 1 then
        let 
            tag = String.dropLeft 1 hash
        in
            "/" ++ tag |> (Debug.log "loading") |> Just
    else
        Nothing

getAjax : String -> Model -> ( Model, Cmd Msg )
getAjax hash model =
    case ajaxUrl hash of
        Just url ->
            model ! [ Http.send Load <| Http.get url uiDecoder ]
        Nothing ->
            model ! []
                          
    


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Load (Err err), _ ) ->
            (Debug.log "load failed" err) |> (always <| getAjax "" model)

        ( Load (Ok ui), _ ) ->
            Loaded (Debug.log "ui" ui) ! []

        ( UIMsg uiMsg, Loaded uiModel ) ->
            let
                ( newUI, cmd ) =
                    UI.update Pane.update uiMsg uiModel
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
            Html.map UIMsg <| UI.view Pane.view ui
