module UI.Subscriptions exposing (subscriptions)

{-| Subscriptions for the UI types

@docs subscriptions
-}

import Dict exposing (Dict)
import Json.Decode as Json exposing (..)
import UI.Types exposing (..)
import UI.Decoders exposing (..)
import PubSub


{-| Takes a layout registry, a pane decoder, a pane subscription
function, and a UI, and returns the subscriptions.
-}

subscriptions :
    Dict String (LayoutViewer pane paneMsg)
    -> (String -> Decoder pane)
    -> (pane -> Sub paneMsg)
    -> Model pane paneMsg
    -> Sub (Msg pane paneMsg)
subscriptions layouts decodePane paneSubscriptions model =
    case model of
        Layout layoutModel ->
            layoutModel |> layoutSubscriptions layouts decodePane paneSubscriptions |> Sub.map LayoutMsg

        Choice choiceModel ->
            choiceModel |> choiceSubscriptions layouts decodePane paneSubscriptions |> Sub.map ChoiceMsg

        Pane paneModel ->
            paneSubscriptions paneModel |> Sub.map PaneMsg


layoutSubscriptions :
    Dict String (LayoutViewer pane paneMsg)
    -> (String -> Decoder pane)
    -> (pane -> Sub paneMsg)
    -> LayoutType pane paneMsg
    -> Sub (LayoutMsgType pane paneMsg)
layoutSubscriptions layouts decodePane paneSubscriptions model =
    let
        sub ( key, child ) =
            child |> subscriptions layouts decodePane paneSubscriptions |> Sub.map (LayoutChildMsg key)

        childSubs =
            List.map sub model.children

        insertSub =
            case model.subscriptions.insert of
                Just ( insertKey, decoder ) ->
                    [ PubSub.subscribe insertKey <| decodeLayoutInsert layouts decodePane ]

                Nothing ->
                    []

        updateSub =
            case model.subscriptions.update of
                Just ( updateKey, decoder ) ->
                    [ PubSub.subscribe updateKey <| decodeLayoutUpdate layouts decodePane ]

                Nothing ->
                    []

        deleteSub =
            case model.subscriptions.delete of
                Just deleteKey ->
                    [ PubSub.subscribe deleteKey decodeLayoutDelete ]

                Nothing ->
                    []
    in
        Sub.batch (insertSub ++ updateSub ++ deleteSub ++ childSubs)


choiceSubscriptions :
    Dict String (LayoutViewer pane paneMsg)
    -> (String -> Decoder pane)
    -> (pane -> Sub paneMsg)
    -> ChoiceType pane paneMsg
    -> Sub (ChoiceMsgType pane paneMsg)
choiceSubscriptions layouts decodePane paneSubscriptions model =
    let
        childSub ( key, child ) =
            child |> subscriptions layouts decodePane paneSubscriptions |> Sub.map (ChoiceChildMsg key)

        chooseSub =
            case model.subscription of
                Just key ->
                    [ PubSub.subscribe key (Json.map Choose Json.string) ]

                Nothing ->
                    []
    in
        Sub.batch (chooseSub ++ (List.map childSub model.children))
