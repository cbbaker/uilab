module UI.Subscriptions exposing (subscriptions)

{-| Subscriptions for the UI types

@docs subscriptions
-}

import Json.Decode as Json exposing (..)
import UI.Types exposing (..)
import UI.Decoders exposing (..)
import PubSub


{-| Takes a layout registry, a pane decoder, a pane subscription
function, and a UI, and returns the subscriptions.
-}
subscriptions :
    LayoutRegistry pane paneMsg
    -> (pane -> Sub paneMsg)
    -> Model pane paneMsg
    -> Sub (Msg pane paneMsg)
subscriptions layouts paneSubscriptions model =
    case model of
        Layout layoutModel ->
            layoutModel |> layoutSubscriptions layouts paneSubscriptions |> Sub.map LayoutMsg

        Choice choiceModel ->
            choiceModel |> choiceSubscriptions layouts paneSubscriptions |> Sub.map ChoiceMsg

        Pane paneModel ->
            paneSubscriptions paneModel |> Sub.map PaneMsg


layoutSubscriptions :
    LayoutRegistry pane paneMsg
    -> (pane -> Sub paneMsg)
    -> LayoutType pane paneMsg
    -> Sub (LayoutMsgType pane paneMsg)
layoutSubscriptions layouts paneSubscriptions model =
    let
        sub ( key, child ) =
            child |> subscriptions layouts paneSubscriptions |> Sub.map (LayoutChildMsg key)

        childSubs =
            List.map sub model.children

        wrapUI key decoder =
            [ ( key, decoder ) ]

        insertSub =
            case model.subscriptions.insert of
                Just ( insertKey, template ) ->
                    [ PubSub.subscribe insertKey <| decodeTemplate Insert template ]

                Nothing ->
                    []

        updateSub =
            case model.subscriptions.update of
                Just ( updateKey, template ) ->
                    [ PubSub.subscribe updateKey <| decodeTemplate Update template ]

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


decodeTemplate :
    (List ( String, Model pane paneMsg ) -> LayoutMsgType pane paneMsg)
    -> (String -> Decoder (Model pane paneMsg))
    -> Decoder (LayoutMsgType pane paneMsg)
decodeTemplate tagger template =
    Json.keyValuePairs Json.value
        |> Json.andThen
            (\pairs ->
                let
                    decodePairs :
                        List ( String, Json.Value )
                        -> List ( String, Model pane paneMsg )
                        -> Decoder (LayoutMsgType pane paneMsg)
                    decodePairs pairs acc =
                        case pairs of
                            ( key, value ) :: rest ->
                                case Json.decodeValue (template key) value of
                                    Ok result ->
                                        decodePairs rest (( key, result ) :: acc)

                                    Err err ->
                                        Json.fail err

                            [] ->
                                acc
                                    |> List.reverse
                                    |> tagger
                                    |> Json.succeed
                in
                    decodePairs pairs []
            )


choiceSubscriptions :
    LayoutRegistry pane paneMsg
    -> (pane -> Sub paneMsg)
    -> ChoiceType pane paneMsg
    -> Sub (ChoiceMsgType pane paneMsg)
choiceSubscriptions layouts paneSubscriptions model =
    let
        childSub ( key, child ) =
            child |> subscriptions layouts paneSubscriptions |> Sub.map (ChoiceChildMsg key)

        chooseSub =
            case model.subscription of
                Just key ->
                    [ PubSub.subscribe key (Json.map Choose Json.string) ]

                Nothing ->
                    []
    in
        Sub.batch (chooseSub ++ (List.map childSub model.children))
