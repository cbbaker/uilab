module UI.Templates exposing (alistDecode, layoutStatic, layoutDynamic)

{-| Templates for creating UI components


@docs alistDecode, layoutStatic, layoutDynamic

-}

import Json.Decode as Json exposing (Decoder)
import UI.Types exposing (..)

{-| Transforms an alist of UI decoders to a decoder for an alist of UIs
-}

alistDecode
    : List ( String, Decoder (Model pane paneMsg) )
    -> Decoder (List ( String, (Model pane paneMsg) ))
alistDecode children =
    let
        helper
            : List ( String, Decoder (Model pane paneMsg) )
            -> List ( String, (Model pane paneMsg) )
            -> Decoder (List ( String, (Model pane paneMsg) ))
        helper input acc =
            case input of
                ( key, decoder ) :: rest ->
                    decoder |> Json.andThen (\value -> helper rest (( key, value ) :: acc))

                [] ->
                    Json.succeed <| List.reverse acc
    in
        helper children []


{-| Creates a layout where the children are driven by the JSON API
-}
layoutDynamic :
    LayoutViewer pane paneMsg
    -> Subscriptions pane paneMsg
    -> (String -> Decoder (Model pane paneMsg))
    -> Decoder (Model pane paneMsg)
layoutDynamic viewer subscriptions item =
    Json.keyValuePairs Json.value
        |> Json.andThen
            (\pairs ->
                let
                    decodePairs :
                        List ( String, Json.Value )
                        -> List ( String, Model pane paneMsg )
                        -> Decoder (Model pane paneMsg)
                    decodePairs pairs acc =
                        case pairs of
                            ( key, value ) :: rest ->
                                case Json.decodeValue (item key) value of
                                    Ok result ->
                                        decodePairs rest (( key, result ) :: acc)

                                    Err err ->
                                        Json.fail err

                            [] ->
                                acc
                                    |> List.reverse
                                    |> LayoutType viewer subscriptions
                                    |> Layout
                                    |> Json.succeed
                in
                    decodePairs pairs []
            )


{-| Creates a layout where the children are passed as arguments
-}
layoutStatic :
    LayoutViewer pane paneMsg
    -> Subscriptions pane paneMsg
    -> List ( String, Decoder (Model pane paneMsg) )
    -> Decoder (Model pane paneMsg)
layoutStatic viewer subscriptions children =
    let
        decodeChildren :
            List ( String, Decoder (Model pane paneMsg) )
            -> List ( String, Model pane paneMsg )
            -> Decoder (Model pane paneMsg)
        decodeChildren children acc =
            case children of
                ( key, decoder ) :: rest ->
                    decoder |> Json.andThen (\value -> decodeChildren rest (( key, value ) :: acc))

                [] ->
                    acc
                        |> List.reverse
                        |> LayoutType viewer subscriptions
                        |> Layout
                        |> Json.succeed
    in
        decodeChildren children []
