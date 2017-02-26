module UI.Viewers exposing (..)

import Html exposing (..)
import UI.Types exposing (..)


view :
    (pane -> Html paneMsg)
    -> Model pane paneMsg
    -> Html (Msg pane paneMsg)
view viewPane model =
    case model of
        Layout layoutModel ->
            layoutModel |> viewLayout viewPane |> Html.map LayoutMsg

        Choice choiceModel ->
            choiceModel |> viewChoice viewPane |> Html.map ChoiceMsg

        Pane paneModel ->
            paneModel |> viewPane |> Html.map PaneMsg


viewLayout : (pane -> Html paneMsg) -> LayoutType pane paneMsg -> Html (LayoutMsgType pane paneMsg)
viewLayout viewPane { viewer, children } =
    let
        viewChild : ( String, Model pane paneMsg ) -> ( String, Html (LayoutMsgType pane paneMsg) )
        viewChild child =
            let
                ( key, value ) =
                    child
            in
                ( key, Html.map (LayoutChildMsg key) <| view viewPane value )
    in
        children |> List.map viewChild |> viewer


currentChild : ChoiceType pane paneMsg -> Maybe (Model pane paneMsg)
currentChild { current, children } =
    let
        find list =
            case list of
                ( key, ui ) :: rest ->
                    if key == current then
                        Just ui
                    else
                        find rest

                _ ->
                    Nothing
    in
        find children


viewChoice : (pane -> Html paneMsg) -> ChoiceType pane paneMsg -> Html (ChoiceMsgType pane paneMsg)
viewChoice viewPane model =
    case currentChild model of
        Just child ->
            child |> view viewPane |> Html.map (ChoiceChildMsg model.current)

        Nothing ->
            div [] []
