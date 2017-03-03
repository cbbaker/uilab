module UI.Updaters exposing (update)

{-| Updates for the UI types

@docs update
-}

import UI.Types exposing (..)

{-| Takes a pane update function, a message, and a UI model, and
returns the updated model and commands
-}

update :
    (paneMsg -> pane -> ( pane, Cmd paneMsg ))
    -> Msg pane paneMsg
    -> Model pane paneMsg
    -> ( Model pane paneMsg, Cmd (Msg pane paneMsg) )
update updatePane msg model =
    case ( msg, model ) of
        ( LayoutMsg layoutMsg, Layout layoutModel ) ->
            let
                ( newLayout, cmd ) =
                    updateLayout updatePane layoutMsg layoutModel
            in
                Layout newLayout ! [ Cmd.map LayoutMsg cmd ]

        ( ChoiceMsg choiceMsg, Choice choiceModel ) ->
            let
                ( newChoice, cmd ) =
                    updateChoice updatePane choiceMsg choiceModel
            in
                Choice newChoice ! [ Cmd.map ChoiceMsg cmd ]

        ( PaneMsg paneMsg, Pane paneModel ) ->
            let
                ( newPane, cmd ) =
                    updatePane paneMsg paneModel
            in
                Pane newPane ! [ Cmd.map PaneMsg cmd ]

        _ ->
            model ! []


updateLayout :
    (paneMsg -> pane -> ( pane, Cmd paneMsg ))
    -> LayoutMsgType pane paneMsg
    -> LayoutType pane paneMsg
    -> ( LayoutType pane paneMsg, Cmd (LayoutMsgType pane paneMsg) )
updateLayout updatePane msg model =
    case msg of
        LayoutChildMsg key uiMsg ->
            model.children
                |> updateLayoutChildren updatePane key uiMsg
                |> Tuple.mapFirst (\children -> { model | children = children })

        Insert inserts ->
            updateLayoutInserts (Debug.log "inserts" inserts) model

        Update updateLayouts ->
            model ! []

        Delete deletes ->
            updateLayoutDeletes deletes model


updateLayoutChildren :
    (paneMsg -> pane -> ( pane, Cmd paneMsg ))
    -> String
    -> Msg pane paneMsg
    -> List ( String, Model pane paneMsg )
    -> ( List ( String, Model pane paneMsg ), Cmd (LayoutMsgType pane paneMsg) )
updateLayoutChildren updatePane updateLayoutKey msg models =
    let
        combine key ( newUI, newCmd ) ( uis, cmd ) =
            ( ( key, newUI ) :: uis
            , Cmd.batch
                [ Cmd.map (LayoutChildMsg key) newCmd
                , cmd
                ]
            )

        updateChild ( key, old ) result =
            let
                current =
                    if key == updateLayoutKey then
                        update updatePane msg old
                    else
                        ( old, Cmd.none )
            in
                combine key current result
    in
        List.foldr updateChild ( [], Cmd.none ) models




updateLayoutInserts :
    List ( String, Model pane paneMsg )
    -> LayoutType pane paneMsg
    -> ( LayoutType pane paneMsg, Cmd (LayoutMsgType pane paneMsg) )
updateLayoutInserts inserts model =
    { model | children = inserts ++ model.children } ! []


updateLayoutDeletes :
    List String
    -> LayoutType pane paneMsg
    -> ( LayoutType pane paneMsg, Cmd (LayoutMsgType pane paneMsg) )
updateLayoutDeletes deletes model =
    let
        filter ( key, _ ) =
            not (List.member key deletes)
    in
        { model | children = List.filter filter model.children } ! []
updateChoice :
    (paneMsg -> pane -> ( pane, Cmd paneMsg ))
    -> ChoiceMsgType pane paneMsg
    -> ChoiceType pane paneMsg
    -> ( ChoiceType pane paneMsg, Cmd (ChoiceMsgType pane paneMsg) )
updateChoice updatePane msg model =
    case msg of
        ChoiceChildMsg key uiMsg ->
            model.children
                |> updateChoiceChildren updatePane key uiMsg
                |> Tuple.mapFirst (\children -> { model | children = children })

        Choose key ->
            { model | current = key } ! []


updateChoiceChildren :
    (paneMsg -> pane -> ( pane, Cmd paneMsg ))
    -> String
    -> Msg pane paneMsg
    -> List ( String, Model pane paneMsg )
    -> ( List ( String, Model pane paneMsg ), Cmd (ChoiceMsgType pane paneMsg) )
updateChoiceChildren updatePane updateChoiceKey msg models =
    let
        combine key ( newUI, newCmd ) ( uis, cmd ) =
            ( ( key, newUI ) :: uis
            , Cmd.batch
                [ Cmd.map (ChoiceChildMsg key) newCmd
                , cmd
                ]
            )

        updateChild ( key, old ) result =
            let
                current =
                    if key == updateChoiceKey then
                        update updatePane msg old
                    else
                        ( old, Cmd.none )
            in
                combine key current result
    in
        List.foldr updateChild ( [], Cmd.none ) models
