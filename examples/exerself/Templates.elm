module Templates exposing (..)

import Json.Decode as Json exposing (Decoder)
import UI.Types as UI
import UI.Templates as UI

import Layout.Container
import Layout.ListGroup
import Layout.Panel
import Layout.PullRight
import Layout.Row
import Pane


type alias UI =
    UI.Model Pane.Model Pane.Msg


item : String -> Decoder UI
item id =
    Json.map (UI.Choice << UI.ChoiceType (Just id) "show") <|
        UI.alistDecode
            [ ( "show", Pane.show id )
            , ( "edit", Pane.edit id )
            ]

insertItem : String -> Decoder UI
insertItem id =
    (Json.field "type" Json.string)
        |> Json.andThen (\type_ ->
                        case (Debug.log "insertItem type" type_) of
                            "newItem" ->
                                Pane.edit id
                            _ ->
                                item id
                        )


rideSubscriptions : UI.Subscriptions Pane.Model Pane.Msg
rideSubscriptions =
    UI.Subscriptions (Just ( "insertRide", insertItem )) Nothing (Just "deleteRide")


itemList : (String -> Decoder UI) -> Decoder UI
itemList =
    UI.layoutDynamic Layout.ListGroup.view rideSubscriptions


noSubscriptions : UI.Subscriptions Pane.Model Pane.Msg
noSubscriptions =
    UI.Subscriptions Nothing Nothing Nothing


container : List ( String, Decoder UI ) -> Decoder UI
container =
    UI.layoutStatic Layout.Container.view noSubscriptions 


panel : List ( String, Decoder UI ) -> Decoder UI
panel =
    UI.layoutStatic Layout.Panel.view noSubscriptions


row : List ( String, Decoder UI ) -> Decoder UI
row =
    UI.layoutStatic Layout.Row.view noSubscriptions


pullRight : List ( String, Decoder UI ) -> Decoder UI
pullRight =
    UI.layoutStatic Layout.PullRight.view noSubscriptions


flash : Decoder UI
flash =
    row [ ( "flash", Pane.flash ) ]


newButton : Decoder UI
newButton =
    row [ ( "pullRight", pullRight [ ( "createButton", Pane.createButton ) ] ) ]


body : Decoder UI
body =
    container
        [ ( "flash", flash )
        , ( "newButton", (Json.field "newButton" newButton) )
        , ( "list"
          , row
                [ ( "list"
                  , (Json.field "list" (itemList item))
                  )
                ]
          )
        ]


root : Decoder UI
root =
    container
        [ ( "panel"
          , panel
                [ ( "heading", (Json.field "heading" Pane.heading) )
                , ( "body", (Json.field "body" body) )
                ]
          )
        ]
