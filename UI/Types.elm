module UI.Types exposing (..)

import Html exposing (..)
import Json.Decode exposing (Decoder)

type Model pane paneMsg
    = Layout (LayoutType pane paneMsg)
    | Choice (ChoiceType pane paneMsg)
    | Pane pane


type alias LayoutType pane paneMsg =
    { viewer : LayoutViewer pane paneMsg
    , subscriptions : Subscriptions pane paneMsg
    , children : List ( String, Model pane paneMsg )
    }


type alias LayoutViewer pane paneMsg =
    List ( String, Html (LayoutMsgType pane paneMsg) ) -> Html (LayoutMsgType pane paneMsg)


type alias Subscriptions pane paneMsg =
    { insert : Maybe ( String, Decoder (Model pane paneMsg) )
    , update : Maybe ( String, Decoder (Model pane paneMsg) )
    , delete : Maybe String
    }

type alias ChoiceType pane paneMsg =
    { subscription : Maybe String
    , current : String
    , children : List ( String, Model pane paneMsg )
    }


type Msg pane paneMsg
    = LayoutMsg (LayoutMsgType pane paneMsg)
    | ChoiceMsg (ChoiceMsgType pane paneMsg)
    | PaneMsg paneMsg


type LayoutMsgType pane paneMsg
    = LayoutChildMsg String (Msg pane paneMsg)
    | Insert (List ( String, Model pane paneMsg ))
    | Update (List ( String, Model pane paneMsg ))
    | Delete (List String)

type ChoiceMsgType pane paneMsg
    = ChoiceChildMsg String (Msg pane paneMsg)
    | Choose String
