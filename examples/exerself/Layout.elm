module Layout exposing (..)

import Dict exposing (Dict)
import UI.Types exposing (..)

import Layout.Panel
import Layout.Container
import Layout.Row
import Layout.ListGroup
import Layout.ListGroupItem
import Layout.PullRight
import Pane


layouts : Dict String (LayoutViewer Pane.Model Pane.Msg)
layouts =
    Dict.fromList
        [ ( "Panel", Layout.Panel.view )
        , ( "Container", Layout.Container.view )
        , ( "Row", Layout.Row.view )
        , ( "ListGroup", Layout.ListGroup.view )
        , ( "ListGroupItem", Layout.ListGroupItem.view )
        , ( "PullRight", Layout.PullRight.view )
        ]
