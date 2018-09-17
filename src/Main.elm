module Main exposing (CommandWindow, Model, Msg(..), drawCircle, main, update, view)

import Browser
import Browser.Events
import Debug
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (on, onClick, onMouseUp)
import Json.Decode exposing (..)
import String exposing (fromFloat)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (pair)


init : String -> ( Model, Cmd Msg )
init =
    always ( Model Nothing, Cmd.none )


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscription
        }


type Msg
    = OpenWindow Point
    | CloseWindow
    | MousePosition Point


type alias Point =
    { x : Float, y : Float }


type alias CommandWindow =
    { center : Point
    , mouse : Point
    }


type alias Model =
    { commandWindow : Maybe CommandWindow
    }


subscription : Model -> Sub Msg
subscription model =
    case model.commandWindow of
        Just _ ->
            Browser.Events.onMouseMove <| mouseDecoder MousePosition

        Nothing ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenWindow circlePoint ->
            ( { model | commandWindow = Just <| CommandWindow circlePoint circlePoint }
            , Cmd.none
            )

        CloseWindow ->
            ( { model | commandWindow = Nothing }, Cmd.none )

        MousePosition pos ->
            case model.commandWindow of
                Just currentCmd ->
                    ( { model | commandWindow = Just { currentCmd | mouse = pos } }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


mouseDecoder : (Point -> a) -> Decoder a
mouseDecoder f =
    map2 (\x y -> Point x y |> f) (field "clientX" float) (field "clientY" float)


onMouseDownWithPoint : (Point -> Msg) -> Html.Attribute Msg
onMouseDownWithPoint f =
    on "mousedown" <| mouseDecoder f



-- 引数は 0 ~ 2pi であること


angleDistant : Float -> Float -> Float
angleDistant a b =
    let
        x =
            a - b |> abs

        y =
            a - b + 2 * pi |> abs
    in
    Basics.min x y


absAngle : Float -> Float
absAngle a =
    if a < 0 then
        a + 2 * pi |> absAngle

    else
        a



{-
   12時を0とし、時計回りに計算する角度から、3時を0とし、反時計回りに計算する角度に変換する
   radianを受け取ることを前提とする
-}


fromTopBasedAngle : Float -> Float
fromTopBasedAngle float =
    pi / 2 - float |> absAngle


circleRadius : Float
circleRadius =
    100


drawCircle : Model -> List String -> Html Msg
drawCircle model entities =
    case model.commandWindow of
        Nothing ->
            div [] []

        Just c ->
            let
                interval =
                    2 * pi / (entities |> List.length |> toFloat)

                elemAndAngle =
                    List.indexedMap (\i e -> ( toFloat i * interval |> fromTopBasedAngle, e )) entities

                distantX =
                    c.mouse.x - c.center.x

                -- SVGの座標系は上部を0とするため反転させる
                distantY =
                    c.mouse.y - c.center.y |> (*) -1

                angle =
                    atan2 distantY distantX |> absAngle |> Debug.log "angle"

                folder ( angleE, nameE ) ( distantA, nameA, angleA ) =
                    let
                        currentDistant =
                            angleDistant angleE angle
                    in
                    if currentDistant > distantA then
                        ( distantA, nameA, angleA )

                    else
                        ( currentDistant, nameE, angleE )

                ( _, closestName, closestAngle ) =
                    case elemAndAngle of
                        ( headAngle, headName ) :: tailElem ->
                            List.foldr folder
                                ( angleDistant headAngle angle, headName, headAngle )
                                tailElem

                        _ ->
                            ( 0, "no element", 0 )

                lineEndX =
                    cos closestAngle |> (*) circleRadius |> (+) c.center.x

                lineEndY =
                    sin closestAngle |> (*) circleRadius |> negate |> (+) c.center.y
            in
            svg []
                [ circle
                    [ cx <| fromFloat c.center.x
                    , cy <| fromFloat c.center.y
                    , r "100"
                    , fill "gray"
                    ]
                    []
                , text_
                    [ x <| fromFloat c.center.x
                    , y <| fromFloat <| c.center.y + 130
                    ]
                    [ Svg.text closestName ]

                {-
                   , text_
                       [ x <| fromPoint c.centerX
                       , y <| fromPoint <| c.centerY + 150
                       ]
                       [ Svg.text <| "angle: " ++ fromFloat angle3 ]
                -}
                , line
                    [ x1 <| fromFloat c.center.x
                    , y1 <| fromFloat c.center.y
                    , x2 <| fromFloat lineEndX
                    , y2 <| fromFloat lineEndY
                    , strokeWidth <| String.fromInt 10
                    , stroke "black"
                    ]
                    []
                ]


elems =
    [ "A", "B", "C", "D", "E" ]


view model =
    div
        [ Html.Attributes.id "main"
        , onMouseDownWithPoint OpenWindow
        , onMouseUp CloseWindow
        ]
        [ drawCircle model elems ]
