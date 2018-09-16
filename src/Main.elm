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
    = OpenWindow Point Point
    | CloseWindow
    | MousePosition Point Point


type alias Point =
    Float


fromPoint : Point -> String
fromPoint =
    fromFloat


type alias CommandWindow =
    { centerX : Point
    , centerY : Point
    , mouseX : Point
    , mouseY : Point
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
        OpenWindow x y ->
            ( { model | commandWindow = Just <| CommandWindow x y x y }, Cmd.none )

        CloseWindow ->
            ( { model | commandWindow = Nothing }, Cmd.none )

        MousePosition x y ->
            case model.commandWindow of
                Just currentCmd ->
                    ( { model | commandWindow = Just { currentCmd | mouseX = x, mouseY = y } }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


mouseDecoder : (Point -> Point -> a) -> Decoder a
mouseDecoder f =
    map2 f (field "clientX" float) (field "clientY" float)


onMouseDown : (Point -> Point -> msg) -> Html.Attribute msg
onMouseDown f =
    on "mousedown" <| mouseDecoder f



-- 引数は 0 ~ 360 であること


angleDistant : Float -> Float -> Float
angleDistant a b =
    let
        x =
            a - b |> abs

        y =
            a - b + 360 |> abs
    in
    Basics.min x y


drawCircle : Model -> List String -> Html Msg
drawCircle model entities =
    case model.commandWindow of
        Nothing ->
            div [] []

        Just c ->
            let
                interval =
                    360 / (entities |> List.length |> toFloat)

                elemAndAngle =
                    List.indexedMap (\i e -> ( toFloat i * interval, e )) entities

                distantX =
                    c.mouseX - c.centerX

                distantY =
                    c.mouseY - c.centerY |> (*) -1

                angleRad =
                    atan2 distantY distantX

                angle =
                    angleRad |> (*) 180 |> (\x -> x / pi)

                angle2 =
                    90 - angle

                angle3 =
                    if angle2 < 0 then
                        angle2 + 360

                    else
                        angle2

                folder ( angleE, nameE ) ( distantA, nameA, angleA ) =
                    let
                        currentDistant =
                            angleDistant angleE angle3
                    in
                    if currentDistant > distantA then
                        ( distantA, nameA, angleA )

                    else
                        ( currentDistant, nameE, angleE )

                ( _, closestName, closestAngle ) =
                    List.foldr folder ( 400, "dummy", 34 ) elemAndAngle

                closestAngle2 =
                    90 - closestAngle |> degrees

                pointX =
                    cos closestAngle2 |> (*) 100 |> (+) c.centerX

                pointY =
                    sin closestAngle2 |> (*) -100 |> (+) c.centerY
            in
            svg []
                [ circle
                    [ cx <| fromPoint c.centerX
                    , cy <| fromPoint c.centerY
                    , r "100"
                    , fill "gray"
                    ]
                    []
                , text_
                    [ x <| fromPoint c.centerX
                    , y <| fromPoint <| c.centerY + 130
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
                    [ x1 <| fromFloat c.centerX
                    , y1 <| fromFloat c.centerY
                    , x2 <| fromFloat pointX
                    , y2 <| fromFloat pointY
                    , strokeWidth <| String.fromInt 10
                    , stroke "black"
                    ]
                    []
                ]


elems =
    [ "A", "B", "C", "D", "E" ]


view model =
    div [ Html.Attributes.id "main", onMouseDown OpenWindow, onMouseUp CloseWindow ]
        [ drawCircle model elems ]
