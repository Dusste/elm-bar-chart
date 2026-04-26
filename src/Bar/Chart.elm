module Bar.Chart exposing (Model, Msg, init, measureContainerWidth, subscriptions, update, view)

import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Process
import Svg
import Svg.Attributes as SA
import Task
import Time



-- MODEL


type alias Model =
    { showLegend : Bool
    , highlightedLegendItem : Maybe String
    , containerWidth : Maybe Float
    , containerIdSeed : Maybe Int
    }


type Msg
    = ShowLegend
    | HighlightLegendItem String
    | GotContainerIdSeed Int
    | MeasureContainer
    | GotContainerElement (Result Browser.Dom.Error Browser.Dom.Element)
    | WindowResized Int Int


chartHeight : Int
chartHeight =
    200


axisOffsetX : Int
axisOffsetX =
    20


chartContainerIdPrefix : String
chartContainerIdPrefix =
    "bar-chart-container-"


containerId : Model -> String
containerId model =
    chartContainerIdPrefix
        ++ (model.containerIdSeed
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "pending"
           )


barSpacing : Float
barSpacing =
    10


barAreaStartX : Float
barAreaStartX =
    30


init : ( Model, Cmd Msg )
init =
    ( { showLegend = False
      , highlightedLegendItem = Nothing
      , containerWidth = Nothing
      , containerIdSeed = Nothing
      }
    , Task.perform (Time.posixToMillis >> GotContainerIdSeed) Time.now
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize WindowResized


type alias BarChartData =
    Dict String { value : Int, color : String }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowLegend ->
            ( { model | showLegend = model.showLegend |> not }
            , Cmd.none
            )

        HighlightLegendItem label ->
            ( { model | highlightedLegendItem = Just label }
            , Cmd.none
            )

        GotContainerIdSeed seed ->
            ( { model | containerIdSeed = Just seed }
            , Task.perform (\_ -> MeasureContainer) (Process.sleep 0)
            )

        MeasureContainer ->
            ( model, measureContainerWidth model )

        GotContainerElement res ->
            case res of
                Ok element ->
                    ( { model | containerWidth = Just element.element.width }
                    , Cmd.none
                    )

                Err _ ->
                    if model.containerWidth == Nothing then
                        ( model, Task.perform (\_ -> MeasureContainer) (Process.sleep 50) )

                    else
                        ( model, Cmd.none )

        WindowResized _ _ ->
            ( model, Task.perform (\_ -> MeasureContainer) (Process.sleep 0) )



-- VIEW


view : (Msg -> msg) -> BarChartData -> Model -> Html msg
view toSelf status model =
    let
        barData =
            toBars status

        barCount =
            List.length barData

        barWidth : Float
        barWidth =
            -- calculate bar width based on a available space
            let
                availableSpace =
                    model.containerWidth
                        |> Maybe.map (\width -> width - (barAreaStartX - 10))
                        |> Maybe.withDefault 0
                        |> max 0

                availableBars =
                    toFloat barCount

                w =
                    if availableBars == 0 then
                        0

                    else
                        (availableSpace / availableBars) - barSpacing
            in
            if barCount <= 3 then
                30

            else
                w

        chartWidth : Float
        chartWidth =
            toFloat barCount * barWidth + toFloat (barCount - 1) * barSpacing

        svgTotalWidth : Float
        svgTotalWidth =
            barAreaStartX + chartWidth + 10

        maxValue =
            List.maximum (List.map (\( _, v, _ ) -> v) barData)
                |> Maybe.withDefault 1

        scaleMax =
            roundUpToNice maxValue

        ticks =
            tickLabels scaleMax

        stepHeight =
            toFloat chartHeight / toFloat scaleMax
    in
    Html.div
        [ HA.id (containerId model)
        , HA.class "flex items-center relative w-full min-w-0"
        ]
        [ Svg.svg
            [ SA.width "100%"
            , SA.height (String.fromInt (chartHeight + 20))
            , HE.onMouseEnter (toSelf ShowLegend)
            , HE.onMouseLeave (toSelf ShowLegend)
            ]
            [ Svg.g
                [ SA.transform "translate(0,10)"
                ]
                (List.concat
                    [ -- Y-axis
                      [ Svg.line
                            [ SA.x1 (String.fromInt axisOffsetX)
                            , SA.y1 "0"
                            , SA.x2 (String.fromInt axisOffsetX)
                            , SA.y2 (String.fromInt chartHeight)
                            , SA.stroke "#6b7280"
                            , SA.strokeWidth "1"
                            ]
                            []
                      ]
                    , -- Tick lines and labels
                      List.concatMap
                        (\tick ->
                            let
                                y =
                                    toFloat chartHeight - (toFloat tick * stepHeight)
                            in
                            [ Svg.text_
                                [ SA.x (String.fromInt (axisOffsetX - 5))
                                , SA.y (String.fromFloat y)
                                , SA.textAnchor "end"
                                , SA.fontSize "10"
                                , SA.dy "3"
                                , SA.fill "#444"
                                ]
                                [ Svg.text (String.fromInt tick) ]
                            , Svg.line
                                [ SA.x1 (String.fromInt axisOffsetX)
                                , SA.y1 (String.fromFloat y)
                                , SA.x2 (String.fromFloat svgTotalWidth)
                                , SA.y2 (String.fromFloat y)
                                , SA.stroke "#e5e7eb"
                                , SA.strokeWidth "1"
                                ]
                                []
                            ]
                        )
                        ticks
                    , -- Bars
                      List.indexedMap
                        (\i ( label, value, color ) ->
                            let
                                heightRatio =
                                    toFloat value / toFloat scaleMax

                                barHeight =
                                    heightRatio * toFloat chartHeight

                                x =
                                    barAreaStartX + toFloat i * (barWidth + barSpacing)

                                y =
                                    toFloat chartHeight - barHeight
                            in
                            Svg.rect
                                [ SA.x (String.fromFloat x)
                                , SA.y (String.fromFloat y)
                                , SA.width (String.fromFloat barWidth)
                                , SA.height (String.fromFloat barHeight)
                                , SA.fill color
                                , HE.onMouseEnter (toSelf <| HighlightLegendItem label)
                                ]
                                []
                        )
                        barData
                    ]
                )
            ]
        , viewLegend toSelf barData model
        ]


viewLegend : (Msg -> msg) -> List ( String, Int, String ) -> Model -> Html msg
viewLegend toSelf entries model =
    Html.div
        [ HA.class <|
            "space-y-1 px-2 py-4 bg-gray-50 border border-gray-200 rounded-md absolute top-[-10px] left-[100%] z-[2] "
                ++ (if model.showLegend then
                        "block"

                    else
                        "hidden"
                   )
        , HE.onMouseEnter (toSelf ShowLegend)
        , HE.onMouseLeave (toSelf ShowLegend)
        ]
        (List.map
            (\( label, value, color ) ->
                viewLegendItem label value color model
            )
            entries
        )


viewLegendItem : String -> Int -> String -> Model -> Html msg
viewLegendItem label value color model =
    Html.div
        [ HA.class <|
            "flex items-center space-x-2 "
                ++ (if model.highlightedLegendItem == Just label then
                        "bg-sky-100 text-sky-600"

                    else
                        "bg-gray-50 text-gray-500"
                   )
        ]
        [ Html.div
            [ HA.class "w-[10px] h-[10px] rounded-sm flex-shrink-0"
            , HA.style "background-color" color
            ]
            []
        , Html.span
            [ HA.class <|
                "text-xs whitespace-nowrap "

            -- ++ (if model.highlightedLegendItem == Just label then
            --         "text-sky-600"
            --     else
            --         "text-gray-500"
            --    )
            ]
            [ Html.text label ]
        , Html.span
            [ HA.class "text-xs whitespace-nowrap" ]
            [ Html.text (String.fromInt value) ]
        ]


measureContainerWidth : Model -> Cmd Msg
measureContainerWidth model =
    Browser.Dom.getElement (containerId model)
        |> Task.attempt GotContainerElement


toBars : BarChartData -> List ( String, Int, String )
toBars status =
    List.map
        (\( label, { value, color } ) -> ( label, value, color ))
        (Dict.toList status)


roundUpToNice : Int -> Int
roundUpToNice maxVal =
    if maxVal <= 5 then
        5

    else if maxVal <= 10 then
        10

    else if maxVal <= 15 then
        15

    else if maxVal <= 20 then
        20

    else
        let
            magnitude =
                10 ^ (String.length (String.fromInt maxVal) - 1)

            base =
                maxVal // magnitude
        in
        (base + 1) * magnitude


tickLabels : Int -> List Int
tickLabels max =
    case max of
        5 ->
            [ 0, 5 ]

        10 ->
            [ 0, 5, 10 ]

        15 ->
            [ 0, 5, 10, 15 ]

        20 ->
            [ 0, 10, 20 ]

        25 ->
            [ 0, 10, 20, 25 ]

        30 ->
            [ 0, 10, 20, 30 ]

        50 ->
            [ 0, 25, 50 ]

        100 ->
            [ 0, 50, 100 ]

        _ ->
            [ 0, max // 2, max ]
