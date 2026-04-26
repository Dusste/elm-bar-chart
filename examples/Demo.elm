module Demo exposing (main)

import Bar.Chart
import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE



-- MODEL


initialWidth : String
initialWidth =
    "200"


type alias Model =
    { count : String, chart : Bar.Chart.Model }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( chart, cmd ) =
            Bar.Chart.init
    in
    ( { count = initialWidth, chart = chart }, Cmd.map ChartMsg cmd )



-- MESSAGES


type Msg
    = ChartMsg Bar.Chart.Msg
    | ChangeContainerWidth String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChartMsg msg_ ->
            let
                ( m, cmd ) =
                    Bar.Chart.update msg_ model.chart

                cmd2 =
                    -- NOTE just for the demo purposes
                    Bar.Chart.measureContainerWidth m
            in
            ( { model | chart = m }, Cmd.batch [ Cmd.map ChartMsg cmd, Cmd.map ChartMsg cmd2 ] )

        ChangeContainerWidth width ->
            ( { model | count = width }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        barData =
            Dict.fromList
                [ ( "Not Started", { value = 10, color = "#a6cee3" } )
                , ( "Pending", { value = 20, color = "#1f78b4" } )
                , ( "In Progress", { value = 30, color = "#b2df8a" } )
                , ( "Under Review", { value = 40, color = "#33a02c" } )
                , ( "Awaiting Action", { value = 50, color = "#fb9a99" } )
                , ( "Completed", { value = 60, color = "#e31a1c" } )
                , ( "Not Applicable", { value = 70, color = "#ff7f00" } )
                ]
    in
    Html.div
        [ HA.class "flex flex-col items-center justify-center my-10 gap-6" ]
        [ Html.h1 [ HA.class "text-2xl font-bold" ] [ Html.text "Bar Chart" ]
        , Html.p [] [ Html.text "Scalable container width - when container width is changed, bars will scale to fit the new width" ]
        , Html.div [] [ Html.input [ HA.type_ "range", HA.min "0", HA.max "1000", HA.value model.count, HA.step "200", HE.onInput ChangeContainerWidth ] [] ]
        , Html.div
            [ HA.class <| "mt-6 w-[" ++ model.count ++ "px]" ]
            [ Html.p [] [ Html.text <| "Width = " ++ model.count ++ "px" ]
            , Bar.Chart.view
                ChartMsg
                barData
                model.chart
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Bar.Chart.subscriptions model.chart |> Sub.map ChartMsg



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
