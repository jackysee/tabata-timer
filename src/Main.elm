port module Main exposing (main)

import Html exposing (Html, div, text, button, span)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (disabled, style, class)
import Time exposing (Time, second)
import NumberInput exposing (intInput)
import String exposing (padLeft)
import Sounds exposing (play, preload, playSounds)


-- APP


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type State
    = Running
    | Stopped
    | Paused


type Stage
    = Prepare
    | Work
    | Rest


type alias Config =
    { prepare : Int
    , rest : Int
    , work : Int
    , cycles : Int
    , tabata : Int
    }


type alias Progress =
    { stage : Stage
    , cycle : Int
    , timeLeft : Int
    , tabata : Int
    , count : Int
    }


type alias Model =
    { total : Int
    , config : Config
    , state : State
    , progress : Progress
    }


init : Maybe Config -> ( Model, Cmd Msg )
init config =
    let
        config_ =
            case config of
                Just config ->
                    config

                Nothing ->
                    { prepare = 10
                    , work = 20
                    , rest = 10
                    , cycles = 8
                    , tabata = 1
                    }

        total_ =
            calculateTotal config_
    in
        ( { total = total_
          , config = config_
          , state = Stopped
          , progress =
                { stage = Prepare
                , cycle = 1
                , timeLeft = config_.prepare
                , tabata = 1
                , count = 1
                }
          }
        , preload Sounds.all
        )


calculateTotal : Config -> Int
calculateTotal config =
    let
        { prepare, work, rest, cycles, tabata } =
            config
    in
        (prepare + (work + rest) * cycles) * tabata


tickProgress : Model -> Progress
tickProgress model =
    let
        count =
            model.progress.count + 1

        { prepare, work, rest, cycles, tabata } =
            model.config

        tabata_duration =
            prepare + (work + rest) * cycles

        tabataNo =
            ceiling (toFloat count / toFloat tabata_duration)

        tabata_progress =
            getProgressCount count tabata_duration

        cycle =
            max 1 (ceiling (toFloat (tabata_progress - prepare) / toFloat (work + rest)))

        is_prepare =
            tabata_progress <= prepare

        cycle_progress =
            getProgressCount (tabata_progress - prepare) (work + rest)
    in
        if is_prepare then
            { stage = Prepare
            , cycle = cycle
            , timeLeft = (prepare - tabata_progress + 1)
            , tabata = tabataNo
            , count = count
            }
        else if cycle_progress <= work then
            { stage = Work
            , cycle = cycle
            , timeLeft = (work - cycle_progress + 1)
            , tabata = tabataNo
            , count = count
            }
        else
            { stage = Rest
            , cycle = cycle
            , timeLeft = (rest - (cycle_progress - work) + 1)
            , tabata = tabataNo
            , count = count
            }


reset : Model -> Model
reset model =
    { model
        | progress =
            { stage = Prepare
            , cycle = 1
            , timeLeft = model.config.prepare
            , tabata = 1
            , count = 1
            }
        , state = Stopped
    }


getProgressCount : Int -> Int -> Int
getProgressCount progress duration =
    let
        remain =
            progress % duration
    in
        if remain == 0 then
            duration
        else
            remain



-- UPDATE


type
    Msg
    --config
    = SetPrepare Int
    | SetRest Int
    | SetWork Int
    | SetCycles Int
    | SetTabata Int
      -- progress
    | Start
    | PauseResume
    | Stop
      -- tick
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        config =
            model.config
    in
        case msg of
            SetPrepare val ->
                updateConfig model { config | prepare = val }

            SetRest val ->
                updateConfig model { config | rest = val }

            SetWork val ->
                updateConfig model { config | work = val }

            SetCycles val ->
                updateConfig model { config | cycles = val }

            SetTabata val ->
                updateConfig model { config | tabata = val }

            Start ->
                let
                    model_ =
                        reset model
                in
                    ( { model_ | state = Running }, play [ Sounds.startSession ] )

            PauseResume ->
                if model.state == Running then
                    ( { model | state = Paused }, play [ Sounds.pauseSession ] )
                else
                    ( { model | state = Running }, play [ Sounds.resumeSession ] )

            Stop ->
                ( reset model, play [ Sounds.stopSession ] )

            Tick t ->
                if model.progress.count < model.total then
                    let
                        progress =
                            tickProgress model

                        model_ =
                            { model | progress = progress }
                    in
                        if progress.stage == Prepare then
                            ( model_
                            , playSounds
                                [ ( progress.timeLeft == 3
                                  , [ Sounds.countdown ]
                                  )
                                , ( progress.timeLeft == model.config.prepare && progress.tabata > 1
                                  , [ Sounds.tabataComplete ]
                                  )
                                ]
                            )
                        else if progress.stage == Work then
                            ( model_
                            , playSounds
                                [ ( progress.timeLeft == model.config.work
                                  , [ Sounds.start
                                    , Sounds.work
                                    ]
                                  )
                                , ( progress.timeLeft == 3
                                  , [ Sounds.countdown ]
                                  )
                                ]
                            )
                        else
                            ( model_
                            , playSounds
                                [ ( progress.timeLeft == model.config.rest
                                  , [ Sounds.end
                                    , Sounds.rest
                                    ]
                                  )
                                , ( progress.timeLeft == 3
                                  , [ Sounds.countdown ]
                                  )
                                ]
                            )
                else if model.progress.count == model.total then
                    ( reset model, play [ Sounds.endSession ] )
                else
                    ( model, Cmd.none )


updateConfig : Model -> Config -> ( Model, Cmd Msg )
updateConfig model config =
    ( { model
        | config = config
        , total = calculateTotal config
      }
    , saveConfig config
    )


view : Model -> Html Msg
view model =
    let
        stopped =
            model.state == Stopped
    in
        div [ class "container " ]
            [ div [ class "section-box" ]
                [ div [ class "section" ]
                    [ renderConfig model.config
                    , div [ class "duration" ]
                        [ div [ class "duration-label " ] [ text "Duration" ]
                        , div [ class "duration-value" ] [ text <| renderAsTime model.total ]
                        ]
                    , div [ class "buttons" ]
                        [ button [ class "btn btn-lg btn-start", onClick Start ] [ text "Start Tabata" ] ]
                    ]
                ]
            , div [ class <| "section-box stage " ++ renderStageClass stopped model.progress.stage ]
                [ div [ class "section stage-section" ]
                    [ div []
                        [ renderStage model.progress model.config ]
                    , div [ class "buttons" ]
                        [ button [ class "btn btn-lg", onClick PauseResume ]
                            [ text
                                (if model.state == Running then
                                    "Pause"
                                 else
                                    "Resume"
                                )
                            ]
                        , button [ class "btn btn-lg", onClick Stop ]
                            [ text "Stop" ]
                        ]
                    ]
                ]
            ]


renderConfig : Config -> Html Msg
renderConfig config =
    div []
        [ makeRow "Prepare" SetPrepare config.prepare
        , makeRow "Rest" SetRest config.rest
        , makeRow "Work" SetWork config.work
        , makeRow "Cycles" SetCycles config.cycles
        , makeRow "Tabatas" SetTabata config.tabata
        ]


makeRow : String -> (Int -> Msg) -> Int -> Html Msg
makeRow label msg model =
    div [ class "config-row" ]
        [ div [ class "config-label" ] [ text label ]
        , div [ class "config-input" ] [ intInput msg model ]
        ]


renderAsTime : Int -> String
renderAsTime seconds =
    (seconds // 60 |> toString |> padLeft 2 '0')
        ++ ":"
        ++ (seconds % 60 |> toString |> padLeft 2 '0')


renderStageClass : Bool -> Stage -> String
renderStageClass stopped stage =
    if stopped then
        "is-hidden is-prepare"
    else
        case stage of
            Prepare ->
                "is-prepare"

            Work ->
                "is-work"

            Rest ->
                "is-rest"


renderStage : Progress -> Config -> Html Msg
renderStage progress config =
    div []
        [ div []
            [ div [ class "duration" ]
                [ div [ class "duration-label" ] [ text <| renderStageLabel progress.stage ]
                , div [ class "duration-time" ] [ text <| renderAsTime progress.timeLeft ]
                ]
            ]
        , div [ class "stage-info-wrap" ]
            [ div [ class "duration stage-info" ]
                [ div [ class "duration-label" ] [ text "Cycle" ]
                , div [ class "duration-value" ]
                    [ span [ class "duration-num" ] [ text <| toString progress.cycle ]
                    , span [ class "duration-denom" ] [ text <| "/" ++ toString config.cycles ]
                    ]
                ]
            , div [ class "duration stage-info" ]
                [ div [ class "duration-label" ] [ text "Tabata " ]
                , div [ class "duration-value" ]
                    [ span [ class "duration-num" ] [ text <| toString progress.tabata ]
                    , span [ class "duration-denom" ] [ text <| "/" ++ toString config.tabata ]
                    ]
                ]
            ]
        ]


renderStageLabel : Stage -> String
renderStageLabel stage =
    case stage of
        Prepare ->
            "Prepare"

        Work ->
            "Work"

        Rest ->
            "Rest"


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.state == Running then
        Time.every second Tick
    else
        Sub.none


port saveConfig : Config -> Cmd msg
