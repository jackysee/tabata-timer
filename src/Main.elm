
import Html exposing (Html, div, text, button, span)
import Html.App as App
import Html.Events exposing ( onClick, onInput )
import Html.Attributes exposing ( disabled, style, class )
import Time exposing (Time, second)
import Result
import Config
import String exposing (padLeft)
import Sounds exposing (play, preload, playSounds)

-- APP
main =
  App.programWithFlags
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

type alias Progress =
  { stage: Stage
  , cycle: Int
  , timeLeft: Int
  , tabata: Int
  , count: Int
  }

type alias Model =
  { total : Int
  , config : Config.Model
  , state : State
  , progress: Progress
  }


init: Maybe Config.Model -> (Model, Cmd Msg)
init config =
  let
    config' =
      case config of
          Just config -> config
          Nothing -> { prepare = 5
                     , work = 6
                     , rest = 6
                     , cycles = 2
                     , tabata = 2
                     }
    total' =  calculateTotal config'
  in (
    { total = total'
    , config = config'
    , state = Stopped
    , progress =
      { stage = Prepare
      , cycle = 1
      , timeLeft = config'.prepare
      , tabata = 1
      , count = 1
      }
    }
    , preload Sounds.all
  )


calculateTotal: Config.Model -> Int
calculateTotal config =
  let {prepare, work, rest, cycles, tabata} = config in
    (prepare + (work + rest) * cycles) * tabata


tickProgress: Model -> Progress
tickProgress model =
  let
    count = model.progress.count + 1
    {prepare, work, rest, cycles, tabata} = model.config
    tabata_duration = prepare + (work + rest)*cycles
    tabataNo = ceiling (toFloat count/ toFloat tabata_duration)
    tabata_progress = getProgressCount count tabata_duration
    cycle = max 1 (ceiling (toFloat (tabata_progress - prepare) / toFloat (work + rest)))
    is_prepare = tabata_progress <= prepare
    cycle_progress = getProgressCount (tabata_progress - prepare) (work + rest)
  in
    if is_prepare then
      { stage = Prepare
      , cycle = cycle
      , timeLeft = (prepare - tabata_progress + 1)
      , tabata = tabataNo
      , count = count
      }
    else if cycle_progress <= work  then
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


reset: Model -> Model
reset model =
  { model | progress =
              { stage = Prepare
              , cycle = 1
              , timeLeft = model.config.prepare
              , tabata = 1
              , count = 1
              }
          , state = Stopped
  }


getProgressCount: Int -> Int -> Int
getProgressCount progress duration =
    let remain = progress % duration in
      if remain == 0
        then duration
      else
        remain


-- UPDATE
type Msg
  = ConfigMsg Config.Msg
  | Start
  | PauseResume
  | Stop
  | Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ConfigMsg subMsg ->
      let config' = Config.update subMsg model.config in
        ( { model | config = config'
                  , total = calculateTotal config'
          }
        , Config.saveConfig config'
        )
    Start ->
      let model' = reset model in
        ( { model' | state = Running }, play [ Sounds.startSession ] )
    PauseResume ->
      if model.state == Running then
        ( { model | state = Paused }, play [ Sounds.pauseSession ] )
      else
        ( { model | state = Running }, play [ Sounds.resumeSession ] )
    Stop ->
      ( reset model , play [Sounds.stopSession] )
    Tick t ->
      if model.progress.count < model.total then
        let
          progress = tickProgress model
          model' = { model | progress = progress }
        in
          if progress.stage == Prepare then
              ( model'
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
              ( model'
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
              ( model'
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
        ( reset model, play [Sounds.endSession] )
      else
        ( model, Cmd.none )


view : Model -> Html Msg
view model =
  let
    stopped = model.state == Stopped
  in
    div [class "container "]
      [ div [ class "section-box" ]
          [ div [ class "section" ]
            [ App.map ConfigMsg (Config.view model.config)
            , div [ class "duration" ]
                [ div [class "duration-label "] [ text "Duration" ]
                , div [class "duration-value" ] [ text <| renderAsTime model.total  ]
                ]
            , div [ class "buttons" ]
                [ button [ class "btn btn-lg btn-start" , onClick Start ] [ text "Start Tabata" ] ]
            ]
          ]
      , div [ class <| "section-box stage " ++  renderStageClass stopped model.progress.stage ]
          [ div [ class "section" ]
            [ div []
                [ renderStage model.progress model.config ]
            , div [ class "buttons" ]
                  [ button [ class "btn btn-lg", onClick PauseResume ]
                      [ text (if model.state == Running then "Pause" else "Resume") ]
                  , button [ class "btn btn-lg", onClick Stop ]
                      [ text "Stop" ]
                  ]
            ]
          ]
      ]


renderAsTime: Int -> String
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
      Prepare -> "is-prepare"
      Work -> "is-work"
      Rest -> "is-rest"


renderStage: Progress -> Config.Model -> Html Msg
renderStage progress config =
  div []
    [ div []
        [ div [class "duration"]
            [ div [ class "duration-label" ] [ text <| renderStageLabel progress.stage ]
            , div [ class "duration-time"] [ text <| renderAsTime progress.timeLeft ]
            ]
        ]
    , div [ class "stage-info-wrap"]
      [ div [class "duration stage-info"]
          [ div [ class "duration-label" ] [ text "Cycle"]
          , div [ class "duration-value"]
              [ span [ class "duration-num" ] [ text <| toString progress.cycle ]
              , span [ class "duration-denom" ] [ text <| "/" ++ toString config.cycles ]
              ]
          ]
      , div [class "duration stage-info"]
          [ div [ class "duration-label" ] [ text "Tabata "]
          , div [ class "duration-value"]
              [ span [ class "duration-num" ] [ text <| toString progress.tabata ]
              , span [ class "duration-denom" ] [ text <| "/" ++ toString config.tabata ]
              ]
          ]
      ]
    ]


renderStageLabel: Stage -> String
renderStageLabel stage =
  case stage of
    Prepare -> "Prepare"
    Work -> "Work"
    Rest -> "Rest"


renderDuration: String -> String -> Html Msg
renderDuration label value =
    div [class "duration"]
     [ div [ class "duration-label" ] [ text label ]
     , div [ class "duration-value"] [ text value ]
     ]


subscriptions: Model -> Sub Msg
subscriptions model =
  if model.state == Running then
    Time.every second Tick
  else
    Sub.none
