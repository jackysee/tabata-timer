port module Config exposing (Model, Msg, view, update, saveConfig)
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import NumberInput exposing (intInput)

--Model
type alias Model =
  { prepare: Int
  , rest: Int
  , work: Int
  , cycles: Int
  , tabata: Int
  }


-- initModel =
--   { prepare = 10
--   , work = 20
--   , rest = 10
--   , cycles = 8
--   , tabata = 4
--   }

type Msg
  = SetPrepare Int
  | SetRest Int
  | SetWork Int
  | SetCycles Int
  | SetTabata Int


update : Msg -> Model -> Model
update msg model =
    case msg of
      SetPrepare val -> {model | prepare = val}
      SetRest val -> {model | rest = val}
      SetWork val -> {model | work = val}
      SetCycles val -> {model | cycles = val}
      SetTabata val -> {model | tabata = val}


view: Model -> Html Msg
view model =
  div []
  [ makeRow "Prepare" SetPrepare model.prepare
  , makeRow "Rest" SetRest model.rest
  , makeRow "Work" SetWork model.work
  , makeRow "Cycles" SetCycles model.cycles
  , makeRow "Tabatas" SetTabata model.tabata
  ]

makeRow: String -> (Int -> Msg) -> Int -> Html Msg
makeRow label msg model =
  div [class "config-row"]
    [ div [class "config-label"] [text label]
    , div [class "config-input"] [intInput msg model]
    ]


port saveConfig: Model -> Cmd msg
