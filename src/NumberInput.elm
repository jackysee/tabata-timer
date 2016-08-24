module NumberInput exposing (intInput)
import Html exposing (Html, div, button, input, text)
import Html.Attributes exposing (value, type', class)
import Html.Events exposing ( onClick, onInput )
import String exposing (toInt)

setInt : (Int -> msg) -> String -> msg
setInt msg' input =
  input |> toInt |> Result.withDefault 0 |> msg'


decrease: Int -> Int
decrease val =
  max 1 (val - 1)


intInput : (Int -> msg) -> Int -> Html msg
intInput msg val =
    div [ class "int-input" ]
      [ button [val |> decrease |> msg |> onClick] [ text "-" ]
      , input
        [ onInput (setInt msg)
        , value (toString val)
        , type' "number"
        , class "input-number-no-spin"
        ] []
      , button [(val + 1) |> msg |> onClick] [ text "+" ]
      ]
