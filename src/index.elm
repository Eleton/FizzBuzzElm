import Browser
import Html exposing (Html, button, div, span, text, input)
import Html.Events exposing (onInput)
import Html.Attributes exposing (..)

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model = { number: Int, fizz: Int, buzz: Int}

init : Model
init =
  Model 100 3 5

-- UPDATE

type Msg
  = Increment
  | Decrement
  | ChangeNumber String
  | ChangeFizz String
  | ChangeBuzz String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model

    Decrement ->
      model

    ChangeNumber text ->
      Debug.log (Debug.toString text)
      { model | number = (Maybe.withDefault 100 (String.toInt text)) }

    ChangeFizz text ->
      Debug.log (Debug.toString text)
      { model | fizz = (Maybe.withDefault 3 (String.toInt text)) }

    ChangeBuzz text ->
      Debug.log (Debug.toString text)
      { model | buzz = (Maybe.withDefault 5 (String.toInt text)) }


-- VIEW

view : Model -> Html Msg
view model =
  div [ class "content" ]
  [ div [ class "center" ]
    [ div [ class "inputContainer" ]
      [ span [] [ text "Amount: "]
      , input
        [ placeholder "Number"
        , type_ "number"
        , value (String.fromInt model.number)
        , onInput ChangeNumber
        ] []
      , span [] [ text "Fizz: "]
      , input
        [ placeholder "Fizz"
        , type_ "number"
        , value (String.fromInt model.fizz)
        , onInput ChangeFizz
        ] []
      , span [] [ text "Buzz: "]
      , input
        [ placeholder "Buzz"
        , type_ "number"
        , value (String.fromInt model.buzz)
        , onInput ChangeBuzz
        ] []
      ]
    ]
  , div [ class "center" ]
    [ div [ class "gridContainer" ]
      (List.map (\s -> div [] [text s]) (List.reverse (fizzbuzz model.number model.fizz model.buzz)))
    ]
  ] 

-- HELPER FUNCTIONS
fizzbuzz : Int -> Int -> Int -> List String
fizzbuzz num fizz buzz =
  if num == 0 then []
  else if (modBy fizz num) == 0 && (modBy buzz num) == 0 then
    "FizzBuzz" :: (fizzbuzz (num - 1) fizz buzz)
  else if (modBy fizz num) == 0 then
    "Fizz" :: (fizzbuzz (num - 1) fizz buzz)
  else if (modBy buzz num) == 0 then
    "Buzz" :: (fizzbuzz (num - 1) fizz buzz)
  else
    String.fromInt(num) :: (fizzbuzz (num - 1) fizz buzz)