--We're gonna use: Buttons, Radio-Buttons, and Text Fields
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

--Define Model
type alias Model =
  { question : String
  , choice1 : String
  , choice2 : String
  , choice3 : String
  , choice4 : String
  , answer : Int
  }

--MODEL (Data)

--Set as Type Model
model : Model
--Instance Variables , Instanciate
model =
  Model "Your question will show here." "A1 goes here." "A2 goes here." "A3 goes here." "A4 goes here." 1

intro : String
intro =  """
CS152 Project
"""

--UPDATE (Change Data)
-- Define Msg
-- Msg is signal to the system
type Msg = 
  Submit 
  | SetQuestion String
  | SetAnswer Int String
  | SetCorrectAnswer Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    Submit -> model
    SetQuestion q -> {model | question = q}
    SetAnswer int ans-> 
      case int of
      --Generate Clone Copy of Model
      1 -> { model | choice1 = ans}
      2 -> { model | choice2 = ans}
      3 -> { model | choice3 = ans}
      4 -> { model | choice4 = ans}
      _ -> { model | choice1 = ans}
    SetCorrectAnswer num -> {model | answer = num}

--VIEW (Print out the data)
view : Model -> Html Msg
view model =
  div []
    [ 
      br [] []
    , input [placeholder "Enter your question here.", onInput SetQuestion] []
    , question "question" "A1" 1
    , question "question" "A2" 2
    , question "question" "A3" 3
    , question "question" "A4" 4
    , button [ onClick Submit ] [ text "Submit" ]
    , br [] [] , br [] []
    , div [] [text model.question]
    , div [] [text model.choice1]
    , div [] [text model.choice2]
    , div [] [text model.choice3]
    , div [] [text model.choice4]
    , br [] []
    , div [] [text (toString model.answer)]
    ]

-- Radio Button
radio : String -> String -> Int -> Html Msg
radio groupName value msg =
  label
    [ style [("padding", "20px")]
    ]
    [ input [ type_ "radio", name groupName, onClick (SetCorrectAnswer msg)] []
    , text value
    ]

-- (Radio Button + Text Box)
-- Takes in String and Int
question : String -> String -> Int -> Html Msg
question groupName name num =
  fieldset []
        [ radio groupName name (num)
        , input [ placeholder "Enter your answer here.", onInput (SetAnswer num)] []
        ]