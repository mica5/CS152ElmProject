--We're gonna use: Buttons, Radio-Buttons, and Text Fields
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

--Define Model. Model is similar to a struct in C, or an object in JS, but it is immutable. Elm gets around this by
--Generating a new copy of that object but which shares memory with the original record, except for the changed values.
type alias Model =
  { question : String
  --Questioner's POV
  --String values for questions
  , questionerChoice1 : String
  , questionerChoice2 : String
  , questionerChoice3 : String
  , questionerChoice4 : String
  --answer index (1 for first answer, x for xth answer)
  , answerIndex : Int
  --Answerer's POV
  }

--MODEL (Data)
--Set as Type Model
model : Model
--Instance Variables , Instantiate
model =
  Model "Your question will show here." "A1 goes here." "A2 goes here." "A3 goes here." "A4 goes here." 1

intro : String
intro =  """
CS152 Project
"""

--UPDATE (Change Data)
-- Define Msg
-- Msg is signal to the system, we use case matching to allow the view section to give a particular signal
type Msg = 
  Submit --Submits the current model values and updates it for the answerer's POV
  | SetQuestion String --sets a new question string
  | SetAnswer Int String --sets a new answer string
  | SetCorrectAnswer Int --sets a new answer index (1 for first answer, x for xth answer)






--Update will take a message signal based on what kind of messgae the view section has given
--The common property with all of them is that they may pass parameters of their own, and they replace the model data with
--an updated copy of itself.
update : Msg -> Model -> Model
--Now we're defining the function call for update. We're naming the first two parameters msg and model.
--Note that msg is a type Msg which is given several types as shown above, and that is handy for case matching
--Then, all that is left is what kind of changes to the model are made.
update msg model =
  case msg of
    Submit -> model
    SetQuestion q -> {model | question = q}
    SetAnswer int ans-> 
      case int of
      --Generate Updated Clone Copy of Model, this is Elm's equivalent to using a setter for an object
      1 -> { model | questionerChoice1 = ans}
      2 -> { model | questionerChoice2 = ans}
      3 -> { model | questionerChoice3 = ans}
      4 -> { model | questionerChoice4 = ans}
      _ -> { model | questionerChoice1 = ans} --We're setting choice 1 as the default choice if anything goes wrong
    SetCorrectAnswer num -> {model | answerIndex = num}






{-VIEW (Print out the data)
View's job is to interpret the model every time it is updated. We will take the date from model and display it in GUI
View's second job is to provide controls to the user to update the model. This is done through text fields and buttons
of all kinds. They will take input, and then pass a message to update. It is up for us to decide what parameters are
necessary and how it works. -}
view : Model -> Html Msg
view model =
  div [] --View is literally one giant hierarchy of HTML. The first [] is for attributes, and the second [] is for content.
    [ 
      br [] [] --We use an empty br to break down into a new line, or provide spacing.
    , fieldset [] 
        [
        input [placeholder "Enter your question here.", onInput SetQuestion] []
        , br [][], br [][], div [][text "Set one answer as the correct answer."], br [][]
        , question "question" "A1" 1
        , br [][]
        , question "question" "A2" 2
        , br [][]
        , question "question" "A3" 3
        , br [][]
        , question "question" "A4" 4
        , br [][]
        , button [ onClick Submit ] [ text "Submit" ]
        ]
    , br [] [] , br [] []
    , fieldset []
        [
        div [] [text model.question]
        , div [] [text model.questionerChoice1]
        , div [] [text model.questionerChoice2]
        , div [] [text model.questionerChoice3]
        , div [] [text model.questionerChoice4]
        , div [] [text ("The current answer is " ++ (toString model.answerIndex) ++ ".")]
        ]
    , br [] []
    ]

-- (Radio Button + Text Box)
{- Radio Buttons use groupNames to exclude other button in the same group, textValue to provide a text paired up with it,
and newAnswerIndex which takes an int for the new answer index-}
-- Takes in groupName to pass into radio, takes in a textValue to assign that button, newAnswerIndex is the new answer
question : String -> String -> Int -> Html Msg
question groupName textValue newAnswerIndex =
  div []
        [ 
          --beginning of radio button
          label
           [ style [("padding", "20px")]]
           [ input [ type_ "radio", name groupName, onClick (SetCorrectAnswer newAnswerIndex)] [], text textValue]
          --ending of radio button
        , input [ placeholder "Enter your answer here.", onInput (SetAnswer newAnswerIndex)] []
        ]