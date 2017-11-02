module Main exposing (..)

import Time
import Random
import Random.Extra
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


-- model


type alias Colour =
    String


type alias Model =
    { colours : List Colour
    , active : Int
    , inProgress : Bool
    , real : Colour
    , fake : Colour
    , hasWon : Bool
    , hasStarted : Bool
    , score : Int
    , timeLeft : Int
    , speed : Float
    }


initModel : Model
initModel =
    { colours =
        [ "red"
        , "green"
        , "blue"
        , "black"
        , "gray"
        , "orange"
        , "pink"
        , "aqua"
        , "gold"
        , "yellow"
        , "olive"
        , "navy"
        , "silver"
        , "lime"
        , "brown"
        , "purple"
        ]
    , active = 6
    , real = ""
    , fake = ""
    , score = 0
    , hasStarted = False
    , hasWon = False
    , inProgress = False
    , speed = 1000
    , timeLeft = 3
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, updateGameColours initModel )



-- update


type Msg
    = UpdateGameColours ( Colour, Colour )
    | TriggerUpdateColours
    | ChooseColour Colour
    | Tick
    | StartPlaying
    | RestartGame


randomColour : Model -> Random.Generator Colour
randomColour model =
    Random.Extra.sample (List.take model.active model.colours)
        |> Random.map (Maybe.withDefault "")


changeGameColours : Model -> Random.Generator ( Colour, Colour )
changeGameColours model =
    Random.pair (randomColour model) (randomColour model)


updateGameColours : Model -> Cmd Msg
updateGameColours model =
    Random.generate UpdateGameColours (changeGameColours model)


increaseNumberOfColours : Model -> Model
increaseNumberOfColours model =
    { model | active = model.active + 2 }


increaseSpeed : Model -> Model
increaseSpeed model =
    { model | speed = model.speed - 200 }


levelUpChecker : Model -> Int -> (Model -> Model) -> Model
levelUpChecker model mod levelUpFn =
    if model.score % mod == 0 then
        levelUpFn model
    else
        model


checkForLevelUps : Model -> Model
checkForLevelUps model =
    let
        activeColoursCount =
            List.take model.active model.colours
                |> List.length
    in
        if activeColoursCount >= List.length model.colours then
            winGame model
        else
            List.foldr
                (\( mod, fn ) m -> levelUpChecker m mod fn)
                model
                [ ( 3, increaseNumberOfColours )
                , ( 4, increaseSpeed )
                ]


progressGame : Model -> Model
progressGame model =
    { model
        | score = model.score + 1
        , timeLeft = model.timeLeft + 2
    }
        |> checkForLevelUps


endGame : Model -> Model
endGame model =
    { model | inProgress = False, timeLeft = 0 }


winGame : Model -> Model
winGame model =
    { model | hasWon = True }
        |> endGame


processModelOnAnswer : Bool -> Model -> Model
processModelOnAnswer isCorrect model =
    if isCorrect then
        progressGame model
    else
        endGame model


processCommandOnAnswer : Bool -> Model -> Cmd Msg
processCommandOnAnswer isCorrect model =
    if isCorrect then
        updateGameColours model
    else
        Cmd.none


startGame : Model -> ( Model, Cmd Msg )
startGame model =
    ( { model
        | hasStarted = True
        , inProgress = True
      }
    , updateGameColours model
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartPlaying ->
            startGame model

        UpdateGameColours ( real, fake ) ->
            ( { model | real = real, fake = fake }, Cmd.none )

        TriggerUpdateColours ->
            ( model, updateGameColours model )

        Tick ->
            let
                newModel =
                    if model.timeLeft > 1 then
                        { model | timeLeft = model.timeLeft - 1 }
                    else
                        endGame model
            in
                ( newModel, Cmd.none )

        ChooseColour colour ->
            if model.inProgress then
                let
                    isCorrect =
                        colour == model.real
                in
                    ( processModelOnAnswer isCorrect model
                    , processCommandOnAnswer isCorrect model
                    )
            else
                ( model, Cmd.none )

        RestartGame ->
            startGame initModel



-- view


renderGameColour : Model -> Html Msg
renderGameColour model =
    let
        containerStyles =
            [ ( "color", model.fake )
            , ( "font-size", "18px" )
            , ( "font-weight", "700" )
            , ( "margin", "50px" )
            , ( "text-transform", "uppercase" )
            ]
    in
        div
            [ class "text-center"
            , style containerStyles
            ]
            [ text model.real ]


renderGameInfo : Model -> Html Msg
renderGameInfo model =
    let
        smallStyles =
            [ ( "display", "block" )
            , ( "color", "gray" )
            ]
    in
        div []
            [ p []
                [ text ("Score: " ++ (toString model.score))
                , small [ style smallStyles ] [ text ("speed: " ++ (toString model.speed)) ]
                ]
            ]


renderColour : Colour -> Html Msg
renderColour colour =
    let
        buttonColours =
            [ ( "background-color", colour )
            , ( "color", "white" )
            , ( "cursor", "pointer" )
            ]
    in
        li []
            [ button
                [ style buttonColours
                , class "btn btn-xs"
                , onClick (ChooseColour colour)
                ]
                [ text colour ]
            ]


renderActiveColours : Model -> Html Msg
renderActiveColours model =
    List.take model.active model.colours
        |> List.map renderColour
        |> ul [ class "list-inline" ]


renderTimeLeft : Int -> Html Msg
renderTimeLeft time =
    p [ style [ ( "font-size", "16px" ) ] ]
        [ i [ class "glyphicon glyphicon-time" ] []
        , text " "
        , text (toString time)
        ]


renderIntro : Html Msg
renderIntro =
    let
        containerStyles =
            [ ( "background-color", "transparent" ), ( "margin-top", "180px" ) ]

        buttonStyles =
            [ ( "margin-top", "40px" )
            , ( "letter-spacing", "2" )
            , ( "font-weight", "300" )
            ]
    in
        div
            [ class "text-center jumbotron"
            , style containerStyles
            ]
            [ h2 [] [ text "Welcome to this awesome game" ]
            , div [] [ text "Your objective is simple: select the colour it says you should." ]
            , div []
                [ strong [] [ text "Remember" ]
                , text " you have just a few seconds to do so"
                , br [] []
                , text "Time is ticking and ticking, and ticking..."
                ]
            , div [ class "row" ]
                [ div [ class "col-xs-4 col-xs-offset-4 text-center" ]
                    [ button
                        [ class "btn btn-md btn-danger btn-block"
                        , onClick StartPlaying
                        , style buttonStyles
                        ]
                        [ text "Start" ]
                    ]
                ]
            ]


renderGameEnded : Bool -> Html Msg
renderGameEnded hasWon =
    let
        message =
            if hasWon then
                "Congratulations! You won the game."
            else
                "Oops... :( Better luck next time!"
    in
        div []
            [ p [] [ text message ]
            , button
                [ class "btn btn-danger btn-sm"
                , onClick RestartGame
                ]
                [ text "Restart game" ]
            ]


renderGame : Model -> Html Msg
renderGame model =
    let
        footer =
            if model.inProgress then
                renderTimeLeft model.timeLeft
            else
                renderGameEnded model.hasWon
    in
        div [ class "container text-center" ]
            [ h3 [ style [ ( "font-weight", "200" ) ] ]
                [ text "Riddle me this - what is my colour?" ]
            , renderActiveColours model
            , hr [] []
            , renderGameColour model
            , renderGameInfo model
            , footer
            ]


view : Model -> Html Msg
view model =
    if model.hasStarted then
        renderGame model
    else
        renderIntro



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.inProgress then
        Time.every (model.speed * Time.millisecond) (always Tick)
    else
        Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
