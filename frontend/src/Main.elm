port module Main exposing (main)

import Array
import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Keyboard.Event exposing (KeyboardEvent)
import Keyboard.Key
import List.Extra
import Task
import Time


port sendCode : String -> Cmd msg


port gotResult : (Maybe String -> msg) -> Sub msg


type alias Model =
    { cells : Dict Int Cell
    , activeCellIndex : Int
    , pendingKeyAction : Maybe Keyboard.Key.Key
    , orientation : Orientation
    , popUp : PopUp
    , toolTip : ToolTip
    , colors : Colors
    , theme : Theme
    , useLigatures : Bool
    , fontSize : Int
    }


type Msg
    = AddCell
    | EditCell String
    | HandleKeyDown KeyboardEvent
    | GotCellCursorRow Int
    | HandleOrientation Int
    | SetPopUp PopUp
    | SetToolTip ToolTip
    | CloseActiveCell
    | SetTheme Theme
    | SetUseLigatures Bool
    | SetFontSize Int
    | GotResult (Maybe String)
    | NoOp


type alias Cell =
    ( String, Maybe String )


type Orientation
    = Portrait
    | Landscape


type PopUp
    = HelpPopUp
    | SettingsPopUp
    | NoPopUp


type ToolTip
    = SavedToolTip
    | SettingsToolTip
    | HelpToolTip
    | NoToolTip


type Theme
    = Light
    | SolarizedLight
    | Dark


type alias Colors =
    { lightFg :
        E.Color
    , darkFg :
        E.Color
    , lightBg :
        E.Color
    , darkBg :
        E.Color
    }


styles =
    { title =
        [ Font.bold
        , E.htmlAttribute <| Html.Attributes.style "font-size" "125%"
        ]
    , subtitle =
        [ E.paddingXY 0 10
        , Font.bold
        , E.htmlAttribute <| Html.Attributes.style "font-size" "110%"
        ]
    , popUp =
        [ E.centerX
        , E.centerY
        , E.padding 30
        , E.spacing 10
        , Border.rounded 10
        , E.inFront viewClosePopUpButton
        ]
    }


main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Maybe String -> ( Model, Cmd Msg )
init _ =
    let
        initialModel =
            { cells =
                Dict.singleton 0 emptyCell
            , activeCellIndex =
                0
            , pendingKeyAction =
                Nothing
            , orientation =
                Landscape
            , popUp =
                NoPopUp
            , toolTip =
                NoToolTip
            , colors =
                getThemeColors Light
            , theme =
                Light
            , useLigatures =
                True
            , fontSize =
                16
            }
    in
    ( initialModel
    , Task.perform (HandleOrientation << round << .width << .viewport) Browser.Dom.getViewport
    )


emptyCell : Cell
emptyCell =
    ( "", Nothing )


view : Model -> Html Msg
view model =
    E.layout
        ([ Font.family
            [ Font.typeface "Fira Code"
            , Font.monospace
            ]
         , E.htmlAttribute <|
            Html.Attributes.style "font-variant-ligatures" <|
                if model.useLigatures then
                    "normal"

                else
                    "none"
         , E.inFront <| viewToolButtons model
         , E.inFront <| viewPopUp model
         , Font.size <| scale model.orientation model.fontSize
         , Font.color model.colors.darkFg
         , Background.color model.colors.lightBg
         ]
            ++ (case model.orientation of
                    Landscape ->
                        [ E.padding 30
                        ]

                    Portrait ->
                        [ E.paddingEach
                            { left =
                                10
                            , right =
                                10
                            , top =
                                45
                            , bottom =
                                10
                            }
                        ]
               )
        )
    <|
        E.column
            [ E.spacing 15
            , E.width (E.fill |> E.maximum 700)
            , E.htmlAttribute <| Html.Attributes.style "margin" "auto"
            , E.htmlAttribute <| Html.Attributes.style "z-index" "0"
            , E.alignTop
            ]
        <|
            List.indexedMap
                (\index result ->
                    viewCell model.activeCellIndex index result model
                )
                (Dict.values model.cells)


viewPopUp : Model -> E.Element Msg
viewPopUp model =
    case model.popUp of
        HelpPopUp ->
            viewHelpPopUp model

        SettingsPopUp ->
            viewSettingsPopUp model

        NoPopUp ->
            E.none


viewSettingsPopUp : Model -> E.Element Msg
viewSettingsPopUp model =
    E.column
        (styles.popUp ++ [ Background.color model.colors.darkBg ])
        [ E.el styles.subtitle <| E.text "Color Theme"
        , Input.radio
            [ E.padding 10
            , E.spacing 20
            ]
            { onChange = SetTheme
            , selected = Just model.theme
            , label = Input.labelHidden "Color Theme"
            , options =
                [ Input.option Light (E.text "Light")
                , Input.option SolarizedLight (E.text "Solarized Light")
                , Input.option Dark (E.text "Dark")
                ]
            }
        , E.el styles.subtitle <| E.text "Ligatures"
        , Input.radioRow
            [ E.padding 10
            , E.spacing 20
            ]
            { onChange = SetUseLigatures
            , selected = Just model.useLigatures
            , label = Input.labelHidden "Ligatures"
            , options =
                [ Input.option True (E.text "On")
                , Input.option False (E.text "Off")
                ]
            }
        , E.el styles.subtitle <| E.text "Font Size"
        , Input.slider
            [ E.height (E.px 20)
            , E.behindContent
                (E.el
                    [ E.width E.fill
                    , E.height (E.px 2)
                    , E.centerY
                    , Background.color model.colors.lightBg
                    , Border.rounded 2
                    ]
                    E.none
                )
            ]
            { onChange = round >> SetFontSize
            , label =
                Input.labelRight []
                    (E.text <| String.fromInt model.fontSize ++ "px")
            , min = 10
            , max = 20
            , step = Just 1
            , value = toFloat <| model.fontSize
            , thumb =
                Input.defaultThumb
            }
        ]


viewHelpPopUp : Model -> E.Element Msg
viewHelpPopUp model =
    E.column
        (styles.popUp ++ [ Background.color model.colors.darkBg ])
        [ E.el styles.title <| E.text "Help"
        , E.paragraph
            [ E.htmlAttribute <| Html.Attributes.style "max-width" "60vw"
            ]
            [ E.text "Wasmiranda: "
            ]
        , E.el styles.subtitle <| E.text "Keyboard Shortcuts"
        , E.row
            []
            [ E.html
                (FeatherIcons.arrowDown
                    |> FeatherIcons.toHtml
                        [ Html.Attributes.style "margin-right" "20px" ]
                )
            , E.text "Go to next cell"
            ]
        , E.row
            []
            [ E.html
                (FeatherIcons.cornerDownLeft
                    |> FeatherIcons.toHtml
                        [ Html.Attributes.style "margin-right" "20px" ]
                )
            , E.text "Add newline"
            ]
        , E.row
            [ E.spacing 5 ]
            [ E.text "Ctrl"
            , E.text "+"
            , E.html
                (FeatherIcons.cornerDownLeft
                    |> FeatherIcons.toHtml
                        [ Html.Attributes.style "margin-right" "20px"
                        , Html.Attributes.style "margin-left" "5px"
                        ]
                )
            , E.text "Add cell"
            ]
        , E.el styles.subtitle <| E.text "Source Code"
        , E.newTabLink []
            { url =
                "https://github.com/JerryNyoike/upgraded-winner"
            , label =
                E.el
                    [ Font.underline ]
                <|
                    E.text "Open source on GitHub"
            }
        ]


viewToolButtons : Model -> E.Element Msg
viewToolButtons model =
    E.row
        [ E.alignRight
        , E.spacing 20
        , E.paddingXY 10 10
        ]
        [ viewHelpButton model
        , viewSettingsButton model
        ]


viewClosePopUpButton : E.Element Msg
viewClosePopUpButton =
    Input.button
        [ E.alignRight
        , E.padding 10
        ]
        { onPress =
            Just <| SetPopUp NoPopUp
        , label =
            E.html
                (FeatherIcons.xCircle
                    |> FeatherIcons.toHtml []
                )
        }


viewSettingsButton : Model -> E.Element Msg
viewSettingsButton model =
    Input.button
        [ E.alignRight
        , Element.Events.onMouseEnter <| SetToolTip SettingsToolTip
        , Element.Events.onMouseLeave <| SetToolTip NoToolTip
        , E.below <|
            case model.toolTip of
                SettingsToolTip ->
                    E.el
                        [ E.htmlAttribute <| Html.Attributes.style "position" "relative"
                        , E.htmlAttribute <| Html.Attributes.style "right" <| String.fromInt <| scale model.orientation 40
                        ]
                    <|
                        E.text "Settings"

                _ ->
                    E.none
        ]
        { onPress =
            Just <| SetPopUp SettingsPopUp
        , label =
            E.html
                (FeatherIcons.settings
                    |> FeatherIcons.toHtml []
                )
        }


viewHelpButton : Model -> E.Element Msg
viewHelpButton model =
    Input.button
        [ E.alignRight
        , Element.Events.onMouseEnter <| SetToolTip HelpToolTip
        , Element.Events.onMouseLeave <| SetToolTip NoToolTip
        , E.below <|
            case model.toolTip of
                HelpToolTip ->
                    E.el
                        [ E.htmlAttribute <| Html.Attributes.style "position" "relative"
                        , E.htmlAttribute <| Html.Attributes.style "right" <| String.fromInt <| scale model.orientation 5
                        ]
                    <|
                        E.text "Help"

                _ ->
                    E.none
        ]
        { onPress =
            Just <| SetPopUp HelpPopUp
        , label =
            E.html
                (FeatherIcons.helpCircle
                    |> FeatherIcons.toHtml []
                )
        }


viewAddCellButton : E.Element Msg
viewAddCellButton =
    Input.button
        [ E.centerX
        , E.alignBottom
        , E.paddingXY 0 20
        ]
        { onPress =
            Just AddCell
        , label =
            E.html
                (FeatherIcons.plusCircle
                    |> FeatherIcons.toHtml []
                )
        }


viewCell : Int -> Int -> ( String, Maybe String ) -> Model -> E.Element Msg
viewCell activeCellIndex currentCellIndex ( src, result ) model =
    let
        resultDisplay =
            E.el
                [ E.paddingEach
                    { left =
                        72
                    , right =
                        0
                    , top =
                        0
                    , bottom =
                        10
                    }
                ]
            <|
                E.paragraph []
                    [ E.text <| Maybe.withDefault "unable to get result" result ]
    in
    E.column
        [ E.spacing 15
        , E.width E.fill
        ]
        [ E.row
            [ E.width E.fill ]
            [ E.el
                [ E.paddingEach
                    { left =
                        0
                    , right =
                        10
                    , top =
                        0
                    , bottom =
                        0
                    }
                , E.width <| E.px 60
                ]
              <|
                E.el
                    [ E.centerX
                    , if activeCellIndex == currentCellIndex then
                        Font.color model.colors.darkFg

                      else
                        Font.color model.colors.lightFg
                    , E.below <|
                        if activeCellIndex == currentCellIndex then
                            viewAddCellButton

                        else
                            E.none
                    ]
                <|
                    E.text <|
                        "["
                            ++ String.fromInt (currentCellIndex + 1)
                            ++ "]"
            , if activeCellIndex == currentCellIndex then
                Input.multiline
                    [ E.width E.fill
                    , Input.focusedOnLoad
                    , E.htmlAttribute <|
                        Html.Events.on "keydown" <|
                            Decode.map HandleKeyDown Keyboard.Event.decodeKeyboardEvent
                    , E.htmlAttribute <| Html.Attributes.id <| getCellId currentCellIndex
                    , E.onRight <| viewRemoveActiveCellButton
                    , Background.color model.colors.lightBg
                    , E.htmlAttribute <| Html.Attributes.style "font-variant-ligatures" "inherit"
                    ]
                    { onChange =
                        EditCell
                    , text =
                        src
                    , placeholder =
                        Nothing
                    , label =
                        Input.labelHidden "edit definition"
                    , spellcheck =
                        False
                    }

              else
                E.el
                    [ E.htmlAttribute <| Html.Attributes.id <| getCellId currentCellIndex
                    , E.padding 10
                    , E.htmlAttribute <| Html.Attributes.style "min-height" "calc(1em + 24px)"
                    , Border.width 1
                    , Border.rounded 5
                    , Border.color model.colors.lightFg
                    , E.width E.fill
                    ]
                <|
                    E.text src
            ]
        , resultDisplay
        ]


viewRemoveActiveCellButton : E.Element Msg
viewRemoveActiveCellButton =
    Input.button
        [ E.centerY
        , E.htmlAttribute <| Html.Attributes.style "margin-left" "-30px"
        , E.htmlAttribute <| onClickNoProp CloseActiveCell
        ]
        { onPress =
            Nothing
        , label =
            E.html
                (FeatherIcons.xCircle
                    |> FeatherIcons.toHtml []
                )
        }


onClickNoProp : msg -> Html.Attribute msg
onClickNoProp msg =
    Html.Events.stopPropagationOn "click" (Decode.map (\msg1 -> ( msg1, True )) (Decode.succeed msg))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditCell newSrc ->
            editCell newSrc model

        AddCell ->
            addCell model

        HandleKeyDown event ->
            handleKeyDown event model

        HandleOrientation width ->
            handleOrientation width model

        SetPopUp popUp ->
            setPopUp popUp model

        SetToolTip toolTip ->
            setToolTip toolTip model

        CloseActiveCell ->
            closeActiveCell model

        GotCellCursorRow row ->
            gotCellCursorRow row model

        SetTheme theme ->
            setTheme theme model

        SetUseLigatures useLigatures ->
            setUseLigatures useLigatures model

        SetFontSize size ->
            setFontSize size model

        GotResult res ->
            let
                ( input, _ ) =
                    Maybe.withDefault emptyCell <|
                        Dict.get (model.activeCellIndex - 1) model.cells

                newCells =
                    Dict.insert (model.activeCellIndex - 1) ( input, res ) model.cells

                newModel =
                    { model | cells = newCells }
            in
            ( newModel, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


setFontSize : Int -> Model -> ( Model, Cmd Msg )
setFontSize size model =
    ( { model
        | fontSize =
            size
      }
    , Cmd.none
    )


setUseLigatures : Bool -> Model -> ( Model, Cmd Msg )
setUseLigatures useLigatures model =
    ( { model
        | useLigatures =
            useLigatures
      }
    , Cmd.none
    )


setTheme : Theme -> Model -> ( Model, Cmd Msg )
setTheme theme model =
    ( { model
        | theme =
            theme
        , colors =
            getThemeColors theme
      }
    , Cmd.none
    )


getThemeColors : Theme -> Colors
getThemeColors theme =
    case theme of
        Light ->
            { lightFg =
                E.rgb255 220 220 220
            , darkFg =
                E.rgb255 0 0 0
            , lightBg =
                E.rgb255 255 255 255
            , darkBg =
                E.rgb255 220 220 220
            }

        SolarizedLight ->
            { lightFg =
                E.rgb255 220 220 220
            , darkFg =
                E.rgb255 0 0 0
            , lightBg =
                E.rgb255 253 246 227
            , darkBg =
                E.rgb255 220 220 220
            }

        Dark ->
            { lightBg =
                E.rgb255 0 0 0
            , darkBg =
                E.rgb255 150 150 150
            , lightFg =
                E.rgb255 180 180 180
            , darkFg =
                E.rgb255 255 255 255
            }


gotCellCursorRow : Int -> Model -> ( Model, Cmd Msg )
gotCellCursorRow row model =
    case model.pendingKeyAction of
        Just Keyboard.Key.Up ->
            if row == 0 || row == 1 then
                activatePreviousCell model

            else
                ( model, Cmd.none )

        Just Keyboard.Key.Down ->
            if row == 0 || row == 2 then
                activateNextCell model

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


closeActiveCell : Model -> ( Model, Cmd Msg )
closeActiveCell model =
    let
        getNextCellIndex currentCell =
            currentCell
    in
    removeCell getNextCellIndex model


setToolTip : ToolTip -> Model -> ( Model, Cmd Msg )
setToolTip toolTip model =
    ( { model
        | toolTip =
            toolTip
      }
    , Cmd.none
    )


setPopUp : PopUp -> Model -> ( Model, Cmd Msg )
setPopUp popUp model =
    ( { model
        | popUp =
            popUp
      }
    , Cmd.none
    )


handleOrientation : Int -> Model -> ( Model, Cmd Msg )
handleOrientation width model =
    let
        orientation =
            if width <= 450 then
                Portrait

            else
                Landscape
    in
    ( { model
        | orientation =
            orientation
      }
    , Cmd.none
    )


handleKeyDown : KeyboardEvent -> Model -> ( Model, Cmd Msg )
handleKeyDown { keyCode, ctrlKey } model =
    case keyCode of
        Keyboard.Key.Enter ->
            if ctrlKey then
                let
                    code =
                        Tuple.first <|
                            Maybe.withDefault emptyCell <|
                                Dict.get model.activeCellIndex model.cells

                    ( newModel, cmd ) =
                        addCell model
                in
                ( newModel
                , Cmd.batch
                    [ cmd
                    , sendCode code
                    ]
                )

            else
                ( model, Cmd.none )

        Keyboard.Key.Up ->
            ( { model
                | pendingKeyAction =
                    Just Keyboard.Key.Up
              }
            , Cmd.none
            )

        Keyboard.Key.Down ->
            ( { model
                | pendingKeyAction =
                    Just Keyboard.Key.Down
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


activateNextCell : Model -> ( Model, Cmd Msg )
activateNextCell model =
    let
        oldActiveCellIndex =
            model.activeCellIndex

        newActiveCellIndex =
            if oldActiveCellIndex == Dict.size model.cells - 1 then
                oldActiveCellIndex

            else
                oldActiveCellIndex + 1
    in
    ( { model
        | activeCellIndex =
            newActiveCellIndex
      }
    , focusCell newActiveCellIndex
    )


activatePreviousCell : Model -> ( Model, Cmd Msg )
activatePreviousCell model =
    let
        oldActiveCellIndex =
            model.activeCellIndex

        newActiveCellIndex =
            if oldActiveCellIndex == 0 then
                oldActiveCellIndex

            else
                oldActiveCellIndex - 1
    in
    ( { model
        | activeCellIndex =
            newActiveCellIndex
      }
    , focusCell newActiveCellIndex
    )


removeCell : (Int -> Int) -> Model -> ( Model, Cmd Msg )
removeCell getNextCellIndex model =
    if Dict.size model.cells > 1 then
        let
            oldActiveCellIndex =
                model.activeCellIndex

            newActiveCellIndex =
                -- first cell
                if oldActiveCellIndex == 0 then
                    oldActiveCellIndex

                else
                    -- any cell after the first one
                    getNextCellIndex oldActiveCellIndex

            filterCell : Int -> Cell -> Dict Int Cell -> Dict Int Cell
            filterCell index cell cells =
                if index > oldActiveCellIndex then
                    Dict.insert (index - 1) cell cells

                else if index == oldActiveCellIndex then
                    cells

                else
                    Dict.insert index cell cells
        in
        ( { model
            | cells =
                Dict.foldr
                    filterCell
                    Dict.empty
                    model.cells
            , activeCellIndex =
                newActiveCellIndex
          }
        , focusCell newActiveCellIndex
        )

    else
        ( model
        , Cmd.none
        )


getActiveCell : Model -> Cell
getActiveCell model =
    Maybe.withDefault emptyCell <|
        -- impossible
        Dict.get model.activeCellIndex model.cells


addCell : Model -> ( Model, Cmd Msg )
addCell model =
    let
        newActiveCellIndex =
            model.activeCellIndex + 1
    in
    ( { model
        | cells =
            Dict.foldl
                (\index cell cells ->
                    if index > model.activeCellIndex then
                        Dict.insert (index + 1) cell cells

                    else
                        Dict.insert index cell cells
                )
                (Dict.singleton newActiveCellIndex emptyCell)
                model.cells
        , activeCellIndex =
            newActiveCellIndex
      }
    , focusCell newActiveCellIndex
    )


focusCell : Int -> Cmd Msg
focusCell index =
    Task.attempt (\_ -> NoOp) <| Browser.Dom.focus <| getCellId index


getCellId : Int -> String
getCellId index =
    "cell" ++ String.fromInt index


editCell : String -> Model -> ( Model, Cmd Msg )
editCell newSrc model =
    ( { model
        | cells =
            Dict.update
                model.activeCellIndex
                (\_ ->
                    Just ( newSrc, Nothing )
                 -- only need to update src
                )
                model.cells
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\width _ -> HandleOrientation width)
        , gotResult GotResult
        ]


insertToList : Int -> a -> List a -> List a
insertToList index element list =
    let
        ( before, after ) =
            List.Extra.splitAt index list
    in
    before ++ (element :: after)


decodeTheme : Decoder Theme
decodeTheme =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Light" ->
                        Decode.succeed Light

                    "SolarizedLight" ->
                        Decode.succeed SolarizedLight

                    "Dark" ->
                        Decode.succeed Dark

                    _ ->
                        Decode.fail "Invalid theme"
            )


encodeTheme : Theme -> Encode.Value
encodeTheme theme =
    Encode.string <|
        case theme of
            Light ->
                "Light"

            SolarizedLight ->
                "SolarizedLight"

            Dark ->
                "Dark"


scale : Orientation -> Int -> Int
scale orientation value =
    case orientation of
        Portrait ->
            round <| toFloat value

        Landscape ->
            value


elmUiColorToCssColor : E.Color -> String
elmUiColorToCssColor color =
    let
        { red, green, blue } =
            E.toRgb color
    in
    "rgb("
        ++ (String.join "," <|
                List.map
                    (\c ->
                        String.fromFloat <| c * 255
                    )
                    [ red, green, blue ]
           )
        ++ ")"
