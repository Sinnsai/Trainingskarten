module Main exposing (main)

--import String
import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, h1, h2, main_, p, span, text)
import Html.Attributes exposing (class, classList, href, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import String
import Svg
import Svg.Attributes as SvgAttr
import Task
import Time
import Url exposing (Url)


type Deck
    = Mandarin
    | Koreanisch


type alias CardProgress =
    { cardId : Int
    , interval : Int
    , repetitions : Int
    , easeFactor : Float
    , nextReview : Int
    }


type alias Card =
    { id : Int
    , lesson : Int
    , german : String
    , foreign : String
    , pronunciation : String
    , category : List String
    }


type Answer
    = Hard
    | Good
    | Easy


type alias StudyStats =
    { total : Int
    , started : Int
    , newCards : Int
    , mastered : Int
    , dueToday : Int
    , progressPercent : Float
    }



-- Modell ist der Zustand in der app


type alias Model =
    { key : Nav.Key
    , cards : List Card
    , currentIndex : Int
    , flipped : Bool
    , route : Route
    , selectedCategory : Maybe String
    , error : Maybe String
    , progress : List CardProgress
    , today : Int
    }


type Route
    = Home
    | ChooseSet Deck
    | Learn Deck Int
    | DailyStack Deck



-- was passieren kann


type Msg
    = FlipCard
    | NextCard
    | ChooseCategory (Maybe String)
    | GotCards (Result Http.Error (List Card))
    | GotDailyCards (Result Http.Error (List Card))
    | MarkCard Answer
    | GoHome
    | SelectDeck Deck
    | SelectLesson Deck Int
    | SelectDailyStack Deck
    | Tick Time.Posix
    | GotTime Time.Posix
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 60000 Tick


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            parseRoute url
    in
    ( { key = key
      , cards = []
      , currentIndex = 0
      , flipped = False
      , route = route
      , selectedCategory = Nothing
      , error = Nothing
      , progress = []
      , today = 0
      }
    , Cmd.batch
        [ Time.now |> Task.perform GotTime
        , loadRoute route
        ]
    )



-- abhier wird app gestartet


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
                case urlRequest of
                    Browser.Internal url ->
                        ( model
                        , Nav.pushUrl model.key (Url.toString url)
                        )

                    Browser.External href ->
                        ( model
                        , Nav.load href
                        )

        UrlChanged url ->
                let
                    newRoute =
                        parseRoute url
                in
                ( { model
                    | route = newRoute
                    , cards = []
                    , currentIndex = 0
                    , flipped = False
                    , selectedCategory = Nothing
                    , error = Nothing
                  }
                , loadRoute newRoute
                )

        GotTime time ->
            ( { model | today = Time.posixToMillis time }
            , Cmd.none
            )

        Tick time ->
            ( { model
                | today = Time.posixToMillis time
              }
            , Cmd.none
            )

         GoHome ->
                    ( model
                     , pushRoute model.key Home
                     )

         SelectDeck deck ->
                     ( model
                     , pushRoute model.key (ChooseSet deck)
                     )

         SelectLesson deck lesson ->
                     ( model
                     , pushRoute model.key (Learn deck lesson)
                     )

         SelectDailyStack deck ->
                     ( model
                     , pushRoute model.key (DailyStack deck)
                     )

        FlipCard ->
            ( { model | flipped = not model.flipped }, Cmd.none )

        -- cmd.non nichts muss geladen oder extern gemacht werden // youtube tutorial
        NextCard ->
            let
                numberOfCards =
                    List.length (visibleCards model)

                nextIndex =
                    if numberOfCards == 0 then
                        0

                    else
                        modBy numberOfCards (model.currentIndex + 1)
            in
            ( { model
                | currentIndex = nextIndex
                , flipped = False
              }
            , Cmd.none
            )

        -- Funktion was passiert wenn man eine Sprache auswählt
        ChooseCategory category ->
            ( { model
                | selectedCategory = category
                , currentIndex = 0
                , flipped = False
              }
            , Cmd.none
            )

        -- ab hier sollte Jason datei geladen sein
        GotCards result ->
            -- über prüfung ob das lenden des Decks erfolgreich war
            case result of
                Ok cards ->
                    ( { model
                        | cards = cards

                        -- alles wird ab hier auf anfag gestzt
                        , currentIndex = 0
                        , flipped = False
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                -- wenn laden fehlschlägt
                Err err ->
                    ( { model
                        | error = Just (httpErrorToString err)
                      }
                    , Cmd.none
                    )

        GotDailyCards result ->
            case result of
                Ok cards ->
                    ( { model
                        | cards = model.cards ++ cards
                        , currentIndex = 0
                        , flipped = False
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model
                        | error = Just (httpErrorToString err)
                      }
                    , Cmd.none
                    )

        -- Status der Karte richtig / falshc
        MarkCard correct ->
            case getCurrentCard model of
                Nothing ->
                    ( model, Cmd.none )

                Just card ->
                    ( { model
                        | progress =
                            updateProgress card.id correct model.progress model.today
                        , currentIndex = 0
                        , flipped = False
                      }
                    , Cmd.none
                    )



-- speichern des lernstands


updateProgress : Int -> Answer -> List CardProgress -> Int -> List CardProgress
updateProgress cardId answer progress today =
    let
        oldProgress =
            progress
                |> List.filter (\p -> p.cardId == cardId)
                |> List.head
                |> Maybe.withDefault
                    { cardId = cardId
                    , interval = 1
                    , repetitions = 0
                    , easeFactor = 2.5
                    , nextReview = today
                    }

        newProgress =
            calculateProgress answer oldProgress today
    in
    newProgress
        :: List.filter (\p -> p.cardId /= cardId) progress



-- Alte einträge werden gelöscht damit man keine doppelten listen bekommt und daudrch fehler meldungen
-- laden der richtigen jason Datei


loadRoute : Route -> Cmd Msg
loadRoute route =
    case route of
        Home ->
            Cmd.none

        ChooseSet _ ->
            Cmd.none

        Learn deck lesson ->
            loadCards deck lesson

        DailyStack deck ->
            loadDailyCards deck



-- laden der richtigen jason Datei


loadCards : Deck -> Int -> Cmd Msg
loadCards deck lesson =
    Http.get
        { url = dataUrl deck lesson
        , expect = Http.expectJson GotCards (Decode.list (cardDecoder deck))
        }


deckLessons : Deck -> List Int
deckLessons deck =
    case deck of
        Mandarin ->
            [ 1, 2 ]

        Koreanisch ->
            [ 1, 2 ]


loadDailyCards : Deck -> Cmd Msg
loadDailyCards deck =
    deckLessons deck
        |> List.map
            (\lesson ->
                Http.get
                    { url = dataUrl deck lesson
                    , expect = Http.expectJson GotDailyCards (Decode.list (cardDecoder deck))
                    }
            )
        |> Cmd.batch


dataUrl : Deck -> Int -> String
dataUrl deck lesson =
    case ( deck, lesson ) of
        ( Mandarin, 1 ) ->
            basePath ++ "/data/1. Lektion Mandarin.json"

        ( Mandarin, 2 ) ->
            basePath ++ "/data/2. Lektion Mandarin.json"

        ( Koreanisch, 1 ) ->
            basePath ++ "/data/1. Lektion Korean.json"

        ( Koreanisch, 2 ) ->
            basePath ++ "/data/2. Lektion Korean.json"

        _ ->
            basePath ++ "/data/1. Lektion Korean.json"


deckFileName : Deck -> String
deckFileName deck =
    case deck of
        Mandarin ->
            "Mandarin"

        Koreanisch ->
            "Korean"


cardDecoder : Deck -> Decoder Card
cardDecoder deck =
    case deck of
        Mandarin ->
            mandarinCardDecoder

        Koreanisch ->
            koreanCardDecoder



-- Decoder


mandarinCardDecoder : Decoder Card



-- decder erklärt wie jason  in eine datei karte umgewandelt wird


mandarinCardDecoder =
    Decode.map6 Card
        -- Karte hat 6 felder die ausgelesen werden sollen ###??? nochmal möglichkeit zu sortieren suchen nach kategorie
        (Decode.field "id" Decode.int)
        -- die werter werden gelesen
        -- die folgen werte werden ausgelesen
        (Decode.field "lesson" Decode.int)
        (Decode.at [ "front", "german" ] Decode.string)
        (Decode.at [ "back", "mandarin" ] Decode.string)
        (Decode.at [ "back", "pinyin" ] Decode.string)
        (Decode.field "category" (Decode.list Decode.string))


koreanCardDecoder : Decoder Card
koreanCardDecoder =
    Decode.map6 Card
        (Decode.field "id" Decode.int)
        (Decode.field "lesson" Decode.int)
        (Decode.at [ "front", "german" ] Decode.string)
        (Decode.at [ "back", "korean" ] Decode.string)
        (Decode.at [ "back", "pronunciation" ] Decode.string)
        (Decode.field "category" (Decode.list Decode.string))


visibleCards : Model -> List Card
visibleCards model =
    let
        categoryFilteredCards =
            case model.selectedCategory of
                Nothing ->
                    model.cards

                Just category ->
                    List.filter (\card -> List.member category card.category) model.cards
    in
    if isDailyStack model.route then
        List.filter (isDueToday model.today model.progress) categoryFilteredCards

    else
        categoryFilteredCards


availableCategories : Model -> List String
availableCategories model =
    model.cards
        |> List.concatMap .category
        |> unique


unique : List String -> List String
unique values =
    case values of
        [] ->
            []

        first :: rest ->
            first :: unique (List.filter (\value -> value /= first) rest)


parseRoute : Url -> Route
parseRoute url =
    case List.reverse (pathSegments url.path) of
        "daily" :: "mandarin" :: _ ->
            DailyStack Mandarin

        "daily" :: "koreanisch" :: _ ->
            DailyStack Koreanisch

        lessonSegment :: "mandarin" :: _ ->
            Learn Mandarin (parseLessonWithDefault lessonSegment)

        lessonSegment :: "koreanisch" :: _ ->
            Learn Koreanisch (parseLessonWithDefault lessonSegment)

        "mandarin" :: _ ->
            ChooseSet Mandarin

        "koreanisch" :: _ ->
            ChooseSet Koreanisch

        _ ->
            Home

basePath : String
basePath =
    "/lernkartentrainer"

routeToPath : Route -> String
routeToPath route =
    case route of
        Home ->
            basePath ++ "/"

        ChooseSet Mandarin ->
            basePath ++ "/mandarin"

        ChooseSet Koreanisch ->
            basePath ++ "/koreanisch"

        Learn Mandarin lesson ->
            basePath ++ "/mandarin/lektion-" ++ String.fromInt lesson

        Learn Koreanisch lesson ->
            basePath ++ "/koreanisch/lektion-" ++ String.fromInt lesson

        DailyStack Mandarin ->
            basePath ++ "/mandarin/daily"

        DailyStack Koreanisch ->
            basePath ++ "/koreanisch/daily"

pushRoute : Nav.Key -> Route -> Cmd Msg
pushRoute key route =
    Nav.pushUrl key (routeToPath route)

viewLanguageNavigation : Html Msg
viewLanguageNavigation =
    div [ class "set-navigation" ]
        [ button
            [ class "set-link"
            , onClick (SelectDeck Mandarin)
            ]
            [ text "Mandarin" ]
        , button
            [ class "set-link"
            , onClick (SelectDeck Koreanisch)
            ]
            [ text "Koreanisch" ]
        ]


viewSetNavigation : Deck -> Html Msg
viewSetNavigation deck =
    case deck of
        Mandarin ->
            div [ class "set-navigation" ]
                [ h2 [] [ text "Mandarin" ]
                , button [ class "set-link", onClick (SelectLesson Mandarin 1) ] [ text "Lernset 1" ]
                , button [ class "set-link", onClick (SelectLesson Mandarin 2) ] [ text "Lernset 2" ]
                , button [ class "set-link", onClick (SelectDailyStack Mandarin) ] [ text "Täglicher Stapel" ]
                , button [ class "set-link", onClick GoHome ] [ text "Zurück zur Sprachauswahl" ]
                ]

        Koreanisch ->
            div [ class "set-navigation" ]
                [ h2 [] [ text "Koreanisch" ]
                , button [ class "set-link", onClick (SelectLesson Koreanisch 1) ] [ text "Lernset 1" ]
                , button [ class "set-link", onClick (SelectLesson Koreanisch 2) ] [ text "Lernset 2" ]
                , button [ class "set-link", onClick (SelectDailyStack Koreanisch) ] [ text "Täglicher Stapel" ]
                , button [ class "set-link", onClick GoHome ] [ text "Zurück zur Sprachauswahl" ]
                ]


pathSegments : String -> List String
pathSegments path =
    path
        |> String.split "/"
        |> List.filter (\segment -> segment /= "")


parseLessonWithDefault : String -> Int
parseLessonWithDefault segment =
    parseLesson segment
        |> Maybe.withDefault 1


parseLesson : String -> Maybe Int
parseLesson segment =
    if String.startsWith "lektion-" segment then
        segment
            |> String.dropLeft 8
            |> String.toInt

    else
        Nothing



-- welche karte kommt als nächstes ###??? random  draus machen


dayInMillis : Int
dayInMillis =
    24 * 60 * 60 * 1000


calculateProgress : Answer -> CardProgress -> Int -> CardProgress
calculateProgress answer progress today =
    case answer of
        Hard ->
            { progress
                | interval = 1
                , repetitions = 0
                , nextReview = today
            }

        Good ->
            { progress
                | interval = max 1 (progress.interval * 2)
                , repetitions = progress.repetitions + 1
                , nextReview = today + (progress.interval * 2 * dayInMillis)
            }

        Easy ->
            { progress
                | interval =
                    round (toFloat progress.interval * 3)
                , repetitions =
                    progress.repetitions + 1
                , easeFactor =
                    progress.easeFactor + 0.15
                , nextReview = today + (round (toFloat progress.interval * 3) * dayInMillis)
            }


cardRepetitions : Card -> List CardProgress -> Int
cardRepetitions card progress =
    progress
        |> List.filter (\p -> p.cardId == card.id)
        |> List.head
        |> Maybe.map .repetitions
        |> Maybe.withDefault 0


cardWeight : Card -> List CardProgress -> Int
cardWeight card progress =
    progress
        |> List.filter (\p -> p.cardId == card.id)
        |> List.head
        |> Maybe.map .nextReview
        |> Maybe.withDefault 0


getWeakCards : Model -> List Card
getWeakCards model =
    visibleCards model
        |> List.sortBy (\card -> cardWeight card model.progress)



-- Hilfsfunktionen für fällige KArten


isDailyStack : Route -> Bool
isDailyStack route =
    case route of
        DailyStack _ ->
            True

        _ ->
            False


progressForCard : Int -> List CardProgress -> Maybe CardProgress
progressForCard cardId progress =
    progress
        |> List.filter (\p -> p.cardId == cardId)
        |> List.head


isDueToday : Int -> List CardProgress -> Card -> Bool
isDueToday today progress card =
    case progressForCard card.id progress of
        Nothing ->
            True

        Just cardProgress ->
            cardProgress.nextReview <= today


learningStatusText : Int -> CardProgress -> String
learningStatusText today progress =
    if progress.nextReview <= today then
        if progress.repetitions == 0 then
            "Schwer / heute erneut üben"

        else
            "Heute fällig"

    else if progress.repetitions >= 5 then
        "Gelernt"

    else if progress.repetitions >= 3 then
        "Fast gelernt"

    else
        "Im Aufbau"

nextReviewText : Int -> Int -> String
nextReviewText today nextReview =
    let
        difference =
            nextReview - today

        days =
            ceiling (toFloat difference / toFloat dayInMillis)
    in
    if nextReview <= today then
        "heute fällig"

    else if days == 1 then
        "morgen"

    else
        "in " ++ String.fromInt days ++ " Tagen"



studyStats : Model -> StudyStats
studyStats model =
    let
        total =
            List.length model.cards

        started =
            model.cards
                |> List.filter
                    (\card ->
                        progressForCard card.id model.progress /= Nothing
                    )
                |> List.length

        newCards =
            total - started

        mastered =
            model.cards
                |> List.filter
                    (\card ->
                        cardRepetitions card model.progress >= 5
                    )
                |> List.length

        dueToday =
            model.cards
                |> List.filter (isDueToday model.today model.progress)
                |> List.length

        progressPercent =
            if total == 0 then
                0

            else
                model.cards
                    |> List.map
                        (\card ->
                            let
                                reps =
                                    cardRepetitions card model.progress
                            in
                            min 5 reps
                        )
                    |> List.sum
                    |> toFloat
                    |> (\learnedPoints -> learnedPoints / (toFloat total * 5) * 100)
    in
    { total = total
    , started = started
    , newCards = newCards
    , mastered = mastered
    , dueToday = dueToday
    , progressPercent = progressPercent
    }


progressRing : Float -> Html Msg
progressRing percent =
    let
        radius =
            45

        circumference =
            2 * pi * radius

        safePercent =
            clamp 0 100 percent

        offset =
            circumference - (safePercent / 100 * circumference)

        percentText =
            String.fromInt (round safePercent) ++ "%"
    in
    Svg.svg
        [ SvgAttr.width "120"
        , SvgAttr.height "120"
        , SvgAttr.viewBox "0 0 120 120"
        , SvgAttr.class "progress-ring"
        ]
        [ Svg.circle
            [ SvgAttr.cx "60"
            , SvgAttr.cy "60"
            , SvgAttr.r (String.fromFloat radius)
            , SvgAttr.fill "none"
            , SvgAttr.stroke "#e3e8f3"
            , SvgAttr.strokeWidth "10"
            ]
            []
        , Svg.circle
            [ SvgAttr.cx "60"
            , SvgAttr.cy "60"
            , SvgAttr.r (String.fromFloat radius)
            , SvgAttr.fill "none"
            , SvgAttr.stroke "#2454d6"
            , SvgAttr.strokeWidth "10"
            , SvgAttr.strokeDasharray (String.fromFloat circumference)
            , SvgAttr.strokeDashoffset (String.fromFloat offset)
            , SvgAttr.transform "rotate(-90 60 60)"
            ]
            []
        , Svg.text_
            [ SvgAttr.x "60"
            , SvgAttr.y "60"
            , SvgAttr.textAnchor "middle"
            , SvgAttr.dominantBaseline "middle"
            , SvgAttr.fontSize "22"
            , SvgAttr.fill "#172033"
            ]
            [ Svg.text percentText ]
        ]



-- geh zur nächsten Karte
-- erzeugen der Webseite


view : Model -> Browser.Document Msg
view model =
    { title = "Lernkarten-Trainer"
    , body =
        [ main_ [ class "app" ]
            [ h1 [] [ text "Lernkarten-Trainer" ]
            , viewError model
            , viewCurrentCard model
            ]
        ]
    }


viewCategories : Model -> Html Msg
viewCategories model =
    let
        categories =
            availableCategories model
    in
    if List.isEmpty categories then
        text ""

    else
        div [ class "category-filter" ]
            (button
                [ classList [ ( "active", model.selectedCategory == Nothing ) ]
                , onClick (ChooseCategory Nothing)
                ]
                [ text "Alle" ]
                :: List.map (viewCategoryButton model.selectedCategory) categories
            )


viewCategoryButton : Maybe String -> String -> Html Msg
viewCategoryButton selectedCategory category =
    button
        [ classList [ ( "active", selectedCategory == Just category ) ]
        , onClick (ChooseCategory (Just category))
        ]
        [ text category ]



-- Funktion anzeigen der Fehler


viewError : Model -> Html Msg
viewError model =
    case model.error of
        -- überprüfung ob fehler gespeichert wurde
        Nothing ->
            -- kein Fehler
            text ""

        Just message ->
            -- Fehler meldung anzeigen
            p [ class "error" ] [ text message ]



-- ### ??? warum hier nur textmassage
-- anzeigen aktueller karte


viewCurrentCard : Model -> Html Msg
viewCurrentCard model =
    case model.route of
        Home ->
            viewLanguageNavigation

        ChooseSet deck ->
            viewSetNavigation deck

        Learn _ _ ->
            viewLearningArea model

        DailyStack _ ->
            viewLearningArea model


viewLearningArea : Model -> Html Msg
viewLearningArea model =
    case getCurrentCard model of
        Nothing ->
            div []
                [ viewStats model
                , p [] [ text "Für heute gibt es keine fälligen Karten oder sie werden noch geladen." ]
                , button [ class "set-link", onClick GoHome ] [ text "Zurück zur Sprachauswahl" ]
                ]

        Just card ->
            div []
                [ button [ class "set-link", onClick GoHome ] [ text "Zurueck zur Sprachauswahl" ]
                , viewStats model
                , viewCategories model
                , div [ class "card-area" ]
                    [ div [ classList [ ( "flashcard", True ), ( "flipped", model.flipped ) ] ]
                        [ viewCardContent card ]
                    , viewCardInfo model card
                    , viewLearningStatus model card
                    , div [ class "actions" ]
                        [ button [ onClick FlipCard ] [ text "Umdrehen" ]
                        , button [ onClick NextCard ] [ text "Naechste Karte" ]
                        ]
                    , div [ class "answer-actions" ]
                        [ button [ class "answer-button bad", onClick (MarkCard Hard) ] [ text "Schwer" ]
                        , button [ class "answer-button good", onClick (MarkCard Good) ] [ text "Gut" ]
                        , button [ class "answer-button easy", onClick (MarkCard Easy) ] [ text "Leicht" ]
                        ]
                    ]
                ]


viewCardInfo : Model -> Card -> Html Msg
viewCardInfo model card =
    p [ class "card-info" ]
        [ text ("Karte " ++ String.fromInt (model.currentIndex + 1) ++ " von " ++ String.fromInt (List.length (visibleCards model)))
        , span [] [ text (" | Kategorie: " ++ String.join ", " card.category) ]
        ]



-- Anzeigen des Lernstatus der angezeigten KArte


viewLearningStatus : Model -> Card -> Html Msg
viewLearningStatus model card =
    case progressForCard card.id model.progress of
        Nothing ->
            div [ class "learning-status" ]
                [ p [ class "status-title" ] [ text "Status: Neue Karte" ]
                , p [] [ text "Fortschritt: 0/5" ]
                , div [ class "progress-bar" ]
                    [ div
                        [ class "progress-fill"
                        , style "width" "0%"
                        ]
                        []
                    ]
                , p [] [ text "Diese Karte wurde noch nicht bewertet." ]
                ]

        Just cardProgress ->
            let
                level =
                    min 5 cardProgress.repetitions

                percent =
                    round (toFloat level / 5 * 100)

                statusText =
                    learningStatusText model.today cardProgress

                dueText =
                    nextReviewText model.today cardProgress.nextReview
            in
            div [ class "learning-status" ]
                [ p [ class "status-title" ]
                    [ text ("Status: " ++ statusText) ]
                , p []
                    [ text
                        ("Fortschritt: "
                            ++ String.fromInt level
                            ++ "/5"
                        )
                    ]
                , div [ class "progress-bar" ]
                    [ div
                        [ class "progress-fill"
                        , style "width" (String.fromInt percent ++ "%")
                        ]
                        []
                    ]
                , p []
                    [ text
                        ("Wiederholungen: "
                            ++ String.fromInt cardProgress.repetitions
                        )
                    ]
                , p []
                    [ text
                        ("Intervall: "
                            ++ String.fromInt cardProgress.interval
                            ++ " Tag(e)"
                        )
                    ]
                , p []
                    [ text
                        ("Nächste Wiederholung: "
                            ++ dueText
                        )
                    ]
                ]



-- welche seite Sprache wird angezeigt


viewCardContent : Card -> Html Msg
viewCardContent card =
    div [ class "flashcard-inner" ]
        [ div [ class "flashcard-front" ]
            [ h2 [] [ text card.german ]
            ]
        , div [ class "flashcard-back" ]
            [ h2 [] [ text card.foreign ]
            , p [] [ text card.pronunciation ]
            ]
        ]



--statistik anzeigen


viewStats : Model -> Html Msg
viewStats model =
    let
        stats =
            studyStats model
    in
    div [ class "stats-panel" ]
        [ h2 [] [ text "Lernstatistik" ]
        , div [ class "stats-content" ]
            [ progressRing stats.progressPercent
            , div [ class "stats-grid" ]
                [ statBox "Alle Karten" stats.total
                , statBox "Begonnen" stats.started
                , statBox "Neu" stats.newCards
                , statBox "Heute fällig" stats.dueToday
                , statBox "Gelernt" stats.mastered
                ]
            ]
        ]


statBox : String -> Int -> Html Msg
statBox label value =
    div [ class "stat-box" ]
        [ span [ class "stat-number" ] [ text (String.fromInt value) ]
        , span [ class "stat-label" ] [ text label ]
        ]



-- aktuelle karte aus der liste holen ###???? bin mir nicht sicher ob das funktioniert


getCurrentCard : Model -> Maybe Card
getCurrentCard model =
    getWeakCards model
        |> List.drop model.currentIndex
        |> List.head


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Netzwerkfehler"

        Http.BadStatus code ->
            "HTTP Status: " ++ String.fromInt code

        Http.BadBody message ->
            "JSON-Fehler: " ++ message


