module Main exposing (main)

import Browser
import Debug
import Generated.Api as BE
import Html as H
import Html.Attributes as HA
import Html.Attributes.Aria as HAA
import Html.Events as HE
import Http
import RemoteData exposing (RemoteData)
import Session
import Utils


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = HandleLoginResp BE.PlayerId (Result Http.Error String)
    | SetLoginPlayerId String
    | SetLoginPassword String
    | LoginSubmit
    | SetRegisterPlayerId String
    | SetRegisterPassword String
    | SetRegisterPasswordAgain String
    | RegisterSubmit
    | HandleRegisterResp BE.PlayerId (Result Http.Error String)
    | SetLobbyMsg String
    | LobbyMsgSubmit
    | HandleLobbyMsgResp (Result Http.Error ())


type alias Model =
    { player : Maybe Session.Player
    , loginToken : RemoteData String String
    , loginPlayerId : String
    , loginPassword : String
    , registerToken : RemoteData String String
    , registerPlayerId : String
    , registerPassword : String
    , registerPasswordAgain : String
    , registerValidationIssues : List String
    , lobbyMsg : String
    , lobbyMsgError : Maybe String
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { player = Nothing
      , loginToken = RemoteData.NotAsked
      , loginPlayerId = ""
      , loginPassword = ""
      , registerToken = RemoteData.NotAsked
      , registerPlayerId = ""
      , registerPassword = ""
      , registerPasswordAgain = ""
      , registerValidationIssues = []
      , lobbyMsg = ""
      , lobbyMsgError = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        HandleLoginResp pId r ->
            ( { model
                | player = Result.toMaybe r |> Maybe.map (Session.Player pId)
                , loginToken = RemoteData.fromResult r |> RemoteData.mapError Utils.httpErrorToStr
              }
            , Cmd.none
            )

        SetLoginPlayerId pId ->
            ( { model | loginPlayerId = pId }, Cmd.none )

        SetLoginPassword pw ->
            ( { model | loginPassword = pw }, Cmd.none )

        LoginSubmit ->
            ( { model | loginToken = RemoteData.Loading }
            , BE.postApiLogin (BE.DbPlayer model.loginPlayerId model.loginPassword) <|
                HandleLoginResp model.loginPlayerId
            )

        SetRegisterPlayerId pId ->
            ( { model | registerPlayerId = pId }, Cmd.none )

        SetRegisterPassword pw ->
            ( { model | registerPassword = pw }, Cmd.none )

        SetRegisterPasswordAgain pw ->
            ( { model | registerPasswordAgain = pw }, Cmd.none )

        RegisterSubmit ->
            let
                handleErrs es =
                    ( { model | registerValidationIssues = es, registerToken = RemoteData.NotAsked }
                    , Cmd.none
                    )

                registerPlayer dbP =
                    ( { model | registerValidationIssues = [], registerToken = RemoteData.Loading }
                    , BE.postApiPlayers dbP <| HandleRegisterResp model.registerPlayerId
                    )
            in
            Utils.result handleErrs registerPlayer <| validateDbPlayer model

        HandleRegisterResp pId r ->
            ( { model
                | player = Result.toMaybe r |> Maybe.map (Session.Player pId)
                , registerToken = RemoteData.fromResult r |> RemoteData.mapError Utils.httpErrorToStr
              }
            , Cmd.none
            )

        SetLobbyMsg s ->
            ( { model | lobbyMsg = s }, Cmd.none )

        LobbyMsgSubmit ->
            let
                submit p =
                    ( { model | lobbyMsgError = Nothing, lobbyMsg = "EMPTY" }, cmd p.token )

                cmd t =
                    if String.isEmpty model.lobbyMsg then
                        Cmd.none

                    else
                        BE.postApiLobby t model.lobbyMsg HandleLobbyMsgResp

                error =
                    -- I'd probably do a timed redirect or similar here. That way we surface the
                    -- error, as well as automatically redirect to where the user should be.
                    ( { model | lobbyMsgError = Just "No Player --- please log in again" }, Cmd.none )
            in
            Utils.maybe error submit model.player

        HandleLobbyMsgResp r ->
            ( Utils.result
                (\e -> { model | lobbyMsgError = Utils.httpErrorToStr e |> Just })
                (\() -> model)
                r
            , Cmd.none
            )


validateDbPlayer : Model -> Result.Result (List String) BE.DbPlayer
validateDbPlayer model =
    if model.registerPassword == model.registerPasswordAgain then
        Ok <| BE.DbPlayer model.registerPlayerId model.registerPassword

    else
        Err [ "Passwords do not match" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> H.Html Msg
view model =
    Utils.maybe (loggedOutView model) (\_ -> loggedInView model) model.player


loggedInView : Model -> H.Html Msg
loggedInView model =
    H.div [ HA.class "lobby" ]
        [ H.div [ HA.class "lobby-games" ]
            [ H.h1 [] [ H.text "Lobby" ]
            ]
        , H.div [ HA.class "chatbox-container" ]
            [ H.h2 [] [ H.text "Chat Lobby" ]
            , H.div [ HA.id "chatbox", HA.class "chatbox" ] []
            , H.form [ HE.onSubmit LobbyMsgSubmit ]
                [ H.ul []
                    [ H.li [ HA.class "chat-message" ]
                        [ H.input
                            [ HA.placeholder "type a chat message"
                            , HA.class "chat-message-input"
                            , HAA.ariaLabel "Enter Chat Message"
                            , HE.onInput SetLobbyMsg
                            ]
                            [ H.text model.lobbyMsg ]
                        ]
                    , H.li []
                        [ H.button
                            [ HA.class "btn primary" ]
                            [ H.text "send" ]
                        ]
                    ]
                , H.p [ HA.class "err" ] <|
                    Utils.maybe [] (List.singleton << H.text) model.lobbyMsgError
                ]
            ]
        ]


loggedOutView : Model -> H.Html Msg
loggedOutView model =
    H.div []
        [ H.div
            [ HA.class "login-box" ]
            [ H.h1 [] [ H.text "Login" ]
            , H.form [ HE.onSubmit LoginSubmit ]
                [ H.input
                    [ HA.placeholder "Player ID"
                    , HAA.ariaLabel "Player ID"
                    , HE.onInput SetLoginPlayerId
                    ]
                    [ H.text model.loginPlayerId
                    ]
                , H.input
                    [ HA.placeholder "Password"
                    , HA.type_ "password"
                    , HAA.ariaLabel "Password"
                    , HE.onInput SetLoginPassword
                    ]
                    [ H.text model.loginPassword
                    ]
                , H.ul
                    [ HA.class "err" ]
                    (viewRemoteDataError model.loginToken)
                , H.button
                    [ HA.class "btn primary" ]
                    [ H.text "Login" ]
                ]
            ]
        , H.div [ HA.class "login-box" ]
            [ H.h1 [] [ H.text "Register" ]
            , H.form [ HE.onSubmit RegisterSubmit ]
                [ H.input
                    [ HA.placeholder "Player ID"
                    , HAA.ariaLabel "Player ID"
                    , HE.onInput SetRegisterPlayerId
                    ]
                    [ H.text model.registerPlayerId
                    ]
                , H.input
                    [ HA.placeholder "Password"
                    , HA.type_ "password"
                    , HAA.ariaLabel "Password"
                    , HE.onInput SetRegisterPassword
                    ]
                    [ H.text model.registerPassword
                    ]
                , H.input
                    [ HA.placeholder "PasswordAgain"
                    , HA.type_ "password"
                    , HAA.ariaLabel "PasswordAgain"
                    , HE.onInput SetRegisterPasswordAgain
                    ]
                    [ H.text model.registerPasswordAgain
                    ]
                , H.ul
                    [ HA.class "err" ]
                    (registerErrors model)
                , H.button
                    [ HA.class "btn primary" ]
                    [ H.text "Register" ]
                ]
            ]
        ]


registerErrors : Model -> List (H.Html msg)
registerErrors m =
    let
        validationErrors =
            List.map (\e -> H.li [] [ H.text e ]) m.registerValidationIssues
    in
    validationErrors ++ viewRemoteDataError m.registerToken


viewRemoteDataError : RemoteData String String -> List (H.Html msg)
viewRemoteDataError =
    Utils.maybe [] (\e -> [ H.li [] [ H.text e ] ]) << Utils.remoteDataError
