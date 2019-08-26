module Main exposing (main)

import Browser
import Debug
import Generated.Api as BE
import Html as H
import Html.Attributes as HA
import Html.Attributes.Aria as HAA
import Html.Events as HE
import Http
import RemoteData exposing (RemoteData(..))
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
    = HandleLoginResp (Result Http.Error String)
    | SetLoginPlayerId String
    | SetLoginPassword String
    | LoginSubmit
    | SetRegisterPlayerId String
    | SetRegisterPassword String
    | SetRegisterPasswordAgain String
    | RegisterSubmit
    | HandleRegisterResp (Result Http.Error String)


type alias Model =
    { loginToken : RemoteData String String
    , loginPlayerId : String
    , loginPassword : String
    , registerToken : RemoteData String String
    , registerPlayerId : String
    , registerPassword : String
    , registerPasswordAgain : String
    , registerValidationIssues : List String
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { loginToken = NotAsked
      , loginPlayerId = ""
      , loginPassword = ""
      , registerToken = NotAsked
      , registerPlayerId = ""
      , registerPassword = ""
      , registerPasswordAgain = ""
      , registerValidationIssues = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        HandleLoginResp (Ok t) ->
            ( { model | loginToken = Success t }, Cmd.none )

        HandleLoginResp (Err err) ->
            ( { model | loginToken = Failure (Utils.httpErrorToStr err) }, Cmd.none )

        SetLoginPlayerId pId ->
            ( { model | loginPlayerId = pId }, Cmd.none )

        SetLoginPassword pw ->
            ( { model | loginPassword = pw }, Cmd.none )

        LoginSubmit ->
            ( model, BE.postApiLogin (BE.DbPlayer model.loginPlayerId model.loginPassword) HandleLoginResp )

        SetRegisterPlayerId pId ->
            ( { model | registerPlayerId = pId }, Cmd.none )

        SetRegisterPassword pw ->
            ( { model | registerPassword = pw }, Cmd.none )

        SetRegisterPasswordAgain pw ->
            ( { model | registerPasswordAgain = pw }, Cmd.none )

        RegisterSubmit ->
            let
                handleErrs es =
                    ( { model | registerValidationIssues = es }, Cmd.none )

                registerPlayer dbP =
                    ( { model | registerValidationIssues = [], registerToken = Loading }
                    , BE.postApiPlayers dbP HandleRegisterResp
                    )
            in
            Utils.result handleErrs registerPlayer <| validateDbPlayer model

        HandleRegisterResp (Ok t) ->
            ( { model | registerToken = Success t }, Cmd.none )

        HandleRegisterResp (Err err) ->
            ( { model | registerToken = Failure (Utils.httpErrorToStr err) }, Cmd.none )


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
