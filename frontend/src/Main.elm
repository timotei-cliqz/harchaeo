
module Main exposing (..)

import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Generated.Api exposing (..)


main =
  program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }


-- * MODEL

type alias Model =
    { channels          : (List Channel)
    , currentChannel    : (Maybe String)
    , currentMessages   : (List Message)
    , pattern           : (Maybe String)
    }

init : (Model, Cmd Msg)
init =
    ( Model [] Nothing [] Nothing
    , Http.send NewChannels getChannel
    )
    -- TODO: emit a command to fetch the list of messages

-- * UPDATE

type Msg =
      NewChannels       (Result Http.Error (List Channel))
    | NewMessages       (Result Http.Error (List Message))
    | FilterMessages    (String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewChannels (Ok channels) ->
            ( { model | channels = channels }
            , Http.send NewMessages (getChannelByName "officetalk")
            )
        NewMessages (Ok messages) ->
            ( { model | currentMessages = messages }
            , Cmd.none
            )
        FilterMessages pattern ->
            ( { model | pattern = Just (String.toLower pattern) }
            , Cmd.none
            )
        _ -> -- TODO: Display error?
            (model, Cmd.none)


-- * VIEW

getMessages : List Message -> List String
getMessages messages =
    List.map (\m -> (Maybe.withDefault "" m.text)) messages

containsMaybe : Maybe String -> String -> Bool
containsMaybe pattern string =
    case pattern of
        Just p ->
            String.contains p (String.toLower string)
        Nothing ->
            True

filterMessages : Maybe String -> List String -> List String
filterMessages pattern strings =
    List.filter (containsMaybe pattern) strings


view : Model -> Html Msg
view ({ currentMessages, pattern }) =
    let
        strings = getMessages currentMessages
        messages = filterMessages pattern strings
    in
        div []
          [ input
          [ placeholder "filter"
          , autofocus True
          , onInput FilterMessages
          ] []
          , ol []
          (List.map (\t -> text t) messages)
        ]


-- * SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
