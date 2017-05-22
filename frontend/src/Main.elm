
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
    --, Http.send NewMessages (getChannelByName "officetalk")
    )
    -- TODO: emit a command to fetch the list of messages

-- * UPDATE

type Msg =
      NewChannels       (Result Http.Error (List Channel))
    | Channel           (Channel)
    | NewMessages       (Result Http.Error (List Message))
    | FilterMessages    (String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewChannels (Ok channels) ->
            -- *Debug.log(List.foldl (++) "" (List.map (\t -> " " ++ t.name) channels))
            ( { model | channels = channels }
            , Cmd.none
            )
        Channel channel ->
            ( { model | currentChannel = Just channel.name }
            , Http.send NewMessages (getChannelByName channel.name)
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
view ({ channels, currentChannel, currentMessages, pattern }) =
    let
        strings = getMessages currentMessages
        messages = filterMessages pattern strings
        --channel_names = List.map (\t -> t.name) channels
    in
        div [] -- * container with channel div and messages div
          [ channelComponent channels currentChannel
          , messagesComponent messages
          ]


-- * SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- * VISUAL COMPONENTS
channelComponent : (List Channel) -> (Maybe String) -> Html.Html Msg
channelComponent channels currentChannel =
    div [inlineStyle, sclollableContainerStyle, channelsStyle, channelPrimaryBgColor]
      [ channelHeader
      , channelNames channels currentChannel
      ]

channelHeader =
    div []
      [ div [whiteFontColor] [text "Cliqz"]
      , div []
        [ div [circleStyle, inlineStyle] []
        , div [inlineStyle, channelPrimaryFontColor] [text "Slack History"]
        ]
      ]

channelNames : (List Channel) -> (Maybe String) -> Html.Html Msg
channelNames channels currentChannel =
    div [] (List.map (\t -> channelName t currentChannel) channels)

channelName : (Channel) -> (Maybe String) -> Html.Html Msg
channelName channel currentChannel =
    case currentChannel of
      Just c ->
        div [sansSerifFont, channelNamesStyle, if channel.name == c then whiteFontColor else channelPrimaryFontColor, onClick (Channel channel)]
          [ text ("#" ++ channel.name)
          ]
      Nothing ->
        div [sansSerifFont, channelNamesStyle, channelPrimaryFontColor, onClick (Channel channel)]
          [ text ("#" ++ channel.name)
          ]


messagesComponent messages =
    div [inlineStyle, sclollableContainerStyle, messageContainerStyle]
    [ --searchBarComponent
    --,
      div [] (List.map (\t -> messageCell t) messages)
    ]

searchBarComponent =
    input [placeholder "filter", autofocus True, onInput FilterMessages] []

messageCell message =
    div [sansSerifFont, messagesStyle]
      [ text message
      ]

-- * STYLES

sclollableContainerStyle =
  style
    [ ("left", "0")
    , ("right", "0")
    , ("overflow-y", "scroll")
    , ("height", "100vh")
    ]

channelsStyle =
  style
    [ ("width", "20%")
    , ("float", "left")
    ]

channelNamesStyle =
  style
    [ ("font-size", "16px")
    , ("text-align", "left")
    ]

channelPrimaryBgColor =
  style
    [("background-color", "#4D394B")]

channelPrimaryFontColor =
  style
    [("color", "#AB9BA9")]

whiteFontColor =
  style
   [("color", "#FFFFFF")]

messageContainerStyle =
  style
    [ ("width", "80%")
    , ("float", "right")
    , ("background-color", "#FFFFFF")
    ]

messagesStyle =
  style
    [ ("font-size", "14px")
    , ("text-align", "left")
    ]

circleStyle =
  style
    [ ("background-color", "#38978D")
    , ("width", "10px")
    , ("height", "10px")
    , ("border-radius", "50%")
    ]

inlineStyle =
  style
    [ ("display", "inline-block")
    ]

sansSerifFont =
  style
    [ ("font-family", "Sans-Serif")
    ]
