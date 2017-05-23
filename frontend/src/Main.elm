
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

channelComponent channels currentChannel =
    div [inlineStyle, sclollableContainerStyle, channelsStyle, channelPrimaryBgColor]
      [ channelHeader
      , channelNames channels currentChannel
      ]

channelHeader =
    div [channelHeaderStyle]
      [ div [whiteFontColor, boldFontStyle, sansSerifFont, cliqzStyle] [text "Cliqz"]
      , div []
        [ div [circleStyle, inlineStyle] []
        , div [inlineStyle, channelPrimaryFontColor, sansSerifFont, style [("margin-left", "6px"), ("font-size", "14px")]] [text "Slack History"]
        ]
      ]

channelNames channels currentChannel =
    div [style [("margin-top", "14px")]]
      [ div [channelPrimaryFontColor, mediumLightFontStyle, sansSerifFont, style [("font-size", "12px"), ("margin-bottom", "8px"), ("padding-left", "17px")]] [text ("CHANNELS (" ++ (toString (List.length channels) ++ ")"))]
      , div [] (List.map (\t -> channelName t currentChannel) channels)
      ]

channelName channel currentChannel =
    case currentChannel of
      Just c ->
        div [sansSerifFont, channelNamesStyle, if channel.name == c then channelNameSelectedStyle else channelPrimaryFontColor, onClick (Channel channel)]
          [ text ("# " ++ channel.name)
          ]
      Nothing ->
        div [sansSerifFont, channelNamesStyle, channelPrimaryFontColor, onClick (Channel channel)]
          [ text ("# " ++ channel.name)
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
    [ ("width", "16%")
    , ("float", "left")
    ]

channelHeaderStyle =
  style
    [ ("padding-left", "17px")
    , ("margin-top", "14px")
    , ("margin-bottom", "10px")
    ]

boldFontStyle =
  style
    [("font-weight", "700")]

mediumLightFontStyle =
  style
    [("font-weight", "600")]

cliqzStyle =
  style
    [("margin-bottom", "4px")]

channelNamesStyle =
  style
    [ ("font-size", "16px")
    , ("text-align", "left")
    , ("margin-right", "20px")
    , ("padding-top", "3px")
    , ("padding-bottom", "3px")
    , ("padding-left", "22px")
    ]
channelNameSelectedStyle =
  style
    [ ("color", "#FFFFFF")
    , ("background", "#4C9689")
    , ("border-top-right-radius", "5px")
    , ("border-bottom-right-radius", "5px")
    ]

channelPrimaryBgColor =
  style
    [("background-color", "#4D394B")]

channelPrimaryFontColor =
  style
    [("color", "#AB9BA9")]

channelSecondaryFontColor =
  style
    [("color", "#7C6A7A")]

channelHoverColor =
  style
    [("color", "#3E313C")]

whiteFontColor =
  style
   [("color", "#FFFFFF")]

messageContainerStyle =
  style
    [ ("width", "84%")
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
