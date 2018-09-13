port module Sounds exposing (..)


startSession =
    "sounds/start-session.mp3"


endSession =
    "sounds/end-session.mp3"


stopSession =
    "sounds/stop-session.mp3"


pauseSession =
    "sounds/pause-session.mp3"


resumeSession =
    "sounds/resume-session.mp3"


tabataComplete =
    "sounds/tabata-complete.mp3"


countdown =
    "sounds/countdown.mp3"


start =
    "sounds/start.mp3"


end =
    "sounds/end.mp3"


work =
    "sounds/work.mp3"


rest =
    "sounds/rest.mp3"


all =
    [ startSession
    , endSession
    , stopSession
    , pauseSession
    , resumeSession
    , tabataComplete
    , countdown
    , start
    , end
    , work
    , rest
    ]


playSounds : List ( Bool, List String ) -> Cmd msg
playSounds conditions =
    case conditions of
        [] ->
            Cmd.none

        first :: tails ->
            if (Tuple.first first) then
                play (Tuple.second first)
            else
                playSounds tails


port preload : List String -> Cmd msg


port play : List String -> Cmd msg
