port module Ports exposing (..)


import Types exposing (..)
import Coders exposing (..)
import TreeUtils exposing (getColumn)
import Json.Encode exposing (..)
import Json.Decode exposing (decodeValue)


sendOut : OutgoingMsg -> Cmd msg
sendOut info =
  let
    dataToSend = encodeAndSend info
  in
  case info of
    -- === Dialogs, Menus, Window State ===
    Alert str ->
      dataToSend ( string str )

    ChangeTitle filepath_ changed ->
      dataToSend ( tupleToValue ( maybeToValue string ) bool ( filepath_, changed ) )

    OpenDialog filepath_ ->
      dataToSend ( maybeToValue string filepath_ )

    ConfirmClose filepath_ callbackTag ->
      dataToSend
        ( object
            [ ( "filepath", maybeToValue string filepath_ )
            , ( "callback", string callbackTag )
            ]
        )

    ConfirmExit filepath_ ->
      dataToSend ( maybeToValue string filepath_ )

    ConfirmCancelCard id origContent ->
      dataToSend ( list [ string id, string origContent ] )

    ColumnNumberChange cols ->
      dataToSend ( int cols )

    Exit ->
      dataToSend null

    -- === Database ===

    ClearDB ->
      dataToSend ( null )

    SaveToDB ( statusValue, objectsValue ) ->
      dataToSend ( list [ statusValue, objectsValue ] )

    SaveLocal tree ->
      dataToSend ( treeToValue tree )

    Push ->
      dataToSend null

    Pull ->
      dataToSend null

    -- === File System ===

    Save filepath_ ->
      dataToSend ( maybeToValue string filepath_ )

    ExportJSON tree ->
      dataToSend ( treeToJSON tree )

    ExportTXT withRoot tree ->
      dataToSend ( treeToMarkdown withRoot tree )

    ExportTXTColumn col tree ->
      dataToSend
        ( tree
            |> getColumn col
            |> Maybe.withDefault [[]]
            |> List.concat
            |> List.map .content
            |> String.join "\n\n"
            |> string
        )

    -- === DOM ===

    ActivateCards (cardId, col, cardIds) ->
      let
        listListStringToValue lls =
          lls
            |> List.map (List.map string)
            |> List.map list
            |> list
      in
      dataToSend ( tripleToValue string int listListStringToValue ( cardId, col, cardIds ) )

    GetContent id ->
      dataToSend ( string id )

    SurroundText id str ->
      dataToSend ( list [ string id, string str ] )

    -- === UI ===

    UpdateCommits ( objectsValue, head_ ) ->
      let
        headToValue mbs =
          case mbs of
            Just str -> string str
            Nothing -> null
      in
      dataToSend ( tupleToValue identity headToValue ( objectsValue, head_ ) )

    SetVideoModal isOpen ->
      dataToSend ( bool isOpen )

    SetShortcutTray isOpen ->
      dataToSend ( bool isOpen )

    -- === Misc ===

    SocketSend collabState ->
      dataToSend ( collabStateToValue collabState )

    ConsoleLogRequested err ->
      dataToSend ( string err )




receiveMsg : (IncomingMsg -> msg) -> (String -> msg) -> Sub msg
receiveMsg tagger onError =
  infoForElm
    (\outsideInfo ->
        case outsideInfo.tag of
          "NewConfirmed" ->
            tagger <| NewConfirmed

          "OpenConfirmed" ->
            tagger <| OpenConfirmed

          "IntentExit" ->
            tagger <| IntentExit

          "ContentIn" ->
            case decodeValue ( tupleDecoder Json.Decode.string Json.Decode.string ) outsideInfo.data of
              Ok (id, str) ->
                tagger <| ContentIn (id, str)

              Err e ->
                onError e

          "CancelCardConfirmed" ->
            tagger <| CancelCardConfirmed

          "Load" ->
            case decodeValue ( tripleDecoder Json.Decode.string Json.Decode.value (Json.Decode.maybe Json.Decode.string) ) outsideInfo.data of
              Ok ( filepath, json, lastActive_ ) ->
                tagger <| Load (filepath, json, lastActive_ |> Maybe.withDefault "1" )

              Err e ->
                onError e

          "Merge" ->
            tagger <| Merge outsideInfo.data

          "ImportJSON" ->
            tagger <| ImportJSON outsideInfo.data

          "CheckoutCommit" ->
            case decodeValue Json.Decode.string outsideInfo.data of
              Ok commitSha ->
                tagger <| CheckoutCommit commitSha

              Err e ->
                onError e

          "SetHeadRev" ->
            case decodeValue Json.Decode.string outsideInfo.data of
              Ok rev ->
                tagger <| SetHeadRev rev

              Err e ->
                onError e

          "FileState" ->
            let decoder = tupleDecoder (Json.Decode.maybe Json.Decode.string) Json.Decode.bool in
            case decodeValue decoder outsideInfo.data of
              Ok (filepath_, changed) ->
                tagger <| FileState filepath_ changed

              Err e ->
                onError e

          "RecvCollabState" ->
            case decodeValue collabStateDecoder outsideInfo.data of
              Ok collabState ->
                tagger <| RecvCollabState collabState

              Err e ->
                onError e

          "CollaboratorDisconnected" ->
            case decodeValue Json.Decode.string outsideInfo.data of
              Ok uid ->
                tagger <| CollaboratorDisconnected uid

              Err e ->
                onError e

          "DoExportJSON" ->
            tagger <| DoExportJSON

          "DoExportTXT" ->
            case decodeValue Json.Decode.int outsideInfo.data of
              Ok col ->
                tagger <| DoExportTXTColumn col

              Err e ->
                tagger <| DoExportTXT

          "DoExportTXTCurrent" ->
              tagger <| DoExportTXTCurrent

          "ViewVideos" ->
            tagger <| ViewVideos

          "Keyboard" ->
            case decodeValue Json.Decode.string outsideInfo.data of
              Ok shortcut ->
                tagger <| Keyboard shortcut

              Err e ->
                onError e

          _ ->
            Debug.crash ("Unexpected info from outside: " ++ toString outsideInfo)
            -- onError <| "Unexpected info from outside: " ++ toString outsideInfo
    )


encodeAndSend : OutgoingMsg -> Json.Encode.Value -> Cmd msg
encodeAndSend info data =
  let
    tagName =
      info
        |> toString
        |> String.words
        |> List.head
        |> Maybe.withDefault (info |> toString)
  in
  infoForOutside { tag = tagName, data = data }


port infoForOutside : OutsideData -> Cmd msg

port infoForElm : (OutsideData -> msg) -> Sub msg
