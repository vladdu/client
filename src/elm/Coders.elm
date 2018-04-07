module Coders exposing (..)

import Json.Encode as Enc
import Json.Decode as Json exposing (..)
import Dict exposing (Dict)
import Html5.DragDrop as DragDrop

import Document exposing (..)
import Trees exposing (..)
import Objects exposing (..)



-- Tree

treeToValue : Tree -> Enc.Value
treeToValue tree =
  case tree.children of
    Children c ->
      Enc.object
        [ ( "id", Enc.string tree.id )
        , ( "content", Enc.string tree.content )
        , ( "children", Enc.list (List.map treeToValue c))
        ]


treesModelDecoder : Decoder Trees.Model
treesModelDecoder =
  Json.map2 Trees.Model
    treeDecoder
    (succeed [])


treeDecoder : Decoder Tree
treeDecoder =
  Json.map3 Tree
    (field "id" string)
    (field "content" string)
    (oneOf  [ ( field
                "children"
                ( list (lazyRecurse (\_ -> treeDecoder))
                  |> Json.map Children
                )
              )
            , succeed (Children [])
            ]
    )


treeListDecoder : Decoder (List (String, String))
treeListDecoder =
  list (tupleDecoder string string)




-- ViewState

viewStateToValue : ViewState -> Enc.Value
viewStateToValue vs =
  Enc.object
    [ ( "active", Enc.string vs.active )
    , ( "activePast", Enc.list (List.map Enc.string vs.activePast) )
    , ( "activeFuture", Enc.list (List.map Enc.string vs.activeFuture) )
    , ( "descendants", Enc.list (List.map Enc.string vs.descendants) )
    , ( "editing", maybeToValue Enc.string vs.editing )
    ]


viewStateDecoder : Decoder ViewState
viewStateDecoder =
  Json.map8 ViewState
    (field "active" string)
    (field "activePast" (list string))
    (field "activeFuture" (list string))
    (field "descendants" (list string))
    (maybe (field "editing" string))
    (succeed DragDrop.init)
    (succeed Nothing)
    (succeed [])


collabStateToValue : CollabState -> Enc.Value
collabStateToValue collabState =
  Enc.object
    [ ( "uid", Enc.string collabState.uid )
    , ( "mode", modeToValue collabState.mode )
    , ( "field", Enc.string collabState.field )
    ]


collabStateDecoder : Decoder CollabState
collabStateDecoder =
  Json.map3 CollabState
    (field "uid" string)
    (field "mode" modeDecoder)
    (field "field" string)



-- Objects


objectsToValue : Objects.Model -> Enc.Value
objectsToValue model =
  let
    treeObjectToValue sha treeObject =
      Enc.object
        [ ( "_id", Enc.string sha )
        , ( "type", Enc.string "tree" )
        , ( "content", Enc.string treeObject.content )
        , ( "children", Enc.list
              (List.map (\(s, i) -> Enc.list [Enc.string s, Enc.string i]) treeObject.children) )
        ]

    refToValue sha ref =
      Enc.object
        [ ( "_id", Enc.string sha )
        , ( "_rev", Enc.string ref.rev )
        , ( "type", Enc.string "ref" )
        , ( "value", Enc.string ref.value)
        , ( "ancestors", Enc.list (ref.ancestors |> List.map Enc.string) )
        ]

    commits =
      commitsToValue model.commits

    treeObjects =
      Dict.toList model.treeObjects
        |> List.map (\(k, v) -> treeObjectToValue k v)
        |> Enc.list

    refs =
      Dict.toList model.refs
        |> List.map (\(k, v) -> refToValue k v)
        |> Enc.list
  in
  Enc.object
    [ ( "commits", commits )
    , ( "treeObjects", treeObjects )
    , ( "refs", refs )
    ]


objectsDecoder : Json.Decoder Objects.Model
objectsDecoder =
  Json.map4 Objects.Model
    ( Json.field "commits" commitsDecoder )
    ( Json.field "treeObjects" treeObjectsDecoder )
    ( Json.field "refs" refObjectsDecoder )
    ( Json.field "status" statusDecoder )


mergeDecoder : Json.Decoder (Maybe String, Maybe String, Objects.Model)
mergeDecoder =
  Json.map3 (\l r m -> (l, r, m))
    ( Json.index 0 (Json.maybe Json.string) )
    ( Json.index 1 (Json.maybe Json.string) )
    ( Json.index 2 objectsDecoder )


commitsToValue : Dict String CommitObject -> Enc.Value
commitsToValue commits =
  let
    commitToValue sha commit =
      Enc.object
        [ ( "_id", Enc.string sha )
        , ( "type", Enc.string "commit" )
        , ( "tree", Enc.string commit.tree )
        , ( "parents", Enc.list (commit.parents |> List.map Enc.string) )
        , ( "author", Enc.string commit.author )
        , ( "timestamp", Enc.int commit.timestamp )
        ]

  in
  Dict.toList commits
    |> List.map (\(k, v) -> commitToValue k v)
    |> Enc.list


commitsDecoder : Json.Decoder (Dict String CommitObject)
commitsDecoder =
  let
    commitObjectDecoder : Json.Decoder CommitObject
    commitObjectDecoder =
      Json.map4 CommitObject
        ( Json.field "tree" Json.string )
        ( Json.field "parents" (Json.list Json.string) )
        ( Json.field "author" Json.string )
        ( Json.field "timestamp" Json.int )
  in
  (Json.dict commitObjectDecoder)


treeObjectsDecoder : Json.Decoder (Dict String TreeObject)
treeObjectsDecoder =
  let
    tupleDecoder a b =
      Json.index 0 a
        |> Json.andThen
          (\aVal -> Json.index 1 b
              |> Json.andThen (\bVal -> Json.succeed (aVal, bVal))
          )

    treeObjectDecoder =
      Json.map2 TreeObject
        ( Json.field "content" Json.string )
        ( Json.field "children" (Json.list (tupleDecoder Json.string Json.string)) )
  in
  (Json.dict treeObjectDecoder)


refObjectsDecoder : Json.Decoder (Dict String RefObject)
refObjectsDecoder =
  let
    refObjectDecoder =
      Json.map3 RefObject
        ( Json.field "value" Json.string )
        ( Json.field "ancestors" (Json.list Json.string) )
        ( Json.field "_rev" Json.string )
  in
  (Json.dict refObjectDecoder)


changeDecoder : Objects.Model -> Json.Decoder Objects.Model
changeDecoder model =
  Json.oneOf
    [ Json.map4 Objects.Model
        ( Json.succeed model.commits )
        ( Json.succeed model.treeObjects )
        ( Json.succeed model.refs )
        statusDecoder
    , Json.map4 Objects.Model
        ( Json.succeed model.commits )
        ( Json.succeed model.treeObjects )
        refObjectsDecoder
        ( Json.succeed model.status )
    , Json.map4 Objects.Model
        ( Json.succeed model.commits )
        treeObjectsDecoder
        ( Json.succeed model.refs )
        ( Json.succeed model.status )
    , Json.map4 Objects.Model
        commitsDecoder
        ( Json.succeed model.treeObjects )
        ( Json.succeed model.refs )
        ( Json.succeed model.status )
    ]



-- Mode

modeToValue : Mode -> Enc.Value
modeToValue mode =
  case mode of
    Active id -> tupleToValue Enc.string Enc.string ("Active", id)
    Editing id -> tupleToValue Enc.string Enc.string ("Editing", id)


modeDecoder : Decoder Mode
modeDecoder =
  let
    modeHelp : (String, String) -> Decoder Mode
    modeHelp (tag, id) =
      case (tag, id) of
        ("Active", id) -> succeed (Active id)
        ("Editing", id) -> succeed (Editing id)
        _ -> fail <| "Failed mode decoder"
  in
  tupleDecoder string string
    |> andThen modeHelp




-- Status

statusToValue : Status -> Enc.Value
statusToValue status =
  case status of
    Clean head ->
      Enc.object
        [ ( "_id", "status" |> Enc.string)
        , ( "status", "clean" |> Enc.string)
        , ( "head", head |> Enc.string)
        ]

    MergeConflict tree aSha bSha conflicts ->
      Enc.object
        [ ( "_id", "status" |> Enc.string)
        , ( "status", "merge-conflict" |> Enc.string)
        , ( "tree", tree |> treeToValue)
        , ( "aSha", aSha |> Enc.string)
        , ( "bSha", bSha |> Enc.string)
        , ( "conflicts", Enc.list (List.map conflictToValue conflicts) )
        ]

    Bare ->
      Enc.object
        [ ( "_id", "status" |> Enc.string)
        , ( "status", "bare" |> Enc.string)
        , ( "bare", Enc.bool True )
        ]


statusDecoder : Decoder Status
statusDecoder =
  let
    cleanDecoder =
      Json.map (\h -> Clean h)
        ( field "head" string)

    mergeConflictDecoder =
      Json.map4 (\t a b c -> MergeConflict t a b c)
        ( field "tree" treeDecoder)
        ( field "aSha" string )
        ( field "bSha" string )
        ( field "conflicts" (list conflictDecoder))

    bareDecoder =
      Json.map (\_ -> Bare)
        ( field "bare" bool )

  in
  oneOf
    [ cleanDecoder
    , mergeConflictDecoder
    , bareDecoder
    ]




-- Conflict

conflictToValue : Conflict -> Enc.Value
conflictToValue {id, opA, opB, selection, resolved} =
  Enc.object
    [ ("id", Enc.string id)
    , ("opA", opToValue opA)
    , ("opB", opToValue opB)
    , ("selection", selectionToValue selection)
    , ("resolved", Enc.bool resolved)
    ]


conflictDecoder : Decoder Conflict
conflictDecoder =
  Json.map5 Conflict
    ( field "id" string )
    ( field "opA" opDecoder )
    ( field "opB" opDecoder )
    ( field "selection" selectionDecoder )
    ( field "resolved" bool )




-- Op

opToValue : Op -> Enc.Value
opToValue op =
  case op of
    Mod id parents str orig ->
      Enc.list (
        (["mod", id, str, orig] |> List.map Enc.string)
        ++ [ parents |> List.map Enc.string |> Enc.list ]
      )

    Del id parents ->
      Enc.list (
        (["del", id] |> List.map Enc.string)
        ++ [ parents |> List.map Enc.string |> Enc.list ]
      )

    _ ->
      Enc.null


opDecoder : Decoder Op
opDecoder =
  let
    modDecoder =
      Json.map4 (\id str orig parents -> Mod id parents str orig)
        ( index 1 string )
        ( index 2 string )
        ( index 3 string )
        ( index 4 (list string) )

    delDecoder =
      Json.map2 (\id parents-> Del id parents)
        ( index 1 string)
        ( index 2 (list string) )
  in
  oneOf
    [ modDecoder
    , delDecoder
    ]




-- Selection

selectionToValue : Selection -> Enc.Value
selectionToValue selection =
  case selection of
    Ours -> "ours" |> Enc.string
    Theirs -> "theirs" |> Enc.string
    Original -> "original" |> Enc.string
    Manual -> "manual" |> Enc.string


selectionDecoder : Decoder Selection
selectionDecoder =
  let
    fn s =
      case s of
        "ours" -> Ours
        "theirs" -> Theirs
        "original" -> Original
        "manual" -> Manual
        _ -> Manual
  in
  Json.map fn string




-- EXPORT ENCODINGS

treeToJSON : Tree -> Enc.Value
treeToJSON tree =
  case tree.children of
    Children c ->
      Enc.list (List.map treeToJSONrecurse c)


treeToJSONrecurse : Tree -> Enc.Value
treeToJSONrecurse tree =
  case tree.children of
    Children c ->
      Enc.object
        [ ( "content", Enc.string tree.content )
        , ( "children", Enc.list (List.map treeToJSONrecurse c) )
        ]


treeToMarkdown : Bool -> Tree -> Enc.Value
treeToMarkdown withRoot tree =
  tree
    |> treeToMarkdownString withRoot
    |> Enc.string


treeToMarkdownString : Bool -> Tree -> String
treeToMarkdownString withRoot tree =
  let
    contentList =
      case tree.children of
        Children c ->
          List.map treeToMarkdownRecurse c
  in
  if withRoot then
    tree.content :: contentList
      |> String.join "\n\n"
  else
    contentList
      |> String.join "\n\n"


treeToMarkdownRecurse : Tree -> String
treeToMarkdownRecurse tree =
  case tree.children of
    Children c ->
      [tree.content] ++ (List.map treeToMarkdownRecurse c)
        |> String.join "\n\n"




-- HELPERS

lazyRecurse : (() -> Decoder a) -> Decoder a
lazyRecurse thunk =
  let
    toResult =
      (\js -> decodeValue (thunk ()) js)
  in
  andThen
    (\a ->
      case toResult a of
        Ok b -> succeed b
        Err err -> fail err
    )
    value


maybeToValue : (a -> Enc.Value) -> Maybe a  -> Enc.Value
maybeToValue encoder mb =
  case mb of
    Nothing -> Enc.null
    Just v -> encoder v


tupleToValue : (a -> Enc.Value) -> (b -> Enc.Value) -> (a, b) -> Enc.Value
tupleToValue aEnc bEnc (aVal, bVal) =
  Enc.list [ aEnc aVal, bEnc bVal ]


tripleToValue : (a -> Enc.Value) -> (b -> Enc.Value) -> (c -> Enc.Value) -> (a, b, c) -> Enc.Value
tripleToValue aEnc bEnc cEnc (aVal, bVal, cVal) =
  Enc.list [ aEnc aVal, bEnc bVal, cEnc cVal ]


tupleDecoder : Decoder a -> Decoder b -> Decoder (a, b)
tupleDecoder a b =
  index 0 a
    |> andThen
      (\aVal -> index 1 b
          |> andThen (\bVal -> succeed (aVal, bVal))
      )


tripleDecoder : Decoder a -> Decoder b -> Decoder c -> Decoder (a, b, c)
tripleDecoder a b c =
  index 0 a
    |> andThen
      (\aVal -> index 1 b
          |> andThen (\bVal -> index 2 c
                          |> andThen (\cVal -> succeed (aVal, bVal, cVal))
                      )
      )
