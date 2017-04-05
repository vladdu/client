module Coders exposing (..)

import Types exposing (..)
import Json.Encode
import Json.Decode as Json exposing (..)
import Array exposing (fromList)
import Dict exposing (Dict)

import Trees


type alias Model =
  { data : Trees.Model
  , treePast : List Tree
  , treeFuture : List Tree
  , viewState : ViewState
  , saved : Bool
  , filepath : Maybe String
  }


-- ENCODERS

modelToValue : Model -> Json.Encode.Value
modelToValue model =
  Json.Encode.object
   [ ("vertices", verticesToValue model.data.vertices)
   , ("edges", edgesToValue model.data.edges)
   , ("treePast", Json.Encode.list (List.map treeToValue model.treePast))
   , ("treeFuture", Json.Encode.list (List.map treeToValue model.treeFuture))
   , ("viewState", viewStateToValue model.viewState)
   , ("filepath", maybeToValue model.filepath Json.Encode.string )
   ]


treeToValue : Tree -> Json.Encode.Value
treeToValue tree =
  case tree.children of
    Children c ->
      Json.Encode.object
        [ ( "id", Json.Encode.string tree.id )
        , ( "content", Json.Encode.string tree.content )
        , ( "children", Json.Encode.list (List.map treeToValue c))
        , ( "deleted", Json.Encode.bool tree.deleted )
        ]


viewStateToValue : ViewState -> Json.Encode.Value
viewStateToValue vs =
  Json.Encode.object
    [ ( "active", Json.Encode.string vs.active )
    , ( "activePast", Json.Encode.list (List.map Json.Encode.string vs.activePast) )
    , ( "activeFuture", Json.Encode.list (List.map Json.Encode.string vs.activeFuture) )
    , ( "descendants", Json.Encode.list (List.map Json.Encode.string vs.descendants) )
    , ( "editing", maybeToValue vs.editing Json.Encode.string )
    ]


nodesToValue : Dict String TreeNode -> Json.Encode.Value
nodesToValue nodes =
  Dict.toList nodes
    |> List.map (\(k, v) -> (k, treeNodeToValue v))
    |> Json.Encode.object


treeNodeToValue : TreeNode -> Json.Encode.Value
treeNodeToValue treeNode =
  Json.Encode.object
    [ ( "content", Json.Encode.string treeNode.content )
    , ( "children", Json.Encode.list (List.map Json.Encode.string treeNode.children) )
    , ( "rev", maybeToValue treeNode.rev Json.Encode.string )
    , ( "deleted", Json.Encode.bool treeNode.deleted )
    ]


verticesToValue : Dict String Vertex -> Json.Encode.Value
verticesToValue vertices =
  Dict.toList vertices
    |> List.map (\(k, v) -> (k, vertexToValue v))
    |> Json.Encode.object


vertexToValue : Vertex -> Json.Encode.Value
vertexToValue vertex =
  Json.Encode.object
    [ ( "content", Json.Encode.string vertex.content )
    , ( "rev", maybeToValue vertex.rev Json.Encode.string )
    ]


edgesToValue : Dict String Edge -> Json.Encode.Value
edgesToValue edges =
  Dict.toList edges
    |> List.map (\(k, v) -> (k, edgeToValue v))
    |> Json.Encode.object


edgeToValue : Edge -> Json.Encode.Value
edgeToValue edge =
  Json.Encode.object
    [ ( "rev", maybeToValue edge.rev Json.Encode.string )
    , ( "from", Json.Encode.string edge.from )
    , ( "to", Json.Encode.string edge.to )
    , ( "modified", Json.Encode.int edge.modified )
    ]




-- EXPORT ENCODINGS

treeToSimpleJSON : Tree -> Json.Encode.Value
treeToSimpleJSON tree =
  case tree.children of
    Children c ->
      Json.Encode.array 
      ( fromList
        [ Json.Encode.object
          [ ( "content", Json.Encode.string tree.content )
          , ( "children", Json.Encode.array (fromList (List.map treeToSimpleJSON c)))
          ]
        ]
      )





-- DECODERS

modelDecoder : Decoder Model
modelDecoder =
  Json.map6 Model
    (field "tree" treesModelDecoder)
    (oneOf [field "treePast" (list treeDecoder), succeed []])
    (oneOf [field "treeFuture" (list treeDecoder), succeed []])
    (field "viewState" viewStateDecoder)
    ( succeed True )
    (maybe (field "filepath" string))


treesModelDecoder : Decoder Trees.Model
treesModelDecoder =
  Json.map5 Trees.Model
    treeDecoder
    (succeed [])
    (succeed Dict.empty)
    (succeed Dict.empty)
    (succeed Dict.empty)


treeDecoder : Decoder Tree
treeDecoder =
  Json.map5 Tree
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
    (field "rev" (maybe string))
    (field "deleted" bool)


viewStateDecoder : Decoder ViewState
viewStateDecoder =
  Json.map5 ViewState
    (field "active" string)
    (field "activePast" (list string))
    (field "activeFuture" (list string))
    (field "descendants" (list string))
    (maybe (field "editing" string))


nodesDecoder : Decoder (Dict String TreeNode)
nodesDecoder =
  (dict treeNodeDecoder)


treeNodeDecoder : Decoder TreeNode
treeNodeDecoder =
  Json.map4 TreeNode
    (field "content" string)
    (field "children" (list string))
    (field "rev" (maybe string))
    (field "deleted" bool)


verticesDecoder : Decoder (Dict String Vertex)
verticesDecoder =
  (dict vertexDecoder)


vertexDecoder : Decoder Vertex
vertexDecoder =
  Json.map2 Vertex
    (field "rev" (maybe string))
    (field "content" string)


edgesDecoder : Decoder (Dict String Edge)
edgesDecoder =
  (dict edgeDecoder)


edgeDecoder : Decoder Edge
edgeDecoder =
  Json.map4 Edge
    (field "rev" (maybe string))
    (field "from" string)
    (field "to" string)
    (field "modified" int)
 



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


maybeToValue : Maybe a -> (a -> Json.Encode.Value) -> Json.Encode.Value
maybeToValue mb encoder =
  case mb of
    Nothing -> Json.Encode.null
    Just v -> encoder v
