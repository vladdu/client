module Document exposing (..)


import Trees exposing (..)
import Objects

import Tuple exposing (first, second)
--import Regex
import Html exposing (..)
import Html.Attributes exposing (..)
--import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
--import Html.Keyed as Keyed
import Html5.DragDrop as DragDrop
import Sha1 exposing (timeJSON)
--import Markdown


-- MODEL


type alias Model =
  { workingTree : Trees.Model
  , objects : Objects.Model
  , uid : String
  , viewState : ViewState
  }


default : Model
default =
  { workingTree = Trees.defaultModel
  , objects = Objects.defaultModel
  , uid = timeJSON ()
  , viewState =
      { active = "1"
      , activePast = []
      , activeFuture = []
      , descendants = []
      , editing = Nothing
      , dragModel = DragDrop.init
      , draggedTree = Nothing
      , collaborators = []
      }
  }


type alias ViewState =
  { active : String
  , activePast : List String
  , activeFuture : List String
  , descendants : List String
  , editing : Maybe String
  , dragModel : DragDrop.Model String DropId
  , draggedTree : Maybe (Tree, String, Int)
  , collaborators : List CollabState
  }


type alias VisibleViewState =
  { active : String
  , editing : Maybe String
  , descendants : List String
  , dragModel : DragDrop.Model String DropId
  , collaborators : List CollabState
  }


type Mode = Active String | Editing String


type DropId = Above String | Below String | Into String


type alias CollabState =
  { uid : String
  , mode : Mode
  , field : String
  }


-- UPDATE


type Msg
    = NoOp
    -- === Card Activation ===
--    | Activate String
    -- === Card Editing  ===
    | OpenCard String String
--    | DeleteCard String
--    -- === Card Insertion  ===
--    | InsertAbove String
--    | InsertBelow String
--    | InsertChild String
--    -- === Card Moving  ===
--    | DragDropMsg (DragDrop.Msg String DropId)
--    -- === History ===
--    | Undo
--    | Redo
--    | Sync
--    | SetSelection String Selection String
--    | Resolve String


update : Msg -> Model -> Model
update msg model =
  case msg of
    -- === Card Activation ===
--
--    Activate id ->
--      case vs.editing of
--        Just eid ->
--          model ! [ sendOut ( GetContent eid ) ]
--            |> cancelCard
--            |> activate id
--
--        Nothing ->
--          model ! []
--            |> activate id
--
--    -- === Card Editing  ===
--
    OpenCard id str ->
      model
        |> openCard id str

--    DeleteCard id ->
--      model ! []
--        |> deleteCard id
--
--    -- === Card Insertion  ===
--
--    InsertAbove id ->
--      model ! []
--        |> insertAbove id
--
--    InsertBelow id ->
--      model ! []
--        |> insertBelow id
--
--    InsertChild id ->
--      model ! []
--        |> insertChild id
--
--    -- === Card Moving  ===
--
--    DragDropMsg dragDropMsg ->
--      let
--        ( newDragModel, dragResult_ ) =
--          DragDrop.update dragDropMsg vs.dragModel
--      in
--      case (vs.draggedTree, DragDrop.getDragId newDragModel, dragResult_ ) of
--        -- Start drag
--        ( Nothing, Just dragId, Nothing ) ->
--          { model
--            | workingTree = Trees.update (Trees.Rmv dragId) model.workingTree
--            , viewState =
--              { vs
--                | dragModel = newDragModel
--                , draggedTree = getTreeWithPosition dragId model.workingTree.tree
--              }
--          }
--          ! []
--
--        -- Successful drop
--        ( Just (draggedTree, _, _), Nothing, Just (dragId, dropId) ) ->
--          let
--            moveOperation =
--              case dropId of
--                Into id ->
--                  move draggedTree id 999999
--
--                Above id ->
--                  move draggedTree
--                    ( ( getParent id model.workingTree.tree |> Maybe.map .id ) ? "0" )
--                    ( ( getIndex id model.workingTree.tree ? 0 ) |> Basics.max 0 )
--
--                Below id ->
--                  move draggedTree
--                    ( ( getParent id model.workingTree.tree |> Maybe.map .id ) ? "0" )
--                    ( ( getIndex id model.workingTree.tree ? 0 ) + 1)
--          in
--          { model | viewState =
--            { vs
--              | dragModel = newDragModel
--              , draggedTree = Nothing
--            }
--          } ! []
--            |> moveOperation
--            |> activate draggedTree.id
--
--        -- Failed drop
--        ( Just (draggedTree, parentId, idx), Nothing, Nothing ) ->
--          { model | viewState =
--            { vs
--              | dragModel = newDragModel
--              , draggedTree = Nothing
--            }
--          } ! []
--            |> move draggedTree parentId idx
--            |> activate draggedTree.id
--
--        _ ->
--          { model | viewState = { vs | dragModel = newDragModel } } ! []
--
--    -- === History ===
--
--    Undo ->
--      model ! []
--
--    Redo ->
--      model ! []
--
--    Sync ->
--      case (model.status, model.online) of
--        (Clean _, True) ->
--          model ! [ sendOut Pull ]
--
--        (Bare, True) ->
--          model ! [ sendOut Pull ]
--
--        _ ->
--          model ! []
--
--    SetSelection cid selection id ->
--      let
--        newStatus =
--          case status of
--            MergeConflict mTree oldHead newHead conflicts ->
--              conflicts
--                |> List.map (\c -> if c.id == cid then { c | selection = selection } else c)
--                |> MergeConflict mTree oldHead newHead
--
--            _ ->
--              status
--
--      in
--      case newStatus of
--        MergeConflict mTree oldHead newHead conflicts ->
--          case selection of
--            Manual ->
--              { model
--                | workingTree = Trees.setTreeWithConflicts conflicts mTree model.workingTree
--                , status = newStatus
--              }
--                ! []
--
--            _ ->
--              { model
--                | workingTree = Trees.setTreeWithConflicts conflicts mTree model.workingTree
--                , status = newStatus
--              }
--                ! []
--                |> cancelCard
--                |> activate id
--
--        _ ->
--          model ! []
--
--    Resolve cid ->
--      case status of
--        MergeConflict mTree shaA shaB conflicts ->
--          { model
--            | status = MergeConflict mTree shaA shaB (conflicts |> List.filter (\c -> c.id /= cid))
--          }
--            ! []
--            |> addToHistory
--
--        _ ->
--          model ! []
--
    -- === Help ===

    NoOp -> model


openCard : String -> String -> Model -> Model
openCard id str model =
  let vs = model.viewState in
  if isLocked vs.collaborators id then
    model
  else
    { model
      | viewState = { vs | active = id, editing = Just id }
    }


isLocked : List CollabState -> String -> Bool
isLocked collaborators id =
  collaborators
    |> List.filter (\c -> c.mode == Editing id)
    |> (not << List.isEmpty)



-- VIEW


view : Model -> Html Msg
view {viewState, workingTree} =
  let
    columnsWithDepth =
      workingTree.columns
        |> List.indexedMap (\i c -> (c, i))
        |> List.drop 1

    getViewArgs cwd =
      let
        editing_ =
          case viewState.editing of
            Nothing ->
              Nothing

            Just editId ->
              if (first cwd |> List.concat |> List.map .id |> List.member editId ) then
                Just editId
              else
                Nothing
      in
      VisibleViewState
        viewState.active
        editing_
        viewState.descendants
        viewState.dragModel
        viewState.collaborators

    columns =
      [([[]], -1)] ++
      columnsWithDepth ++
      [([[]], List.length columnsWithDepth)]
        |> List.map (\t -> lazy3 viewColumn (getViewArgs t) (second t) (first t))
  in
  div [ id "app"
      ]
    ( columns
    )

viewColumn : VisibleViewState -> Int -> Column -> Html Msg
viewColumn vstate depth col =
  let
    buffer =
      [div [ class "buffer" ][]]
  in
  div
    [ class "column" ]
    ( buffer ++
      --(List.map (lazy3 viewGroup vstate depth) col) ++
      buffer
    )


{-
viewGroup : VisibleViewState -> Int -> Group -> Html Msg
viewGroup vstate depth xs =
  let
    firstChild =
      xs
        |> List.head
        |> Maybe.withDefault defaultTree
        |> .id

    lastChild =
      xs
        |> List.reverse
        |> List.head
        |> Maybe.withDefault defaultTree
        |> .id

    isActiveDescendant =
      vstate.descendants
        |> List.member firstChild

    viewFunction t =
      let
        isActive =
          t.id == vstate.active

        isEditing =
          case vstate.editing of
            Just editId ->
              t.id == editId

            Nothing ->
              False

        isLast =
          t.id == lastChild

        isCollabActive =
          vstate.collaborators
            |> List.map .mode
            |> List.member (Active t.id)

        collabsEditing =
          vstate.collaborators
            |> List.filter (\c -> c.mode == Editing t.id)
            |> List.map .uid

        collaborators =
          vstate.collaborators
            |> List.filter (\c -> c.mode == Active t.id || c.mode == Editing t.id)
            |> List.map .uid
      in
      viewKeyedCard (isActive, isEditing, depth, isLast, collaborators, collabsEditing, vstate.dragModel) t
  in
    Keyed.node "div"
      [ classList [ ("group", True)
                  , ("active-descendant", isActiveDescendant)
                  ]
      ]
      (List.map viewFunction xs)


viewKeyedCard : (Bool, Bool, Int, Bool, List String, List String, DragDrop.Model String DropId) -> Tree -> (String, Html Msg)
viewKeyedCard tup tree =
  (tree.id, lazy2 viewCard tup tree)


viewCard : (Bool, Bool, Int, Bool, List String, List String, DragDrop.Model String DropId) -> Tree -> Html Msg
viewCard (isActive, isEditing, depth, isLast, collaborators, collabsEditing, dragModel) tree =
  let
    hasChildren =
      case tree.children of
        Children c ->
          ( c
              |> List.length
          ) /= 0

    tarea content =
      textarea
        [ id ( "card-edit-" ++ tree.id )
        , classList [ ("edit", True)
                    , ("mousetrap", True)
                    ]
        , defaultValue content
        ]
        []

    buttons =
      case (isEditing, isActive) of
        ( False, True ) ->
          [ div [ class "flex-row card-top-overlay"]
                [ span
                  [ class "card-btn ins-above"
                  , title "Insert Above (Ctrl+K)"
                  , onClick (InsertAbove tree.id)
                  ]
                  [ text "+" ]
                ]
          , div [ class "flex-column card-right-overlay"]
                [ span
                  [ class "card-btn delete"
                  , title "Delete Card (Ctrl+Backspace)"
                  , onClick (DeleteCard tree.id)
                  ]
                  []
                , span
                  [ class "card-btn ins-right"
                  , title "Add Child (Ctrl+L)"
                  , onClick (InsertChild tree.id)
                  ]
                  [ text "+" ]
                , span
                  [ class "card-btn edit"
                  , title "Edit Card (Enter)"
                  , onClick (OpenCard tree.id tree.content)
                  ]
                  []
                ]
          , div [ class "flex-row card-bottom-overlay" ]
                [ span
                  [ class "card-btn ins-below"
                  , title "Insert Below (Ctrl+J)"
                  , onClick (InsertBelow tree.id)
                  ]
                  [ text "+" ]
                ]
          ]

        ( True, _ ) ->
          [ div [ class "flex-column card-right-overlay"]
                [ span
                  [ class "card-btn save"
                  , title "Save Changes (Ctrl+Enter)"
                  , onClick (Port (Keyboard "mod+enter"))
                  ]
                  []
                ]
          ]

        _ ->
          []


    dropRegions =
      let
        dragId_ = DragDrop.getDragId dragModel

        dropId_ = DragDrop.getDropId dragModel

        dropDiv str dId =
          div
            ( [ classList
                  [ ("drop-region-"++str, True)
                  , ("drop-hover", dropId_ == Just dId )
                  ]
              ]
              ++ ( DragDrop.droppable DragDropMsg dId )
            )
            []
      in
      case dragId_ of
        Just dragId ->
          [ dropDiv "above" (Above tree.id)
          , dropDiv "into" (Into tree.id)
          ]
          ++ (if isLast then [ dropDiv "below" (Below tree.id) ] else [])

        Nothing ->
          []


    cardAttributes =
      [ id ("card-" ++ tree.id)
      , classList [ ("card", True)
                  , ("active", isActive)
                  , ("editing", isEditing)
                  , ("collab-active", not isEditing && not (List.isEmpty collaborators) )
                  , ("collab-editing", not isEditing && not (List.isEmpty collabsEditing))
                  , ("has-children", hasChildren)
                  ]
      ]
      ++ (if not isEditing then DragDrop.draggable DragDropMsg tree.id else [])
  in
  if isEditing then
    div cardAttributes
      (
        [ tarea tree.content ]
        ++
        buttons
      )
  else
    let
      collabsString =
        collaborators
          |> List.map (\c -> if List.member c collabsEditing then c ++ " is editing" else c)
          |> String.join(", ")
    in
    div cardAttributes
      (
        buttons ++
        dropRegions ++
        [ div
            [ class "view"
            , onClick (Activate tree.id)
            , onDoubleClick (OpenCard tree.id tree.content)
            ]
            [( lazy viewContent tree.content )]
        , span [ class "collaborators" ] [text collabsString]
        ]
      )


viewContent : String -> Html Msg
viewContent content =
  let
    options =
      { githubFlavored = Just { tables = True, breaks = True }
      , defaultHighlighting = Nothing
      , sanitize = False
      , smartypants = False
      }

    processedContent =
      content
        |> Regex.replace Regex.All (Regex.regex "{\\+\\+") (\_ -> "<ins class='diff'>")
        |> Regex.replace Regex.All (Regex.regex "\\+\\+}") (\_ -> "</ins>")
        |> Regex.replace Regex.All (Regex.regex "{--") (\_ -> "<del class='diff'>")
        |> Regex.replace Regex.All (Regex.regex "--}") (\_ -> "</del>")
  in
  Markdown.toHtmlWith options
    [] processedContent

        -}
