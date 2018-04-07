module Trees exposing (..)


import Types exposing (..)
import TreeUtils exposing (getTree, getColumns, getParent, getChildren)
import List.Extra as ListExtra
import Sha1 exposing (Diff, diff3Merge)
import Diff exposing (..)
import Regex



-- MODEL

type alias Model =
  { tree : Tree
  , columns : List Column
  }


defaultModel : Model
defaultModel =
  { tree = defaultTree
  , columns = [[[defaultTree]], [getChildren defaultTree] ]
  }


defaultTree : Tree
defaultTree =
  { id = "0"
  , content = ""
  , children = Children [Tree "1" "" (Children [])]
  }





-- UPDATE

type TreeMsg
  = Nope
  | Ins String String String Int
  | Upd String String
  | Mov Tree String Int
  | Rmv String



update : TreeMsg -> Model -> Model
update msg model =
  setTree (updateTree msg model.tree) model


updateTree : TreeMsg -> Tree -> Tree
updateTree msg tree =
  case msg of
    Ins newId newContent parentId idx ->
      insertSubtree (Tree newId newContent (Children [])) parentId idx tree

    Upd id str ->
      modifyTree id (\t -> { t | content = str} ) tree

    Mov newTree parentId idx ->
      tree
        |> pruneSubtree newTree.id
        |> insertSubtree newTree parentId idx

    Rmv id ->
      pruneSubtree id tree

    Nope -> tree


setTree : Tree -> Model -> Model
setTree newTree model =
  let
    newColumns =
      if newTree /= model.tree then
        getColumns [[[newTree]]]
      else
        model.columns
  in
  { model
    | tree = newTree
    , columns = newColumns
  }


setTreeWithConflicts : List Conflict -> Tree -> Model -> Model
setTreeWithConflicts conflicts originalTree model =
  let
    newTree =
      originalTree
        |> apply (List.map (conflictToTreeMsg originalTree) conflicts)

    newColumns =
      if newTree /= model.tree then
        getColumns [[[newTree]]]
      else
        model.columns
  in
  { model
    | tree = newTree
    , columns = newColumns
  }


conflictToTreeMsg : Tree -> Conflict -> TreeMsg
conflictToTreeMsg tree {id, opA, opB, selection, resolved} =
  case (id, opA, opB, selection, resolved) of
    (_, opA, _, Ours, False) ->
      opToTreeMsg tree opA

    (_, _, opB, Theirs, False) ->
      opToTreeMsg tree opB

    (_, Mod tid _ strA orig, Mod _ _ strB _, Manual, False) ->
      let
        tokenize s =
          Regex.split Regex.All (Regex.regex "(\\s+|\\b)") s -- List String

        changeMerge d ds =
          case (d, ds) of
            (NoChange a, (NoChange b) :: tail) ->
              NoChange (a ++ b) :: tail

            (Added a, (Added b) :: tail) ->
              Added (a ++ b) :: tail

            (Removed a, (Removed b) :: tail) ->
              Removed (a ++ b) :: tail

            (ch, list) ->
              ch :: list

        diffWords l r =
          diff (tokenize l) (tokenize r)
            |> List.foldr changeMerge []
            |> List.map
              (\c ->
                case c of
                  NoChange s -> s
                  Added s -> "{++" ++ s ++ "++}"
                  Removed s -> "{--" ++ s ++ "--}"
              )
            |> String.join ""

        diffLinesString l r =
          diffLines l r
            |> List.map
              (\c ->
                case c of
                  NoChange s -> s
                  Added s -> "{++" ++ s ++ "++}"
                  Removed s -> "{--" ++ s ++ "--}"
              )
            |> String.join "\n"

        mergedString : String
        mergedString =
          diff3Merge (String.lines strA) (String.lines orig) (String.lines strB)
            |> List.map
              (\c ->
                case c of
                  Sha1.DiffOk strings ->
                    String.join "\n" strings

                  Sha1.DiffConflict (strAs, strOs, strBs) ->
                    "\n`>>>>>>>`\n" ++
                    (String.join "\n" strAs) ++
                    "\n`=======`\n" ++
                    (String.join "\n" strBs) ++
                    "\n`<<<<<<<`\n"
              )
            |> String.join "\n"

        manualString =
          "`Your version:`\n" ++
          (diffLinesString orig strA) ++
          "\n\n--------\n`Their version:`\n" ++
          (diffLinesString orig strB)
      in
      Upd tid mergedString

    _ ->
      Nope


opToTreeMsg : Tree -> Op -> TreeMsg
opToTreeMsg origTree op =
  case op of
    Mod tid _ str _ ->
      Upd tid str

    Del tid _ ->
      Rmv tid

    Types.Ins id str pids idx ->
      case ListExtra.last pids of
        Just pid ->
          Ins id str pid idx

        Nothing ->
          Nope

    Types.Mov tid opids oidx npids nidx ->
      case (getTree tid origTree, ListExtra.last npids) of
        (Just tree, Just pid) ->
          Mov tree pid nidx

        _ -> Nope





-- TREE TRANSFORMATIONS

apply : List TreeMsg -> Tree -> Tree
apply msgs tree =
  List.foldl (\m t -> updateTree m t) tree msgs


insertSubtree : Tree -> String -> Int -> Tree -> Tree
insertSubtree subtree parentId idx tree =
  let
    fn = (\c -> (List.take idx c) ++ [subtree] ++ (List.drop idx c))
  in
  modifyChildren parentId fn tree


pruneSubtree : String -> Tree -> Tree
pruneSubtree id tree =
  modifySiblings id (\c -> List.filter (\x -> x.id /= id) c) tree


modifyTree : String -> (Tree -> Tree) -> Tree -> Tree
modifyTree id upd tree =
  if tree.id == id then
    upd tree
  else
    { tree
      | children =
          getChildren tree
            |> List.map (modifyTree id upd)
            |> Children
    }


modifyChildren : String -> (List Tree -> List Tree) -> Tree -> Tree
modifyChildren pid upd tree =
  if tree.id == pid then
    { tree
      | children =
          getChildren tree
            |> upd
            |> Children
    }
  else
    { tree
      | children =
          getChildren tree
            |> List.map (modifyChildren pid upd)
            |> Children
    }


modifySiblings : String -> (List Tree -> List Tree) -> Tree -> Tree
modifySiblings id upd tree =
  case getParent id tree of
    Nothing ->
      tree
    Just parentTree ->
      modifyChildren parentTree.id upd tree
