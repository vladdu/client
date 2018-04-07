module Trees exposing (..)


import Tuple exposing (first, second)
import Dict exposing (Dict)
import List.Extra as ListExtra
import Sha1 exposing (Diff, diff3Merge)
import Diff exposing (..)
import Regex
import Random exposing (initialSeed, int, minInt, maxInt)



-- MODEL

type alias Model =
  { tree : Tree
  , columns : List Column
  }


type alias Tree =
  { id : String
  , content : String
  , children : Children
  }


type alias Conflict =
  { id : String
  , opA : Op
  , opB : Op
  , selection : Selection
  , resolved : Bool
  }


type Children = Children (List Tree)
type alias Group = List Tree
type alias Column = List (List Tree)


type Op = Ins String String (List String) Int | Mod String (List String) String String | Del String (List String) | Mov String (List String) Int (List String) Int
type Selection = Original | Ours | Theirs | Manual




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
  | Insert String String String Int
  | Update String String
  | Move Tree String Int
  | Rmv String



update : TreeMsg -> Model -> Model
update msg model =
  setTree (updateTree msg model.tree) model


updateTree : TreeMsg -> Tree -> Tree
updateTree msg tree =
  case msg of
    Insert newId newContent parentId idx ->
      insertSubtree (Tree newId newContent (Children [])) parentId idx tree

    Update id str ->
      modifyTree id (\t -> { t | content = str} ) tree

    Move newTree parentId idx ->
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
      Update tid mergedString

    _ ->
      Nope


opToTreeMsg : Tree -> Op -> TreeMsg
opToTreeMsg origTree op =
  case op of
    Mod tid _ str _ ->
      Update tid str

    Del tid _ ->
      Rmv tid

    Ins id str pids idx ->
      case ListExtra.last pids of
        Just pid ->
          Insert id str pid idx

        Nothing ->
          Nope

    Mov tid opids oidx npids nidx ->
      case (getTree tid origTree, ListExtra.last npids) of
        (Just tree, Just pid) ->
          Move tree pid nidx

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



getColumns : List Column -> List Column
getColumns cols =
  let
    col =
      case (ListExtra.last cols) of
        Nothing -> [[]]
        Just c -> c

    hasChildren =
      col
        |> List.concat
        |> List.any (\x -> (getChildren x) /= [])

    nextColumn col =
      List.map getChildren (List.concat col)
  in
  if hasChildren then
    getColumns(cols ++ [nextColumn(col)])
  else
    cols




-- ACCESSORS

getTree : String -> Tree -> Maybe Tree
getTree id tree =
  if tree.id == id then
    Just tree
  else
    getChildren tree
      |> List.map (getTree id)
      |> List.filter (\m -> m /= Nothing)
      |> List.head
      |> Maybe.withDefault Nothing

getParent : String -> Tree -> Maybe Tree
getParent id tree =
  case tree.children of
    Children [] ->
      Nothing
    Children children ->
      if (List.member id (List.map .id children)) then
        Just tree
      else
        children
          |> List.map (getParent id)
          |> List.filter (\m -> m /= Nothing)
          |> List.head
          |> Maybe.withDefault Nothing


getChildren : Tree -> List Tree
getChildren x =
  case x.children of
    Children c ->
      c


getSiblings : String -> Tree -> List Tree
getSiblings id tree =
  if (getChildren tree |> List.map .id |> List.member id) then
    getChildren tree
  else
    List.concatMap (getSiblings id) (getChildren tree)


getColumn : Int -> Tree -> Maybe (List (List Tree))
getColumn n tree =
  let
    cols =
      getColumns [[[tree]]]
  in
  ListExtra.getAt n cols


getTreeWithPosition : String -> Tree -> Maybe (Tree, String, Int)
getTreeWithPosition id tree =
  Maybe.map3
    (\t p i -> (t, p, i))
    ( getTree id tree )
    ( getParent id tree |> Maybe.map .id )
    ( getIndex id tree )


getPrevNext : Int -> String -> Tree -> Maybe Tree
getPrevNext shift id tree =
  let
    siblings = getSiblings id tree
    idx =
      siblings
        |> List.map .id
        |> ListExtra.elemIndex id
  in
  case idx of
    Nothing -> Nothing

    Just i ->
      siblings
        |> ListExtra.getAt (i + shift)


getPrev : String -> Tree -> Maybe Tree
getPrev id tree =
  getPrevNext (-1) id tree


getNext : String -> Tree -> Maybe Tree
getNext id tree =
  getPrevNext 1 id tree


getPrevNextInColumn : Int -> String -> Tree -> Maybe Tree
getPrevNextInColumn shift id tree =
  let
    n = getDepth 0 tree id
    column_ = getColumn n tree
  in
  case column_ of
    Nothing -> Nothing

    Just col ->
      let
        idx =
          col
            |> List.concat
            |> List.map .id
            |> ListExtra.elemIndex id
      in
      case idx of
        Nothing -> Nothing

        Just i ->
          col
            |> List.concat
            |> ListExtra.getAt (i + shift)


getPrevInColumn : String -> Tree -> Maybe Tree
getPrevInColumn id tree =
  getPrevNextInColumn (-1) id tree


getNextInColumn : String -> Tree -> Maybe Tree
getNextInColumn id tree =
  getPrevNextInColumn 1 id tree


getContent : String -> Tree -> String
getContent id tree =
  case getTree id tree of
    Nothing ->
      ""
    Just t ->
      t.content


getIndex : String -> Tree -> Maybe Int
getIndex id tree =
  getSiblings id tree
    |> List.map .id
    |> ListExtra.elemIndex id


getDescendants : Tree -> List Tree
getDescendants t =
  let
    children = getChildren t
  in
  if List.isEmpty children then
    []
  else
    children ++ (List.concatMap getDescendants children)


getAscendants : Tree -> Tree -> List Tree -> List Tree
getAscendants all target accum =
  let
    current =
      case (List.head accum) of
        Nothing -> target
        Just t -> t
  in
  case (getParent current.id all) of
    Nothing -> accum
    Just p ->
      (getAscendants all target (p :: accum))


getDepth : Int -> Tree -> String -> Int
getDepth prev tree id =
  case tree.children of
    Children children ->
      if (tree.id == id) then
        prev
      else
        children
          |> List.map ((flip (getDepth (prev+1))) id)
          |> List.maximum
          |> Maybe.withDefault 0




-- DATABASE TRANSFORMATIONS


generateId : String -> Int -> String
generateId timeString time =
  [ "node"
  , timeString
  , Random.step (int 0 maxInt) (initialSeed time)
      |> first
      |> toString
  ]
    |> String.join "-"




-- SPECIAL PROPERTIES

centerlineIds : List (List String) -> List String -> List String -> List (List String)
centerlineIds flatCols allIds activePast =
  let
    lastActiveOrAll aP ids =
      let
        lastActiveIdx_ =
          aP
            |> ListExtra.findIndex (\a -> List.member a ids)
      in
      case lastActiveIdx_ of
        Nothing -> ids
        Just idx ->
          aP
            |> ListExtra.getAt idx -- Maybe String
            |> Maybe.withDefault "1"
            |> ListExtra.singleton
  in
  flatCols
    |> List.drop 1
    |> List.map (\c -> List.filter (\id -> List.member id allIds) c)
    |> ListExtra.filterNot List.isEmpty
    |> List.map (lastActiveOrAll activePast)




-- HELPERS

(?) : Maybe a -> a -> a
(?) maybe default =
  Maybe.withDefault default maybe

infixr 9 ?

newLine : String
newLine =
  String.fromList ['\n']

withIdTree : String -> Tree
withIdTree id =
  Tree id "" (Children [])

dictUpdate : comparable -> (b -> b) -> Dict comparable b -> Dict comparable b
dictUpdate id upd dict =
  Dict.update
    id
    (\n_ ->
      case n_ of
        Just n -> Just (upd n)
        Nothing -> Nothing
    )
    dict
