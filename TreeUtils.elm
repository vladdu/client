module TreeUtils exposing (..)

import String
import List.Extra as ListExtra
import Types exposing (..)
import Sha1


-- TRANSFORMATIONS

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




-- SINGLE TREE ACCESSORS

getChildren : Tree -> List Tree
getChildren x =
  case x.children of
    Children c ->
      c




-- GLOBAL ACCESSORS

getGroup : String -> List Tree -> List Tree
getGroup id trees =
  let
    isMember =
      trees
        |> List.map .id
        |> List.member id

    children =
      trees -- List Tree
        |> List.concatMap getChildren -- List Tree
  in
  if isMember then
    trees
  else
    getGroup id children



getPrevId : String -> List Tree -> Maybe String
getPrevId id trees =
  Nothing


getNextId : String -> List Tree -> Maybe String
getNextId id trees =
  Nothing




-- HELPERS

(?) : Maybe a -> a -> a
(?) maybe default =
  Maybe.withDefault default maybe

infixr 9 ?

newLine : String
newLine =
  String.fromList ['\n']

