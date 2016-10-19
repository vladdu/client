module CommitGraph exposing (..)

import List.Extra as ListExtra
import Types exposing (Commit)

-- import Html
-- import Svg exposing (..)
-- import Svg.Attributes exposing (..)

type alias CommitDot =
  { id : String
  , parents : List String
  , x : Int
  , y : Int
  , dx : Int
  }


commitToPair : Commit -> (String, List String)
commitToPair commit =
  (commit.id, commit.parents)


pairToCommitDot : (String, List String) -> CommitDot
pairToCommitDot pair =
  { id = fst pair
  , parents = snd pair
  , x = 0
  , y = 0
  , dx = 0
  }


getCoords : List (String, List String) -> List CommitDot
getCoords pairs =
  let
    all =
      pairs
        |> List.map pairToCommitDot
        |> ListExtra.groupWhile (\x y -> (List.sort x.parents) == (List.sort y.parents))
        |> List.indexedMap (\oi l -> List.indexedMap (\i c -> {c| y = oi, dx = i}) l)
        |> List.concat

    shiftCD : List CommitDot -> CommitDot -> CommitDot
    shiftCD cds cd =
      let
        p =
          cds 
            |> ListExtra.find (\c -> (List.member c.id cd.parents))
      in
      case p of
        Nothing -> 
          let
            db1 = Debug.log "cd no parent" cd
          in
          cd

        Just parent ->
          let
            db1 = Debug.log "cd" cd
            db2 = Debug.log "parent" parent
          in
          { cd | x = cd.dx + parent.x }

  in
  all
    |> List.reverse
    |> List.map (shiftCD all)





-- viewCommitDot : CommitDot -> Svg Msg
-- viewCommitDot cd =
  -- svg stuff here
