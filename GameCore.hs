module GameCore where

import Data.List (insert, delete, elem, unfoldr)
import qualified Data.Map as DMap

data Game = Game_state {player::Player, adversary::HidPlayer}

data Player = Player_state {lib::[[Card]], hand::[[Card]], visible::HidPlayer}
empty = DMap.empty

member :: Ord k => k -> DMap.Map k a -> Bool
member = DMap.member
data HidPlayer = HidPlayer_state {vplayed::[DMap.Map Int Played], vcards::[[Card]], hp::Int}
instance Show HidPlayer where
  show hpl = "-->\n" ++ (show $ map DMap.toDescList $ vplayed hpl) ++ "\n" ++ (show $ vcards hpl) ++ "\nHP: " ++ (show $ hp hpl) ++ "\n"
type Card = String
type Token = String
type Counter = String
type Played = ([Bool], [Counter], Playedaux)

data Playedaux = Token Token | CardT Card deriving (Show)

data CardId = Card Card | Id Int deriving (Show, Eq)
data StackT = Lib | Hand | VPlayed | VCards | NullS deriving (Show, Eq, Ord)
type Stack = (StackT, Int)

data Command a b = Cmd { doCommand :: a -> Player -> b }
  
data CoreC = Move_card {whichc::CardId, from_where::Stack, where_to::Stack} 
        | Receive_dmg {how_much::Int} 
        | From_lib {whichs::Int, how_many::Int, where_to::Stack} 
        | Add_counter {whichs::Int, whichc::CardId, counter::Counter}
        | Del_counter {whichs::Int, whichc::CardId, counter::Counter}
        | Tap {whichs::Int, whichc::CardId, whicht::Int}
        | Untap {whichs::Int, whichc::CardId, whicht::Int}
        | Add_token {whichs::Int, name::Token}
        | NullC deriving (Show, Eq)

doCoreC :: Command CoreC Player
doCoreC = Cmd (\cmd -> \pl -> 
  case cmd of
    Receive_dmg dmg 
      -> Player_state (lib pl) (hand pl) 
          (HidPlayer_state (vplayed $ visible pl) (vcards $ visible pl) ((hp $ visible pl)-dmg))
    Move_card cardid from to
      -> let 
          newlib = move_card cardid  pl Lib from to;
          newhand = move_card cardid  pl Hand from to;
          newvplayed = move_played cardid pl VPlayed from to;
          newvcards = move_card cardid pl VCards from to 
         in Player_state (newlib) (newhand) (HidPlayer_state (newvplayed) (newvcards) (hp $ visible pl))
    From_lib whichl hmany wheret
      -> let (flibs, rlibs) = splitAt (whichl) (lib pl); clib = head rlibs; rest = tail rlibs; (draw, left) = splitAt hmany clib in 
          foldr (\a b -> (doCommand doCoreC) (Move_card (Card a) (NullS,0) wheret) b) (Player_state (flibs ++ [left] ++ rest) (hand pl) (visible pl)) draw
    Tap ws wc wt
      -> aux_settap True ws wc wt pl
    Untap ws wc wt
      -> aux_settap False ws wc wt pl
    Add_counter ws wc ct
      -> aux_chcts ws wc (insert ct) pl
    Del_counter ws wc ct 
      -> aux_chcts ws wc (delete ct) pl
    Add_token ws t  
      -> aux_modvplayed (aux_insertp ([False], [], Token t) (vplayed $ visible pl) ws) pl             
    otherwise
      -> pl
  )

aux_chcts :: Int -> CardId -> ([Counter] -> [Counter]) -> Player -> Player
aux_chcts ws wc f pl = aux_modvplayed2 (\vpl -> let (k, (t, cs, name)) = findplayed wc vpl in DMap.insert k (t, f cs,name) vpl) pl ws 

aux_settap :: Bool -> Int -> CardId -> Int -> Player -> Player
aux_settap st ws wc wt pl = 
  aux_modvplayed2 (\vpl -> let  (k, (l, cs, name)) = findplayed wc vpl in 
    DMap.insert k (aux_modlist (\_ -> st) l wt, cs, name) vpl) pl ws


getplayedid :: CardId -> Int
getplayedid c = case c of 
  Id i -> i
  Card _ -> error "Card name instead of id given.\n"

findplayed :: CardId -> DMap.Map Int Played -> (Int, Played)
findplayed c cs = let id = getplayedid c in (id, cs DMap.! id)

getcardname :: CardId -> Card
getcardname cid = case cid of
  Id _ -> error "ID instead of card name given.\n"
  Card c -> c

aux_modvplayed2 :: (DMap.Map Int Played -> DMap.Map Int Played) -> Player -> Int -> Player
aux_modvplayed2 f pl i =
  let v = visible pl in
    pl {visible = v { vplayed = aux_modlist f (vplayed v) i } }

aux_modvplayed :: [DMap.Map Int Played] -> Player -> Player
aux_modvplayed nvp pl = 
  let v = visible pl in
    pl { visible = v { vplayed = nvp } }

move_card :: CardId -> Player -> StackT -> Stack -> Stack -> [[Card]]
move_card cid p w f t = let cstack = get_cstack p w 
  in if (w/=(fst t)) && (w/=(fst f)) 
    then cstack 
    else 
      case fst f of
       VPlayed
        -> let (_,(_,_,n)) = findplayed cid ((vplayed $ visible p) !! (snd f))
            in 
              case n of 
                CardT name -> aux_insertcard name cstack (snd t)
                Token t   -> cstack      
       otherwise
        -> let c = getcardname cid 
            in 
              let cinstack = if (fst t) == w then aux_insertcard c cstack (snd t) else cstack
                in 
                  if (fst f) == w then aux_deletecard c cinstack (snd f) else cinstack
          
aux_modlist:: (a -> a) -> [a] -> Int -> [a]
aux_modlist f cstack which = let (beg, r) = splitAt (which) cstack in beg ++ (f (head r)): (tail r)

aux_insertcard :: Card -> [[Card]] -> Int -> [[Card]]
aux_insertcard c = aux_modlist (insert c)

aux_deletecard :: Card -> [[Card]] -> Int -> [[Card]]
aux_deletecard c = aux_modlist (delete c)

get_cstack :: Player -> StackT -> [[Card]]
get_cstack pl st = case st of
  Hand    -> (hand pl)
  Lib     -> (lib pl)
  VCards  -> (vcards $ visible pl)
  _       -> error "Wrong StackT at get_cstack!\n"

move_played :: CardId -> Player -> StackT -> Stack -> Stack -> [DMap.Map Int Played]
move_played cid p w f t = let pstack = get_pstack p w 
  in if (fst f)/=w && (fst t)/=w then pstack 
  else if (fst f) == w 
    then 
      let (id,pl) = findplayed cid (pstack !! (snd f)) in aux_deletep id (if (fst t) == w then aux_insertp pl pstack (snd t) else pstack) (snd f)
    else let c = getcardname cid in aux_insertp ([False], [], CardT c) pstack (snd t)
  
aux_deletep :: Int -> [DMap.Map Int Played] -> Int -> [DMap.Map Int Played]
aux_deletep id = aux_modlist (DMap.delete id)

aux_insertp :: Played -> [DMap.Map Int Played] -> Int -> [DMap.Map Int Played]
aux_insertp c = aux_modlist (\map -> DMap.insert ((+) 1 $ aux_getkey map) c map)

aux_getkey :: DMap.Map Int a -> Int
aux_getkey m = if DMap.null m then 0 else fst $ head $ DMap.toDescList m
get_pstack :: Player -> StackT -> [DMap.Map Int Played]
get_pstack pl st = case st of 
  VPlayed -> vplayed $ visible pl
  _       -> error "Wrong StackT at get_pstack!\n"

shuffle :: Player -> Int -> Player
shuffle pl hash = pl { lib = map (randomShuffleList hash) (lib pl) }

randomShuffleList :: Int -> [a] -> [a]
randomShuffleList h ls = let lls = length ls in foldl (flip auxswap) ls (genRandomNumberList h lls)

genRandomNumberList :: Int -> Int -> [Int]
genRandomNumberList h lls =  map (`mod` (lls-1)) $ take lls $ unfoldr (\x -> Just (x, (1664525*x + 1013904223) `mod `(2^32))) (h`mod`(2^32))

auxswap :: Int -> [a] -> [a]
auxswap n [] = []
auxswap n (x:xs) = let (l,(r:rs)) = splitAt n xs in r:(l ++ (x:rs))
