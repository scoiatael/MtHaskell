module Core where

import Data.List (insert, delete, elem)
import qualified Data.Map as DMap

data Game = Game_state {player::Player, adversary::HidPlayer}

data Player = Player_state {lib::[[Card]], hand::[[Card]], visible::HidPlayer} deriving Show
data HidPlayer = HidPlayer_state {vplayed::[DMap.Map Int Played], vcards::[[Card]], hp::Int} deriving Show
type Card = String
type Token = String
type Counter = String
type Played = (Bool, [[Counter]], Playedaux)

data Playedaux = Token Token | CardT Card deriving (Show)

data CardId = Card Card | Id Int deriving (Show, Eq)
data StackT = Lib | Hand | VPlayed | VCards | NullS deriving (Show, Eq, Ord)
type Stack = (StackT, Int)
data CoreCommand = Move_card {whichc::CardId, from_where::Stack, where_to::Stack} 
        | Receive_dmg {how_much::Int} 
        | From_lib {whichs::Int, how_many::Int, where_to::Stack} 
        | Mod_state {whichs::Int, whichc::CardId, new_state::(Bool, [[Counter]])}
        | Add_token {whichs::Int, name::Token}
        | NullC deriving (Show, Eq)

type Player_move = Player -> CoreCommand -> Player
make_move :: Player_move
make_move pl cmd = 
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
          foldr (\a b -> make_move b (Move_card (Card a) (NullS,0) wheret)) (Player_state (flibs ++ [left] ++ rest) (hand pl) (visible pl)) draw
    Mod_state ws wc (nt,nc)
      -> case wc of 
          Id i -> let (_,_,cardname) = ((vplayed $ visible pl) !! ws) DMap.! i in 
            aux_modvplayed (aux_modlist (DMap.insert i (nt,nc,cardname)) (vplayed $ visible pl) ws) pl
          Card _ -> error "Card name instead of id given.\n"
    Add_token ws t  
      -> aux_modvplayed (aux_insertp (False, [], Token t) (vplayed $ visible pl) ws) pl             
    otherwise
      -> pl

aux_modvplayed :: [DMap.Map Int Played] -> Player -> Player
aux_modvplayed nvp pl = 
  Player_state
    (lib pl)
    (hand pl)
    (HidPlayer_state
      (nvp)
      (vcards $ visible pl)
      (hp $ visible pl))

move_card :: CardId -> Player -> StackT -> Stack -> Stack -> [[Card]]
move_card cid p w f t = let cstack = get_cstack p w 
  in if (w/=(fst t)) && (w/=(fst f)) 
      then cstack 
      else 
        if (fst f) == VPlayed 
          then 
            case cid of 
              Card _ -> error "Card name instead of id given.\n"
              Id id -> let (_,_,n) = ((vplayed $ visible p) !! (snd f)) DMap.! id 
                in 
                  case n of 
                    CardT name -> aux_insertcard name cstack (snd t)
                    Token t   -> cstack
                      
          else let c = 
                    case cid of 
                      Card s -> s 
                      _      ->  error "ID instead of card name given.\n"
                  in let cinstack = 
                          if (fst t) == w 
                            then aux_insertcard c cstack (snd t) 
                            else cstack
                        in if (fst f) == w then aux_deletecard c cinstack (snd f) else cinstack
          
        
    
  

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
  else if (fst f) == w then case cid of
    Card _ -> error "Card name instead of id given.\n"
    Id id -> let pl = (pstack !! (snd f)) DMap.! id in aux_deletep id (if (fst t) == w then aux_insertp pl pstack (snd t) else pstack) (snd f)
  else case cid of
    Id _  -> error "Id instead of card name given.\n"
    Card c -> aux_insertp (False, [], CardT c) pstack (snd t)
  
aux_deletep :: Int -> [DMap.Map Int Played] -> Int -> [DMap.Map Int Played]
aux_deletep id = aux_modlist (DMap.delete id)

aux_insertp :: Played -> [DMap.Map Int Played] -> Int -> [DMap.Map Int Played]
aux_insertp c = aux_modlist (\map -> DMap.insert ((+) 1 $ aux_genkey map) c map)

aux_genkey :: DMap.Map Int a -> Int
aux_genkey m = (+) 1 $ if DMap.null m then 0 else fst $ head $ DMap.toDescList m
get_pstack :: Player -> StackT -> [DMap.Map Int Played]
get_pstack pl st = case st of 
  VPlayed -> vplayed $ visible pl
  _       -> error "Wrong StackT at get_pstack!\n"

