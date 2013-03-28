module Core where

import Data.List (insert, delete, elem)

data Game = Game_state {player::Player, adversary::HidPlayer}

data Player = Player_state {lib::[Card], hand::[Card], visible::HidPlayer} deriving Show
data HidPlayer = HidPlayer_state {ing::[Played], rfp::[Played], grv::[Card], hp::Int} deriving Show
data Stack = Lib | Hand | Ing | Rfp | Grv | NullSt deriving Eq
type Card = String
type Token = String
data Played = Card Card deriving (Show, Eq, Ord)

adversary_alive :: HidPlayer -> Bool
adversary_alive hpl = (hp hpl) > 0

player_alive :: Player -> Bool
player_alive pl = (adversary_alive $ visible pl) && (length $ lib pl) > 0

data Command = Move_card Card Stack Stack | Receive_dmg Int | From_lib Int | To_lib Card | Null | Help | End deriving Eq

type HidPlayer_move = HidPlayer -> Command -> HidPlayer
make_shadow_move :: HidPlayer_move
make_shadow_move arg cmd = visible $ make_move (Player_state [] [] arg) cmd

type Player_move = Player -> Command -> Player
make_move :: Player_move
make_move pl cmd = 
  case cmd of
    Receive_dmg dmg 
      -> Player_state (lib pl) (hand pl) (HidPlayer_state 
        (ing $ visible pl) (rfp $ visible pl) (grv $ visible pl)  ((hp $ visible pl)-dmg))
    Move_card card from to
      -> let 
          newlib = move_card card pl Lib from to;
          newhand = move_card card pl Hand from to;
          newig = move_played card pl Ing from to;
          newgrv = move_card card pl Grv from to;
          newrfp = move_played card pl Rfp from to in Player_state (newlib) (newhand) (HidPlayer_state (newig) (newrfp) (newgrv) (hp $ visible pl))
    From_lib am
      -> let (draw, left) = splitAt am (lib pl) in 
          Player_state (left) ((hand pl) ++ draw) (visible pl)
    To_lib c
      -> Player_state (shuffle $ insert c $ lib pl) (delete c $ hand pl) (visible pl)
    otherwise
      -> pl
     
shuffle :: [Card] -> [Card]
shuffle r = r

move_card c p w f t = if w/=t && w/=f then get_cstack p w else
  case w==t of 
    False -> if c `elem` (get_cstack p t) then delete c (get_cstack p f) else get_cstack p f
    True ->  if c `elem` (get_cstack p f) then insert c (get_cstack p t) else get_cstack p t

get_cstack :: Player -> Stack -> [Card]
get_cstack pl st = case st of
  Hand -> hand pl
  Lib -> lib pl
  Grv -> grv $ visible pl

move_played :: Card -> Player -> Stack -> Stack -> Stack -> [Played]
move_played c p w f t = if w/=f && w/=t then  get_pstack p w else
  case w==f of
    True ->  if (Card c) `elem` (get_pstack p t) then delete (Card c) (get_pstack p f) else get_pstack p f
    False -> if (Card c) `elem` (get_pstack p f) then insert (Card c) (get_pstack p t) else get_pstack p t

get_pstack :: Player -> Stack -> [Played]
get_pstack pl st = case st of
  Ing -> ing $ visible pl
  Rfp -> rfp $ visible pl

