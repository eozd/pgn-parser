module PgnParserTypes where

import           Data.Char
import           Data.Matrix

data PieceType
  = King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn
  deriving (Eq, Show)

data Color
  = White
  | Black
  deriving (Eq, Show)

data Piece =
  Piece Color
        PieceType
  deriving (Eq, Show)

type Rank = Int

type File = Char

data Cell =
  Cell File
       Rank
       (Maybe Piece)
  deriving (Eq, Show)

newtype Board =
  Board (Matrix Cell)
  deriving (Eq, Show)
