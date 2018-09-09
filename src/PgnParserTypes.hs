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

fileFromIdx :: Int -> File
fileFromIdx file = chr $ file - 1 + ord 'a'

emptyCell :: Int -> Int -> Cell
emptyCell fileIdx rank =
  let file = fileFromIdx fileIdx
   in Cell file rank Nothing

emptyBoard :: Board
emptyBoard = Board $ matrix 8 8 (uncurry emptyCell)

startBoard :: Board
startBoard =
  let (Board emptyMatrix) = emptyBoard
   in Board $
      setElem (Cell 'a' 1 (Just $ Piece White Rook)) (1, 1) .
      setElem (Cell 'a' 2 (Just $ Piece White Knight)) (1, 2) .
      setElem (Cell 'a' 3 (Just $ Piece White Bishop)) (1, 3) .
      setElem (Cell 'a' 4 (Just $ Piece White Queen)) (1, 4) .
      setElem (Cell 'a' 5 (Just $ Piece White King)) (1, 5) .
      setElem (Cell 'a' 6 (Just $ Piece White Bishop)) (1, 6) .
      setElem (Cell 'a' 7 (Just $ Piece White Knight)) (1, 7) .
      setElem (Cell 'a' 8 (Just $ Piece White Rook)) (1, 8) .
      ---
      setElem (Cell 'b' 1 (Just $ Piece White Pawn)) (2, 1) .
      setElem (Cell 'b' 2 (Just $ Piece White Pawn)) (2, 2) .
      setElem (Cell 'b' 3 (Just $ Piece White Pawn)) (2, 3) .
      setElem (Cell 'b' 4 (Just $ Piece White Pawn)) (2, 4) .
      setElem (Cell 'b' 5 (Just $ Piece White Pawn)) (2, 5) .
      setElem (Cell 'b' 6 (Just $ Piece White Pawn)) (2, 6) .
      setElem (Cell 'b' 7 (Just $ Piece White Pawn)) (2, 7) .
      setElem (Cell 'b' 8 (Just $ Piece White Pawn)) (2, 8) .
      ---
      setElem (Cell 'g' 1 (Just $ Piece Black Pawn)) (7, 1) .
      setElem (Cell 'g' 2 (Just $ Piece Black Pawn)) (7, 2) .
      setElem (Cell 'g' 3 (Just $ Piece Black Pawn)) (7, 3) .
      setElem (Cell 'g' 4 (Just $ Piece Black Pawn)) (7, 4) .
      setElem (Cell 'g' 5 (Just $ Piece Black Pawn)) (7, 5) .
      setElem (Cell 'g' 6 (Just $ Piece Black Pawn)) (7, 6) .
      setElem (Cell 'g' 7 (Just $ Piece Black Pawn)) (7, 7) .
      setElem (Cell 'g' 8 (Just $ Piece Black Pawn)) (7, 8) .
      ---
      setElem (Cell 'h' 1 (Just $ Piece Black Rook)) (8, 1) .
      setElem (Cell 'h' 2 (Just $ Piece Black Knight)) (8, 2) .
      setElem (Cell 'h' 3 (Just $ Piece Black Bishop)) (8, 3) .
      setElem (Cell 'h' 4 (Just $ Piece Black Queen)) (8, 4) .
      setElem (Cell 'h' 5 (Just $ Piece Black King)) (8, 5) .
      setElem (Cell 'h' 6 (Just $ Piece Black Bishop)) (8, 6) .
      setElem (Cell 'h' 7 (Just $ Piece Black Knight)) (8, 7) .
      setElem (Cell 'h' 8 (Just $ Piece Black Rook)) (8, 8) $
      ---
      emptyMatrix
