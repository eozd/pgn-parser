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
       Color
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
      color =
        if odd (fileIdx + rank)
          then White
          else Black
   in Cell file rank color Nothing

cellWithPiece :: Int -> Int -> Piece -> Cell
cellWithPiece fileIdx rank piece =
  let (Cell cellFile cellRank cellColor _) = emptyCell fileIdx rank
   in Cell cellFile cellRank cellColor (Just piece)

emptyBoard :: Board
emptyBoard = Board $ matrix 8 8 (uncurry emptyCell)

startBoard :: Board
startBoard =
  let (Board emptyMatrix) = emptyBoard
   in Board $
      setElem (cellWithPiece 1 1 (Piece White Rook)) (1, 1) .
      setElem (cellWithPiece 1 2 (Piece White Knight)) (1, 2) .
      setElem (cellWithPiece 1 3 (Piece White Bishop)) (1, 3) .
      setElem (cellWithPiece 1 4 (Piece White Queen)) (1, 4) .
      setElem (cellWithPiece 1 5 (Piece White King)) (1, 5) .
      setElem (cellWithPiece 1 6 (Piece White Bishop)) (1, 6) .
      setElem (cellWithPiece 1 7 (Piece White Knight)) (1, 7) .
      setElem (cellWithPiece 1 8 (Piece White Rook)) (1, 8) .
      ---
      setElem (cellWithPiece 2 1 (Piece White Pawn)) (2, 1) .
      setElem (cellWithPiece 2 2 (Piece White Pawn)) (2, 2) .
      setElem (cellWithPiece 2 3 (Piece White Pawn)) (2, 3) .
      setElem (cellWithPiece 2 4 (Piece White Pawn)) (2, 4) .
      setElem (cellWithPiece 2 5 (Piece White Pawn)) (2, 5) .
      setElem (cellWithPiece 2 6 (Piece White Pawn)) (2, 6) .
      setElem (cellWithPiece 2 7 (Piece White Pawn)) (2, 7) .
      setElem (cellWithPiece 2 8 (Piece White Pawn)) (2, 8) .
      ---
      setElem (cellWithPiece 7 1 (Piece Black Pawn)) (7, 1) .
      setElem (cellWithPiece 7 2 (Piece Black Pawn)) (7, 2) .
      setElem (cellWithPiece 7 3 (Piece Black Pawn)) (7, 3) .
      setElem (cellWithPiece 7 4 (Piece Black Pawn)) (7, 4) .
      setElem (cellWithPiece 7 5 (Piece Black Pawn)) (7, 5) .
      setElem (cellWithPiece 7 6 (Piece Black Pawn)) (7, 6) .
      setElem (cellWithPiece 7 7 (Piece Black Pawn)) (7, 7) .
      setElem (cellWithPiece 7 8 (Piece Black Pawn)) (7, 8) .
      ---
      setElem (cellWithPiece 8 1 (Piece Black Rook)) (8, 1) .
      setElem (cellWithPiece 8 2 (Piece Black Knight)) (8, 2) .
      setElem (cellWithPiece 8 3 (Piece Black Bishop)) (8, 3) .
      setElem (cellWithPiece 8 4 (Piece Black Queen)) (8, 4) .
      setElem (cellWithPiece 8 5 (Piece Black King)) (8, 5) .
      setElem (cellWithPiece 8 6 (Piece Black Bishop)) (8, 6) .
      setElem (cellWithPiece 8 7 (Piece Black Knight)) (8, 7) .
      setElem (cellWithPiece 8 8 (Piece Black Rook)) (8, 8) $
      ---
      emptyMatrix
