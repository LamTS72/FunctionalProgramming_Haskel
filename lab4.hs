import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fol
import qualified Data.List as List
import Data.Maybe

data Player = One | Two deriving (Show, Eq)

change :: Player -> Player
change One = Two
change Two = One

type Board = Seq.Seq Int

initialBoard :: Board
initialBoard = Seq.fromList [5, 4, 3, 2, 1]

move :: Board -> (Int, Int) -> Maybe Board
move board (row, stars)
  | and [(Seq.index board row) >= stars,
          row < 5] = Just (Seq.adjust (\x -> x - stars) row board)
  | otherwise = Nothing

display :: Board -> String
display board = List.intercalate "\n" (zipWith (++) numbers (stars board))
                where numbers = ["1. ", "2. ", "3. ", "4. ", "5. "]
                      stars board = [(concat . take n) (repeat "* ")
                                    | n <- Fol.toList board]
main :: IO ()
main = nim

nim :: IO ()
nim = do putStrLn "Welcome to nim!"
         putStrLn (display initialBoard)
         turn initialBoard One

-- the main game loop
turn :: Board -> Player -> IO ()
turn board player = do putStrLn ("\nIt's your turn, Player " ++ (show player) ++ "!")
                       putStrLn "Choose a row to remove stars!"
                       row <- getLine
                       putStrLn "How many stars do you want to remove?"
                       stars <- getLine
                       let newBoard = move board ((read row) - 1, read stars)
                       if newBoard == Nothing
                         then do putStrLn "Not valid movement"
                                 turn board player
                         else isOver (fromJust newBoard) (change player)

isOver :: Board -> Player -> IO()
isOver board player = do if board == Seq.fromList [0, 0, 0, 0, 0]
                           then putStrLn ("Congratulations, Player " ++ (show player)
                                         ++ ", you win!")
                           else do putStrLn ""
                                   putStrLn (display board)
                                   turn board player

