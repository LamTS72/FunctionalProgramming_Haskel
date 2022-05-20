import Data.Char
act :: IO (Char, Char)
act = do x <- getChar
         --getChar
         y <- getChar
         return (x,y)

type Board = [Int]

next :: Int -> Int
next 1 = 2
next 2 = 1

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board] 
   where update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO()
putRow row num = do putStr(show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     putChar '\n'
                     if isDigit x then 
                        return (digitToInt x)
                     else
                     	do putStrLn "ERROR: Invalid digit"
                     	   getDigit prompt

play :: Board -> Int -> IO()
play board player = 
	do putChar '\n'
	   putBoard board
	   if finished board then
	   	  do putChar '\n'
	   	     putStr "Player "
	   	     putStr (show (next player))
	   	     putStrLn " wins!!!"
	   	else
	   		do putChar '\n'
	   		   putStr "Player "
	   		   putStr (show player)
	   		   putStrLn " is playing..."
	   		   row <- getDigit "Row?"
	   		   num <- getDigit "How many stars?"
	   		   if valid board row num then
	   		   	  play(move board row num) (next player)
	   		   else
	   		   	   do putChar '\n'
	   		   	      putStrLn "ERROR: Invalid move"
	   		   	      play board player


nim :: IO()
nim = do putStr "Initializing... \n"
         play initial 1 

