-- Valarie Sheffey
-- CS 456
-- Hangman


module Hangman where
import System.Random
import Data.List
import Data.List.Split

main = do
  guessWord <- getWord
  putStrLn $ "guessWord = " ++ guessWord
  showLogo
  let currentWord = ['.' |a <- guessWord]
  showIntro currentWord
  turn 0 currentWord guessWord


-- Represents a round of play
turn :: Int -> [Char] -> [Char] -> IO ()
turn error currentWord guessWord = do
  putStrLn ""
  putStr "Enter the letter you want to guess: "
  c <- getChar
  getChar
  putChar c
  checkGuess c error currentWord guessWord


showLogo :: IO ()
showLogo = do
  putStrLn "--------------------------------------------"
  putStrLn "| #  #   #   #   #  #### #   #   #   #   # |"
  putStrLn "| #  #  # #  ##  # #     ## ##  # #  ##  # |"
  putStrLn "| #### ##### # # # #  ## # # # ##### # # # |"
  putStrLn "| #  # #   # #  ## #   # #   # #   # #  ## |"
  putStrLn "| #  # #   # #   #  ###  #   # #   # #   # |"
  putStrLn "--------------------------------------------"


prn_galg :: Int -> IO ()
prn_galg 0 = do
  putStrLn "Amount of wrong letters: 0"
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn ""  
  putStrLn "____________"
prn_galg 1 = do
  putStrLn "Amount of wrong letters: 1"
  putStrLn ""
  putStrLn "  |"
  putStrLn "  |"
  putStrLn "  |"
  putStrLn "  |"
  putStrLn "  |"  
  putStrLn "__|___________"
prn_galg 2 = do
  putStrLn "Amount of wrong letters: 2"
  putStrLn "  _______"
  putStrLn "  |"
  putStrLn "  |"
  putStrLn "  |"
  putStrLn "  |"
  putStrLn "  |"  
  putStrLn "__|___________"
prn_galg 3 = do
  putStrLn "Amount of wrong letters: 3"
  putStrLn "  ________"
  putStrLn "  |/     "
  putStrLn "  |      "
  putStrLn "  |"
  putStrLn "  |"
  putStrLn "  |"  
  putStrLn "__|___________"
prn_galg 4 = do
  putStrLn "Amount of wrong letters: 4"
  putStrLn "  ________"
  putStrLn "  |/   |  "
  putStrLn "  |    O  "
  putStrLn "  |"
  putStrLn "  |"
  putStrLn "  |"  
  putStrLn "__|___________"
prn_galg 5 = do
  putStrLn "Amount of wrong letters: 5"
  putStrLn "  ________"
  putStrLn "  |/   |  "
  putStrLn "  |    O  "
  putStrLn "  |    |"
  putStrLn "  |    |"
  putStrLn "  |"  
  putStrLn "__|___________"
prn_galg 6 = do
  putStrLn "Amount of wrong letters: 6"
  putStrLn "  ________"
  putStrLn "  |/   |  "
  putStrLn "  |    O  "
  putStrLn "  |   \\|"
  putStrLn "  |    |"
  putStrLn "  |"  
  putStrLn "__|___________"
prn_galg 7 = do
  putStrLn "Amount of wrong letters: 7"
  putStrLn "  ________"
  putStrLn "  |/   |  "
  putStrLn "  |    O  "
  putStrLn "  |   \\|/"
  putStrLn "  |    |"
  putStrLn "  |"  
  putStrLn "__|___________"
prn_galg 8 = do
  putStrLn "Amount of wrong letters: 8"
  putStrLn "  ________"
  putStrLn "  |/   |  "
  putStrLn "  |    O  "
  putStrLn "  |   \\|/"
  putStrLn "  |    |"
  putStrLn "  |   / "  
  putStrLn "__|___________"
prn_galg 9 = do
  putStrLn "Amount of wrong letters: 9"
  putStrLn "  ________"
  putStrLn "  |/   |  "
  putStrLn "  |    O  "
  putStrLn "  |   \\|/"
  putStrLn "  |    |"
  putStrLn "  |   / \\"  
  putStrLn "__|___________"
prn_galg 10 = do
  putStrLn "\nAmount of wrong letters: 10"
  putStrLn "  ________"
  putStrLn "  |/   |  "
  putStrLn "  |    X  "
  putStrLn "  |   \\|/"
  putStrLn "  |    |"
  putStrLn "  |   / \\"  
  putStrLn "__|___________"


-- Gets a random number from 0 to i
randomNumber :: Int -> IO Int
randomNumber i = randomRIO (0, i)


-- Selects a random word from the dictionary
getWord :: IO String
getWord = do
  rawDictionary <- readFile "words.txt"
  let dictionary = splitOn "|" (init rawDictionary)
  index <- randomNumber $ (length dictionary - 1)
  let word = dictionary !! index
  return (word)
 

-- Displays game intro
showIntro :: String -> IO ()
showIntro word = do
  putStrLn ""
  putStrLn "Welcome to the game Hangman!"
  putStrLn ""
  putStrLn "The objective in this game is to guess the word."
  putStrLn "You can enter both uppercase and lowercase letters."
  putStrLn "If you think you know the word, you can type it in."
  putStrLn "You will lose if you have guessed 10 letters wrong."
  putStrLn $ "This is the word you need to guess: " ++ word
  putStrLn ""


-- Displays whether player has won or lost
showResults :: Bool -> String -> IO ()
showResults bool str = do
  putStrLn "" 
  putStrLn "---------------\n"
  putStrLn "--- Results ---\n"
  putStrLn "---------------\n"
  putStrLn message
    where message
            | bool      = "Congratulations you guessed the right word!\n"
            | otherwise = "You guessed the wrong word. The word was "++str++". Better luck next time!\n"


-- Checks if player's guess is correct
checkGuess :: Char -> Int -> [Char] -> [Char] -> IO ()
checkGuess char error currentWord guessWord  
  |(elem char guessWord)     = guessedRight char error (updateWord char currentWord (elemIndices char guessWord)) guessWord
  |otherwise                 = guessedWrong char (error + 1) currentWord guessWord


-- Checks if player has won
guessedRight :: Char -> Int -> [Char] -> [Char] -> IO ()
guessedRight char error currentWord guessWord 
  | currentWord == guessWord = showResults True guessWord
  | otherwise                = correctResponse char error currentWord guessWord


-- Checks if player has lost
guessedWrong :: Char -> Int -> [Char] -> [Char] -> IO ()
guessedWrong char error currentWord guessWord 
  | error == 10 = do
      prn_galg 10
      showResults False guessWord
  | otherwise   = incorrectResponse char error currentWord guessWord


-- Displays repsonse when guess was correct
correctResponse :: Char -> Int -> [Char] -> [Char] -> IO ()
correctResponse char error currentWord guessWord = do
  putStrLn("")
  putStrLn("That letter was correct")
  putStrLn("The word including the letters you guessed: " ++ currentWord)
  prn_galg error
  turn error currentWord guessWord


-- Displays response when guess was incorrect
incorrectResponse :: Char -> Int -> [Char] -> [Char] -> IO ()
incorrectResponse char error currentWord guessWord = do
  putStrLn("")
  putStrLn("That letter was incorrect")
  putStrLn("The word including the letters you guessed: " ++ currentWord)
  putStrLn("")
  prn_galg error
  turn error currentWord guessWord


-- Adds discovered letters to current word
updateWord :: Char -> [Char] -> [Int] -> [Char]
updateWord char word [] = word
updateWord char word indices =
  updateWord char ((fst pair ++ char : []) ++ (drop 1 $ snd pair)) (tail indices)
  where pair = splitAt index word
        index = head indices
  
