{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lecture09 where
import Data.Hashable
import Data.Aeson
import GHC.Generics
import Data.Functor
import System.FilePath.Posix
import Control.Exception
import System.Directory
import System.Random
import Data.List
import Data.Ord
import Data.Foldable

{-
  09: Монады IO и Random

  - overview and motivation
  - Pure computations a -> b
    - Lazy: very hard to do i/o => pure
    - effects and side-effects
      - java checked exceptions
        - throwable Exception
  - not a toy => I/O (before monads, haskell 1.0)
    - streams
    - continuations
  - Effects
    - a -> IO b -- i/o
    - a -> (b -> r) -> r -- continuations
    - a -> b + Ex -- exceptions
    - a -> [b]
    - a -> Maybe b
    - T = PR _, IO _, (_ -> r) -> r, _ + Ex, [_], Maybe _
      - T-effectfull computation a -> b is a pure computation a -> T b
      - why we want (show examples, continuations figure 4, arguments passing)
        - 1. want to embed pure data into effectful world
          a -> T a
        - 2. composition of effectful computation
          a -> T b
          b -> T c
          ---------
          a -> T c

          bind :: T b -> (b -> T c) -> T c
    - class T m where
        (>>=)  :: m a -> (  a -> m b) -> m b
        return ::   a                 -> m a
  - Monad
    - set of types: Int, Float, Maybe a, IO a, ...
    - laws
      Left identity: 	return a >>= f ≡ f a
      Right identity: m >>= return ≡ m
      Associativity: 	(m >>= f) >>= g	≡ m >>= (\x -> f x >>= g)
  - Higher-kinded polymorphism + type classes + monads
  - monadic I/O (haskell 1.3 1996)
    - standard functions
  - random
    - standard functions
  - algebraic effects
  - Functor => Applicative => Monad
    - semantics (behaviour)

  Подробнее:
  - Chapter 7
    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf
  - https://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html
-}

-- <Задачи для самостоятельного решения>

{-
  TODO list

  Напишите программу для работы со списком задач.
  Хранить задачи нужно в виде файлов в определённой папке.
  Вы можете сами выбрать формат имени и содержимого файлов.

  Вам понадобятся
  - System.Directory
    https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
  - Работа с файлами в IO
    http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html
-}

newtype TodoList = TodoList { toPath :: FilePath} deriving (Eq, Show)

newtype Id = Id {toString :: String}  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype Title = Title String deriving (Eq, Show, Hashable, Generic ,ToJSON, FromJSON)

newtype Deadline = Deadline String deriving (Eq, Show, Hashable, Generic ,ToJSON, FromJSON, Ord)

newtype Content = Content String deriving (Eq, Show, Hashable,Generic ,ToJSON, FromJSON)

newtype TodoError = TodoError { message::String } deriving (Eq, Show, Generic ,ToJSON, FromJSON)
instance Exception TodoError

-- Тип для чтения Todo
data Todo = Todo
  { todoId :: Id
  , title :: Title
  , content :: Content
  , deadline :: Deadline
  , isDone :: Bool
  } deriving (Eq, Show,Generic , ToJSON, FromJSON)

-- Тип для редактирования Todo
data TodoEdit = TodoEdit
  { title :: Title
  , content :: Content
  , deadline :: Deadline
  } deriving (Eq, Show)

updateTodo :: Todo -> TodoEdit -> Todo
updateTodo todo (TodoEdit t c d) = todo {title = t, content = c, deadline = d} 

todoDeadline :: Todo -> Deadline
todoDeadline (Todo _ _ _ d _) = d

createTodoList :: FilePath ->  IO TodoList
createTodoList rootFolder = createDirectory rootFolder $> TodoList rootFolder

mkId :: Title -> Content -> Deadline -> Id
mkId tl text dl = Id . show . hash $ (tl, text, dl)  

saveTodo :: TodoList -> Todo -> IO ()
saveTodo root todo = encodeFile (toPath root </> (toString . todoId $ todo)) todo $> ()


addTodo :: TodoList -> Title -> Content -> Deadline -> IO Id
addTodo  root  t c d = saveTodo root todo $> fileId
                            where 
                              todo = Todo { todoId = fileId, title = t, content = c, deadline = d, isDone = False }
                              fileId = mkId t c d
                                                                


readTodo :: TodoList -> Id -> IO Todo
readTodo todoList (Id id) = do
                  mtodo  <- decodeFileStrict $ (toPath todoList </> id) :: IO (Maybe Todo)
                  return $ case mtodo of
                          Just a -> a
                          _ -> throw $ TodoError "Corrupted TODO file"
                            
                              

showTodo :: TodoList -> Id -> IO ()
showTodo todoList id =  readTodo todoList id >>= putStr . show

removeTodo :: TodoList -> Id -> IO ()
removeTodo todoList (Id filename) = removeFile $ toPath todoList </> filename

editTodo :: TodoList -> Id -> TodoEdit -> IO ()
editTodo todoList id update = do
                          todo <- readTodo todoList id
                          let updated = updateTodo todo update
                          saveTodo todoList updated

setTodoAsDone :: TodoList -> Id -> IO ()
setTodoAsDone todoList id = do
                            oldTodo <- readTodo todoList id
                            let doneTodo = oldTodo { isDone = True}
                            saveTodo todoList doneTodo
                              

-- Todo должны быть упорядочены по возрастанию deadline'а
readAllTodo :: TodoList -> IO [Todo]
readAllTodo todoList = do
                      files <- listDirectory (toPath todoList)
                      todos <- traverse (readTodo todoList) . map Id $ files
                      return $ sortBy (comparing todoDeadline) todos

readUnfinishedTodo :: TodoList -> IO [Todo]
readUnfinishedTodo todoList = do
                            todos <- readAllTodo todoList
                            return . filter isUnfinished $ todos
                            where 
                              isUnfinished (Todo _ _ _ _ False) = True
                              isUnfinished _ = False

showTodos :: [Todo] -> IO ()
showTodos = traverse_ (putStrLn . show)

showAllTodo :: TodoList -> IO ()
showAllTodo todoList = readAllTodo todoList >>= showTodos

showUnfinishedTodo :: TodoList -> IO ()
showUnfinishedTodo todoList = readUnfinishedTodo todoList >>= showTodos

{-
  Напишите игру для угадывания случайного числа.

  При старте, необходимо случайным образом выбрать число в
  отрезке [0..100] и предоставить пользователю возможность его угадать.

  Пример игровой сессии:

  > playGuessGame
  Your number: 50
  > Too big
  Your number: 25
  > Too small
  Your number: 37  
  > Yep, that's the number!
-}
newtype Guess = Guess {toInt :: Integer} deriving (Eq, Show)
data GuessTry = TooBig | TooSmall | Equal deriving Eq
instance Show GuessTry where
  show TooBig = "Too big"
  show TooSmall = "Too small"
  show Equal = "Yep, that's the number!"

mkGuess :: Integer -> Guess -> GuessTry
mkGuess number (Guess guess) 
      | number == guess = Equal
      | number < guess = TooBig
      | otherwise = TooSmall

gameLoop :: Integer -> IO ()
gameLoop number = do
              _ <- putStr "Your number:"
              guess <-  Guess `fmap` (readLn :: IO Integer)
              let guessTry = mkGuess number guess
              _ <- putStrLn . show $ guessTry
              if guessTry == Equal then pure () else gameLoop number

reasonableRandom :: IO Integer
reasonableRandom = randomRIO (-1000, 1000) :: IO Integer
playGuessGame :: IO ()
playGuessGame = do
              number <- reasonableRandom
              _ <- putStrLn $ "Shhhh number is " ++ show number
              gameLoop number


-- </Задачи для самостоятельного решения>