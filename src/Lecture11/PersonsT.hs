{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lecture11.PersonsT where

import Data.List
import Data.Maybe
import Data.Functor
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad()
import Prelude hiding (id)
import Lecture10.Reader (Person (..), Sex (..), PersonId, persons, processSingle, processPair)


-- <Задачи для самостоятельного решения>

{-
  В этом задании нужно адаптировать код из 10 лекции к новым требованиям
  при помощи трансформеров.

  1. Необходимо собирать статистику найденных людей:
    - количество одиноких персон
    - количество замужних персон
  2. В функцию `findById` добавить логгирование с помощью монады Write.
    Нужно сообщать была ли найдена персона по данному id или нет.

  Вы можете переиспользовать функции, которые остались без изменения из Lecture10.Reader
-}

data PersonSearchStats = PersonSearchStats
  { marriedPersonsCount :: Integer
  , singlePersonsCount :: Integer
  } deriving (Show)

emptyStats :: PersonSearchStats
emptyStats = PersonSearchStats 0 0

newtype PersonsT a = PersonsT
  { runPersonsT :: (ReaderT [Person] (StateT PersonSearchStats (Writer [String])) a)}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader [Person]
    , MonadWriter [String]
    , MonadState PersonSearchStats
    )

runPersons :: PersonsT a -> ((a, PersonSearchStats), [String])
runPersons = runWriter . flip runStateT emptyStats . flip runReaderT persons . runPersonsT 
               

findById :: PersonId -> PersonsT (Maybe Person)
findById pId = do
              ps <- ask
              let sr = find ((==) pId. id) $ ps
              case sr of
                    Just (Person i _ _ _ _ _) -> tell ["Found person " ++ show i] 
                    _ -> tell ["Person with id " ++ show pId ++ "Not found"]
              return sr
               

processPerson :: PersonId -> PersonsT (Maybe String)
processPerson pId = do
                  put emptyStats
                  first <- findById pId
                  second <- fmap join . traverse fp $ first 
                  case (first, second) of
                        (Just h@(Person _ _ _ _ Male _), Just w@(Person _ _ _ _ Female _)) ->  modify addMarried $> Just (processPair h w)
                        (Just w@(Person _ _ _ _ Female _), Just h@(Person _ _ _ _ Male _)) ->  modify addMarried $> Just (processPair h w)
                        ((Just a), Nothing) -> modify addSingle $>  Just (processSingle a)
                        (Nothing,(Just a)) -> modify addSingle  $> Just (processSingle a)
                        _ ->  return Nothing
                    where
                      fp (Person _ _ _ _ _ (Just anId)) = findById anId
                      fp _ = return Nothing
                      addSingle ps@(PersonSearchStats _ s) = ps {singlePersonsCount = s + 1}
                      addMarried ps@(PersonSearchStats m s) = ps {marriedPersonsCount = m + 1}

{-
  Функция должна выводить на экран:
  - общую поисковую статистику по всем найденым персонам.
  - результат каждого поиска

  Записывать в "persons.log" общий лог всех поисков.
-}
processPersons :: [PersonId] -> IO ()
processPersons personIds = error "not implemented"

-- </Задачи для самостоятельного решения>
