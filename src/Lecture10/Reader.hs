module Lecture10.Reader where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Prelude hiding (id)

-- <Задачи для самостоятельного решения>

{-
  Задача: по имеющейся базе данных сфомировать набор рекламных писем.
  Для неженатого(-ой) персоны письмо должно иметь вид:

  Для мужчины:

"""
  Уважаемый Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Для женщины:

"""
  Уважаемая Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Семейным парам шлется одно письмо вида

"""
  Уважаемые Имя_мужа Отчество_мужа и Имя_жены Отчество_жены!
  Разрешите предложить вам наши услуги.
"""

-}

data Sex = Male | Female deriving (Show, Eq, Ord)

type PersonId = Int

data Person = Person
  { id :: Int
  , family :: String
  , name :: String
  , surname :: String
  , sex :: Sex
  , marriedBy :: Maybe Int
  } deriving (Show, Eq, Ord)

persons :: [Person]
persons =
  [ Person 1 "Иванов" "Иван" "Иванович" Male Nothing
  , Person 2 "Петров" "Петр" "Петрович" Male (Just 7)
  , Person 3 "Соловьева" "Алия" "Фаридовна" Female Nothing
  , Person 4 "Кузнецова" "Мария" "Ивановна" Female (Just 8)
  , Person 5 "Гринько" "Юлия" "Владимировна" Female Nothing
  , Person 6 "Кабанов" "Александр" "Романович" Male Nothing
  , Person 7 "Петрова" "Екатерина" "Алексеевна" Female (Just 2)
  , Person 8 "Кузнецов" "Евгений" "Семёнович" Male (Just 4)
  , Person 9 "Антонов" "Юрий" "Васильевич" Male Nothing
  ]

-- Поиск персоны по номеру
findById :: PersonId -> Reader [Person] (Maybe Person)
findById pId = do
              ps <- ask
              return . find ((==) pId. id) $ ps
letUs :: String
letUs = "!\n"++"Разрешите предложить Вам наши услуги."
letUsNonRespect :: String
letUsNonRespect = "!\n"++"Разрешите предложить вам наши услуги."
processSingle :: Person -> String
processSingle (Person _ _ i o s Nothing) = case s of
                  Male -> "Уважаемый "++ i ++ " " ++ o ++ letUs
                  Female -> "Уважаемая "++ i ++ " " ++ o ++ letUs

processSingle _ = error "wrong"                 

processPair :: Person -> Person -> String
processPair (Person hId _ hi ho _ (Just wId')) (Person wId _ wi wo _ (Just hId')) 
                                      | wId' == wId && hId' == hId =
                                         "Уважаемые "++ hi ++ " " ++ ho ++ " и " ++ wi ++ " " ++ wo ++ letUsNonRespect
processPair _ _ = error "not married"

processPerson :: PersonId -> Reader [Person] (Maybe String)
processPerson pId = do
                  first <- findById pId
                  second <- fmap join . traverse fp $ first 
                  return $ case (first, second) of
                        (Just h@(Person _ _ _ _ Male _), Just w@(Person _ _ _ _ Female _)) -> 
                              Just (processPair h w)
                        (Just w@(Person _ _ _ _ Female _), Just h@(Person _ _ _ _ Male _)) ->  
                              Just (processPair h w)
                        ((Just a), Nothing) ->  
                              Just (processSingle a)
                        (Nothing,(Just a)) ->  
                              Just (processSingle a)
                        _ ->  Nothing
                    where
                      fp :: Person -> Reader [Person] (Maybe Person)
                      fp (Person _ _ _ _ _ (Just anId)) = findById anId
                      fp _ = return Nothing


processPersons :: [PersonId] -> [Maybe String]
processPersons personIds =  do
                      pId <- personIds
                      return . runReader (processPerson pId) $ persons
                        

-- </Задачи для самостоятельного решения>
