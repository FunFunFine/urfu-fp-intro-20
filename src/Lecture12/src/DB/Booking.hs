{-# LANGUAGE DeriveAnyClass #-}

module DB.Booking where

import Data.Aeson
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Servant.API
import GHC.Generics

import DB.MovieSession
import DB.Seat
import DB.Internal

{-
  Тип для идентификатора бронирования
-}
newtype BookingId = BookingId
  { unBookingId :: Integer }
  deriving (Eq, Show)
  deriving ToRow via (Only Integer)
  -- ^ этот инстанс позволяет использовать `BookingId` с функциями для базы данных
  -- `via` говорит о том, что `BookingId` нужно использовать как `Integer`.
  -- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html
  deriving (FromHttpApiData, FromField, ToField, FromJSON, ToJSON)
    via Integer
  -- ^ тоже самое для других классов

{-
  Record, описывающий таблицу `bookings`
-}
data Booking = Booking
  { bookingId :: BookingId
  , seatId :: SeatId
  , isPreliminary :: Bool
  , movieSessionId :: MovieSessionId
  , createdAt :: UTCTime
  } deriving (Eq, Show, Generic)
-- Класс Generic отвечает за универсальное кодирование типа, т.е. за  такое представление,
-- в котором используются конструкторы типов из ограниченного набора
-- https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Generics.html
-- Это представление используется при выводе instance'ов других классов

deriving instance FromRow Booking
deriving instance ToRow Booking
-- ^ получаем возможность записывать и читать данные из базы данных с помощью `Booking`

instance ToJSON Booking
instance FromJSON Booking
-- ^ возможность для работы с JSON

{-
  Booking запрос должен проверить наличие предварительного бронирования.
  Если оно существует и прошло меньше 10 минут от создания, то бронирование
  проходит успешно, иначе необходимо вернуть сообщение об ошибке в JSON формате.
-}
isExpiredBooking :: MonadIO m => Booking -> m Bool
isExpiredBooking (Booking _ _ _ _ created) = do
                                current <- liftIO getCurrentTime
                                undefined


data BookingAttempt = Done | Old | AlreadyDone  deriving (Eq, Show)

tryBook
  :: DBMonad m
  => BookingId
  -> m BookingAttempt
tryBook bId = runSQL $ \conn -> do
    booking <- query conn ("SELECT id, seat_id,  movie_session_id, is_preliminary, created_at " <>
    "from bookings where booking_id = ? limit 1") bId
    isExpired <-  isExpired booking
    if isExpired then return Old True else if ! (isPreliminary booking) then return AlreadyDone else  do 
                                  query conn ("UPDATE bookings " <>
                                              " SET " <>
                                              "is_preliminary = ? " <>
                                              "WHERE " <>
                                                "id = ?") (False, bId)
                                  return Done
