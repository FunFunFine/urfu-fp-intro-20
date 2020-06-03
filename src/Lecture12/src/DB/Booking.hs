{-# LANGUAGE DeriveAnyClass #-}

module DB.Booking where
import Control.Monad.Except
import Data.Aeson
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Servant.API
import GHC.Generics
import DB.MovieSession
import DB.Seat (SeatId)
import DB.Internal
import Data.Functor
import Data.Maybe

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
  , movieSessionId :: MovieSessionId
  , isPreliminary :: Bool
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
timeToPay :: NominalDiffTime
timeToPay = secondsToNominalDiffTime $ 60 * 10

isExpired :: MonadIO m => UTCTime -> m Bool
isExpired created = do
                  current <- liftIO getCurrentTime
                  let expiration = addUTCTime timeToPay created
                  return $ current >= expiration


data CheckoutResponse = CheckedOut | BookingIsExpired deriving (Eq, Show, Generic)

instance ToJSON CheckoutResponse
instance FromJSON CheckoutResponse

data RefundResponse = Refunded | Preliminary deriving (Eq, Show, Generic)

instance ToJSON RefundResponse
instance FromJSON RefundResponse


data BookingAttempt = Booked SeatId MovieSessionId | Expired | AlreadyBooked  deriving (Eq, Show)

tryBook
  :: DBMonad m
  => BookingId
  -> m (Maybe BookingAttempt)
tryBook bId = do
      bookingM <- getBooking bId
      (flip traverse) bookingM (\(Booking _ sId msId isPrelim created) -> do
                      isExp <- isExpired created
                      case (isExp, isPrelim) of 
                        (_, False) -> return AlreadyBooked
                        (False, _) -> setBookingFinal bId $> Booked sId msId
                        _ -> return Expired
                      )




getBooking :: DBMonad m => BookingId -> m (Maybe Booking)
getBooking bId = runSQL $ \conn -> do
                  bookings <- query conn ("SELECT id, seat_id,  movie_session_id, is_preliminary, created_at " 
                                        <>"from bookings where booking_id = ? limit 1") bId
                  return $ listToMaybe bookings

setBookingFinal :: DBMonad m => BookingId -> m ()
setBookingFinal bId =  runSQL $ \conn ->
                            execute conn ("UPDATE bookings SET is_preliminary = false WHERE id = ?") bId
                        
removeBooking :: DBMonad m => BookingId -> m ()
removeBooking bId = runSQL $ \conn ->
                            execute conn ("DELETE FROM bookings WHERE id = ?") bId
