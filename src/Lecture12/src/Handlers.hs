{-
  Этот модуль содержит функции для обработки API запросов.
  В MVC паттерне их можно рассматривать как контроллеры.
-}
module Handlers where

import Control.Monad.IO.Class (MonadIO)
import Servant.Server

import App
import DB.MovieSession
import DB.Seat
import DB.Preliminary
import DB.Booking
import Utils

getSessions :: MonadIO m => AppT m [MovieSession]
getSessions = getMovieSessions

getSeats :: MonadIO m => MovieSessionId -> AppT m [Seat]
getSeats = getSeatsBySessionId

postPreliminary :: MonadIO m => MovieSessionId -> SeatId -> AppT m BookingId
postPreliminary msId seatId = do
  bookings <- createPreliminary msId seatId
  case bookings of
    (b:_) -> pure $ bookingId b
    _ -> throwJSONError err404 $ JSONError "booking is not found"


checkout :: MonadIO m => BookingId -> AppT (Maybe String)
checkout bId = do
    booking <- getBooking bId
    let isLate = isBookingLate booking
    return $ if isLate then Nothing else Just "ok"

refund :: MonadIO m => BookingId -> AppT (Maybe String)
refund bId = do
  booking <- getBooking bId
  dropBooking bId
  return $ Just "ok"
        


