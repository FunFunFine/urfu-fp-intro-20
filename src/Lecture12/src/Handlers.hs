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
import Data.Functor

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


checkout :: MonadIO m => BookingId -> AppT m CheckoutResponse
checkout bId = do
    attempt <- tryBook bId
    case attempt of 
      Just (Booked sId msId) -> setSeatUnavailable msId sId $> CheckedOut
      Just AlreadyBooked -> return CheckedOut
      Just Expired -> return BookingIsExpired
      _ -> throwJSONError err404 $ JSONError "booking is not found or something else happened"
    
refund :: MonadIO m => BookingId -> AppT m RefundResponse
refund bId = do
  bookingM <- getBooking bId
  case bookingM of 
    Just (Booking _ _ _ True _ ) -> return Preliminary
    Just (Booking _ sId msId _ _) -> setSeatAvailable msId sId >> removeBooking bId $> Refunded
    _ -> throwJSONError err404 $ JSONError "booking is not found or something else happened"
        


