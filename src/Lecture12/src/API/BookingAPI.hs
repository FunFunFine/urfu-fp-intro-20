module API.Preliminary where

import Servant.API
import DB.MovieSession
import DB.Seat
import DB.Booking
data CheckoutResponse = Paid | TooLate deriving (show)
data RefundResponse = Refunded | NotFound deriving (show)

type BookingAPI
  = ("api" 
    :> "checkout"
    :> Capture "booking_id" BookingId
    :> Get '[JSON] CheckoutResponse)
    :<|>
    ("api" 
    :> "refund" 
    :> Capture "booking_id" BookingId 
    :> Get '[JSON] RefundResponse )