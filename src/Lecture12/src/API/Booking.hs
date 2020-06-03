module API.Booking where

import Servant.API
import DB.Booking



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