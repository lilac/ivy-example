{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}
module Forms.Login where

import Text.Digestive
import Text.Digestive.Blaze.Html5
import Text.Blaze (Html)
import Control.Applicative
import qualified Data.ByteString.Lazy as L
import Network.Wai.Parse (FileInfo)
import Commons

data Login = Login { email :: String, passwd :: String, img :: Maybe (FileInfo L.ByteString) }

accountVal :: Monad m => Validator m Html Login
accountVal = check "User email or passwd error." $ \(Login eml pw img) ->
    eml == "ivy" && pw == "ivy"

loginForm :: (Monad m, Functor m) => Form m (RequestData L.ByteString) Html BlazeFormHtml Login
loginForm = validate (Login <$> label "Email: " ++> inputText Nothing <++ errors
    <*> label "Password: " ++> inputText Nothing <++ errors
    <*> label "img: " ++> inputFile <++ errors <++ submit "submit")
    accountVal <++ errors

