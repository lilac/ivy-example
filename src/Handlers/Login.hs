{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveDataTypeable #-}
module Handlers.Login where

import Control.Isomorphism.Partial.TH
import Text.Digestive.Types
import Text.Digestive.Blaze.Html5
import Text.Digestive.Forms.Html (renderFormHtml)
import Commons
import Forms.Login (loginForm)
import Web.Ivy.Types
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A
import Data.String
import Handlers.Home
import Data.Typeable

data Login = Login deriving (Eq, Show, Typeable)
$(defineIsomorphisms ''Login)

instance Handler Login where
    get _ _ = do
        fHtml <- viewForm loginForm "login"
        return $ responseBlaze $ loginView fHtml

    post _ req = do
        env <- getRequestEnv' req
        efRes <- eitherForm loginForm "login" env
        case efRes of
            Left v -> return $ loginResponse v
            Right a -> get Home req

loginResponse = responseBlaze . loginView

loginView :: BlazeFormHtml -> Html
loginView f = do
    docTypeHtml $ do
        H.head $ do
            meta ! charset "UTF-8"
            H.title "Login"
        H.body $ do
            H.h1 "Login"
            H.form ! enctype t ! method "post" $ do
                fieldset form
                --input ! type_ "submit" ! value "submit"
        where (form, et) = renderFormHtml f
              t = enctype2Attr et

enctype2Attr = fromString . show
