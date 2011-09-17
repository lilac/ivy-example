{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Handlers.Home where

import Control.Isomorphism.Partial.TH
import Views.Home
import Text.Blaze (Html)
import Commons
import Web.Ivy.Types
import Data.Typeable

data Home = Home deriving (Show, Eq, Typeable)
$(defineIsomorphisms ''Home)

instance Handler Home where
    --get _ _ = return $ responseLBS statusOK [] "This is home page."
    get _ _ = return $ responseBlaze template
