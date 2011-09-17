{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, TemplateHaskell, DeriveDataTypeable #-}
import Network.Wai
import Network.HTTP.Types (statusOK)
import Network.Wai.Handler.Warp (run)
import Control.Isomorphism.Partial
import Control.Isomorphism.Partial.TH
import Text.Syntax
import Web.Ivy.Routes hiding (url)
import qualified Web.Ivy.Routes as R
import Web.Ivy.Types
import Data.ByteString.Lazy.Char8 (unpack, pack, append)
import qualified Data.ByteString.Char8 as BS
import Handlers.Home
import Text.Blaze (Html)
import Control.Monad.IO.Class
import System.Time (getClockTime)
import Handlers.Login
import Commons
import Data.Typeable

application req = return $
  responseLBS statusOK [("Content-Type", "text/html")] $ pack $ "default application\n" 
  ++ show req

data Blog = Blog Int Int Int deriving (Show, Eq, Typeable)
$(defineIsomorphisms ''Blog)

data Profile = Profile Int deriving (Show, Eq, Typeable)
$(defineIsomorphisms ''Profile)

data Friends = Friends Int deriving (Show, Eq, Typeable)
$(defineIsomorphisms ''Friends)


instance Handler Blog where
    get b@(Blog y m d) _ = do
        t <- liftIO getClockTime
        return $ responseHtml $ trans' "blog" ++ show t

rProfile = profile <$> text "profile/" *> int

instance Handler Profile

rFriends = friends <$> text "friends/" *> int

instance Handler Friends

userRoute = routeIso <$> rProfile
        <|> routeIso <$> rFriends

rHome = home <$> text "/"
rBlog = blog <$> text "/blog/" *> int <-> int <-> int
rLogin = login <$> text "/login"

--route :: Syntax delta => [delta a]
route = routeIso <$> rHome
      <|> routeIso <$> rLogin
      <|> routeIso <$> rBlog
      <|> text "/user/" *> userRoute

url = R.url route

ivyApp :: Application
ivyApp req = let path = rawPathInfo req in
    case (parseUrl route $ BS.unpack path) of
        [Route r] -> handle r req
        otherwise -> application req

main = run 8080 ivyApp

