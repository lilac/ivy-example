{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables #-}
module Commons where
import Text.Blaze (Html, toHtml)
import Network.Wai
import Network.HTTP.Types
import Text.Blaze.Renderer.Utf8
import Network.Wai.Parse
import Text.Digestive.Forms (FormInput(..))
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import Data.Maybe
{-
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.ByteString (Parser)
-}
import Text.Digestive.Result (FormId(..))
import Text.Digestive.Types
import Network.Wai.Parse
import Data.Enumerator (Iteratee, yield)
import Control.Monad
import Text.I18n
import Text.I18n.Po (getL10n)
import Data.String
import System.IO.Unsafe (unsafePerformIO)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

data RequestData f = RequestParam S.ByteString
        | RequestFile (FileInfo f)

instance FormInput (RequestData f) (FileInfo f) where
    getInputStrings i = case i of
        RequestParam p -> (: []) $ S.unpack p
        otherwise -> []
    getInputFile i = case i of
        RequestFile f -> Just f
        otherwise -> Nothing

instance FormInput String String where
    getInputStrings i = return i
    getInputFile i = Just i

--responseHtml = responseLBS statusOK [("Content-Type", "text/html")]
responseHtml :: String -> Response
responseHtml content = responseBlaze $
    docTypeHtml $ do
        H.head $ do
            meta ! charset "UTF-8"
            H.title "Login"
        H.body $ toHtml $content

responseBlaze :: Html -> Response
responseBlaze = ResponseBuilder statusOK [("Content-Type", "text/html")] . renderHtmlBuilder

getRequestEnv :: forall m x y. Monad m => Sink x y -> Request -> Iteratee S.ByteString IO (Environment m (RequestData y))
getRequestEnv sink req = do
    (params, files) <- parseRequestBody sink req
    let ef :: FormId -> m (Maybe (RequestData y))
        ef id = return $ fmap RequestParam (lookup sid params) `mplus`
               fmap RequestFile (lookup sid files)
            where sid = S.pack $ show id
    return $ Environment ef

getRequestEnv' :: forall m . Monad m => Request -> Iteratee S.ByteString IO (Environment m (RequestData L.ByteString))
getRequestEnv' = getRequestEnv lbsSink

trans :: IsString s => String -> IO s
trans msg = do
    (l10n, errors) <- getL10n "locale"
    return $ fromString $ localize l10n (Locale "zh") $ gettext msg

trans' :: IsString s => String -> s
trans' = unsafePerformIO . trans
{-
parseFormId :: Parser FormId
parseFormId = do 
    id <- many letter
    string "-fval["
    nums <- sepBy (char '.') natural
    char ']'
    return $ FormId id nums


natural :: Parser Integer
natural = fmap (post 0) $ many1 digit
  where
    post ac []     = (ac * 10) 
    post ac [x]    = (ac * 10) + digitToInt x
    post ac (x:xs) = post (ac * 10 + digitToInt x) xs

whiteSpace = many (oneOf " \t") >> return ()
-}
