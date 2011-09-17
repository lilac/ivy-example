{-# LANGUAGE OverloadedStrings #-}
module Views.Home where

import Prelude
import qualified Prelude as P
import Data.Monoid (mempty)

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

template :: Html
template = do
    docTypeHtml $ do
        H.head $ do
            meta ! charset "UTF-8"
            H.title "BlazeHtml - Home"
            link ! rel "stylesheet" ! type_ "text/css" ! href "./css/default.css"
            link ! rel "stylesheet" ! type_ "text/css" ! href "./css/syntax.css"
        body $ do
            H.div ! A.id "navigation" $ do
                a ! href "." ! A.id "logo" $ "BlazeHtml"
                a ! href "./benchmarks.html" ! class_ "menu" $ "benchmarks"
                a ! href "./docs/index.html" ! class_ "menu" $ "docs"
                a ! href "./about.html" ! class_ "menu" $ "about"
                a ! href "./tutorial.html" ! class_ "menu" $ "tutorial"
                a ! href "." ! class_ "menu" $ "home"
            H.div ! A.id "header" $ do
                H.div ! class_ "column" $ do
                    p $ do
                        "BlazeHtml is a blazingly fast HTML combinator library for the"
                        a ! href "http://haskell.org/" $ "Haskell"
                        "programming language. It embeds HTML templates in Haskell code for optimal efficiency and composability. To get started, just"
                    pre $ code "cabal install blaze-html"
                H.div ! class_ "column" $ do
                    p "The project is aimed at those who seek to write web applications in Haskell — it integrates well with all Haskell web frameworks."
                    p $ do
                        "The best way to get started with BlazeHtml is to have a look at our"
                        a ! href "./tutorial.html" $ "tutorial"
                        "."
            H.div ! A.id "content" $ do
                H.div ! A.id "features" $ do
                    h1 "Features"
                    ul $ do
                        li $ do
                            "Pretty fast — have a look at"
                            a ! href "./benchmarks.html" $ "our benchmarks"
                        li "Lightweight DSL syntax"
                        li "Embedded in Haskell"
                        li "Efficient Unicode support"
                        li "Supports HTML 4 Strict and HTML 5"
                        li "Tool to create code from an HTML file"
                H.div ! A.id "status" $ do
                    h1 "Status"
                    p $ do
                        "The BlazeHtml API is considered stable, however,\
						\ the implementation is still experimental. We like to encourage you to try it and tell us what you think, and submit possible bugs to the"
                        a ! href "http://github.com/jaspervdj/blaze-html/issues/" $ "issue tracker"
                        "(if you do not have a GitHub account,"
                        a ! href "./about.html#contact" $ "contact us by mail"
                        "."
                H.div ! A.id "code" $ do
                    h1 "Code"
                    p $ do
                        "If you are interested, all code is available on"
                        a ! href "http://github.com/jaspervdj/blaze-html/" $ "GitHub"
                        "."
            H.div ! A.id "footer" $ mempty
