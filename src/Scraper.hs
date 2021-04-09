{-# LANGUAGE OverloadedStrings #-}

module Scraper
    ( checkAllPlayers
    ) where

import Control.Monad (forM_)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit ((.|), runConduit)
import qualified Data.Text as T
import Network.HTTP.Conduit (http, newManager, parseUrl, responseBody, parseRequest, tlsManagerSettings)
import Text.Parsec (runParser)
import Text.HTML.DOM (sinkDoc)
import Text.XML (Document)
import Text.XML.Cursor (Axis, Cursor, node, fromDocument, attribute, attributeIs, content, ($//), (&//), element, descendant, child, (>=>), check, orSelf)

import Calculator
import Parser


pbe = "https://probaseballexperience.jcink.net"

makeRequest :: T.Text -> IO Document
makeRequest url = do
    request <- parseRequest $ T.unpack url
    manager <- newManager tlsManagerSettings
    runResourceT $ do
        response <- http request manager
        runConduit $ responseBody response .| sinkDoc

minorsRosterPages :: Document -> [T.Text]
minorsRosterPages d = fromDocument d $// axis
    where axis = attributeIs "id" "cat_17"
                 >=> child >=> child >=> attributeIs "class" "forum-row"
                 >=> child >=> child >=> attributeIs "class" "desc"
                 >=> child >=> attributeIs "class" "subforums"
                 >=> child >=> attributeIs "class" "tooltip"
                 >=> check bar
                 >=> attribute "href"

bar :: Cursor -> Bool
bar c = any ("Roster" `T.isSuffixOf`) (child c >>= content)

players :: Document -> [T.Text]
players d = fromDocument d $// axis
    where axis = attributeIs "class" "topic-row"
                 >=> child >=> child >=> check baz
                 >=> attribute "href"

allPages :: Document -> IO [Document]
allPages d = do
    let rest = axis $ fromDocument d
        axis = take 1 . (descendant >=> attributeIs "class" "pagination")
               >=> child >=> attributeIs "class" "pagination_page"
               >=> attribute "href"
    (d:) <$> mapM makeRequest rest

allPlayers :: Document -> IO [T.Text]
allPlayers d = do
    pages <- allPages d
    pure $ pages >>= players

baz :: Cursor -> Bool
baz c = any ("This topic was started:" `T.isPrefixOf`) (attribute "title" c)

checkTeam :: T.Text -> IO ()
checkTeam url = do
    d <- makeRequest url
    ps <- allPlayers d
    forM_ ps checkPlayer
    --forM_ (players d) checkPlayer
    putStr "Finished team "
    print url

playerName :: Document -> [T.Text]
playerName d = fromDocument d $// axis
    where axis = attributeIs "class" "topic-title"
                 >=> child >=> content

checkPlayer :: T.Text -> IO ()
checkPlayer url = do
    d <- makeRequest url
    print $ playerName d
    let post = firstPost d
    --print $ T.take 50 post
    --putStrLn . T.unpack $ T.unlines post
    let p = runParser player 0 "" (T.unpack $ T.unlines post)
    if either (const True) badAttrs p
        then print url *> print (head $ playerName d) *> print p
        --else pure ()
        else let Right p' = p in print $ tpeSpent p'
    --print p

--firstPost :: Document -> [T.Text]
firstPost d = doc $// axis
    where axis = content
          doc = head $ fromDocument d $// attributeIs "class" "postcolor"
{-
checkAllPlayers :: IO ()
checkAllPlayers = do
    d <- makeRequest pbe
    forM_ (minorsRosterPages d) checkTeam
-}

checkAllPlayers = do
    checkTeam "http://probaseballexperience.jcink.net/index.php?showforum=138"
