#!/usr/bin/env stack
{- stack script
    --resolver lts-18.6
    --package tagsoup
    --package http-conduit
    --package bytestring
 -}

{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List
import GHC.Stack
import Network.HTTP.Conduit hiding (path)
import System.Environment
import Text.HTML.TagSoup
import Text.StringLike

main :: IO ()
main = do
  args <- getArgs
  if length args == 1
    then do
      let first = head args
      (cycle', path) <- findCycle first [first]
      let path_length = show $ length path
      putStrLn $
        first
          ++ " has a path of length "
          ++ path_length
          ++ ":"
      printListIndented path
      let cycle_length = show $ length cycle'
      putStrLn $ "with a cycle of length " ++ cycle_length ++ ":"
      printListIndented cycle'
    else putStrLn "Usage: ./wiki_depth <wikipedia-page>"

printListIndented :: [String] -> IO ()
printListIndented list =
  putStrLn $ "    " ++ foldl1 (\acc s -> acc ++ "\n    " ++ s) list

-- Returns the cycle and the path to the cycle
findCycle :: String -> [String] -> IO ([String], [String])
findCycle url list = do
  text <- fetchPage url
  let first = head $ drop 1 $ getLinkReferences $ getBody text
  if first `elem` list
    then
      pure
        ( reverse $ first : takeWhile (/= first) list,
          reverse $ dropWhile (/= first) list
        )
    else findCycle first (first : list)

fetchPage :: String -> IO L.ByteString
fetchPage article = simpleHttp ("https://en.wikipedia.org/wiki/" ++ article)

-- getLinkReferences :: L.ByteString -> [String]
getLinkReferences :: (StringLike a, Show a) => [Tag a] -> [String]
getLinkReferences =
  filter (not . (':' `elem`))
    . map (drop 6)
    . filter ("/wiki/" `isPrefixOf`)
    . map (unQuote . show . (\(TagOpen _ ((_, href) : _)) -> href) . head)
    . partitions (~== "<a>")

-- | anything that matches the predicate is put as a separate element in the
-- output list
-- >>> breakOnAll (== '#') "foo#bar##baz" == ["foo", "#", "bar", "#", "#", bar"]
breakOnAll :: (a -> Bool) -> [a] -> [[a]]
breakOnAll p xs = case break p xs of
  (everything, []) -> [everything] -- no break
  (start, x : xs) -> [start] ++ [[x]] ++ breakOnAll p xs

-- | As a precondition it is required that all parenthesis are properly nested.
-- If this precondition is unmet behavior is undefined
unsafeSkipParensNested :: HasCallStack => [Tag C.ByteString] -> [Tag C.ByteString]
unsafeSkipParensNested = go 0 . (>>= splitParensOut)
  where
    splitParensOut = \case
      TagText str ->
        map (TagText . C.pack)
          . breakOnAll ((||) <$> (== '(') <*> (== ')'))
          . C.unpack
          $ str
      x -> [x]
    (op, cp) = (TagText (C.pack "("), TagText (C.pack ")"))
    anyParens = (||) <$> (== op) <*> (== cp)
    ifUnnested :: HasCallStack => Int -> [Tag C.ByteString] -> [Tag C.ByteString]
    ifUnnested nestingDepth s
      | nestingDepth < 0 = error "negative nesting depth"
      | nestingDepth == 0 = s
      | otherwise = []
    go nestingDepth chars = case break anyParens chars of
      (everything, []) -> ifUnnested nestingDepth everything
      (start, x : rest)
        | x == op -> ifUnnested nestingDepth start ++ go (nestingDepth + 1) rest
        | x == cp ->
          if nestingDepth == 0
            then -- ignore bad nesting that occurs when unmatched
            -- close parens is found
              start ++ go 0 rest
            else -- we are in an impossible state or nestingDepth > 1
            -- so no need to include start ever
              go (nestingDepth - 1) rest
      (start, end) -> error $ "impossible: " ++ show start ++ "," ++ show end

getBody :: C.ByteString -> [Tag C.ByteString]
getBody =
  -- TODO: this breaks on articles that have <p> tags in their info box
  -- An example of this is Belief which should have attitude as its link
  -- but instead gets Justification from the box saying that this article is
  -- part of a series on Epistemology
  unsafeSkipParensNested
    . dropWhile (/= TagOpen (C.pack "p") [])
    . dropWhile (~/= "<div id=mw-content-text")
    . parseTags

unQuote :: String -> String
unQuote l = init $ tail l

findBottomLevelPTag :: [L.ByteString] -> [Tag L.ByteString] -> [Tag L.ByteString]
findBottomLevelPTag _ [] = error "ran out of tags"
findBottomLevelPTag [] (t : ts) =
  case t of
    TagOpen n _
      | n == C.pack "p" -> ts
      | otherwise -> findBottomLevelPTag [n] ts
    _ -> findBottomLevelPTag [] ts
findBottomLevelPTag s@(x : xs) (t : ts)
  | TagOpen x [] ~== t = findBottomLevelPTag (x : s) ts
  | TagClose x ~== t = findBottomLevelPTag xs ts
  | otherwise = findBottomLevelPTag s ts

removeCoordinateReferences :: StringLike a => [Tag a] -> [Tag a]
removeCoordinateReferences tags =
  if tags !! 1 ~== "<span id=coordinates>"
    then dropWhile (~/= "<p>") tags
    else tags
