import Control.Monad
import Network.HTTP.Conduit
import System.Environment
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Text.HTML.TagSoup
import Data.List

main = do
    args <- getArgs
    if length args == 1
        then do
            (cycle, path) <- findFirstCycle (args !! 0) [args !! 0]
            putStrLn $ (args !! 0)
                ++ " has a path of length "
                ++ (show $ length path) ++ ":"
            print path
            putStrLn $ "with a cycle of length "
                ++ (show $ length cycle) ++ ":"
            print cycle
        else
            putStrLn "Usage: ./wiki_depth <wikipedia-page>"

-- Returns the cycle and the path to the cycle
findFirstCycle :: String -> [String] -> IO ([String], [String])
findFirstCycle url list = do
    text <- fetchPage (url)
    let first = head $ getLinkReferences text
    if first `elem` list
        then return $ ( reverse $ first : takeWhile (/=first) list
                      , reverse $ dropWhile (/=first) list
                      )
        else findFirstCycle first (first : list)


fetchPage :: String -> IO L.ByteString
fetchPage article = simpleHttp ("https://en.wikipedia.org/wiki/" ++ article)

unQuote :: String -> String
unQuote l = take (length l - 2) $ drop 1 l

getLinkReferences :: L.ByteString -> [String]
getLinkReferences text =
    filter (not . (':' `elem`)) $
    map (drop 6) $
    filter ("/wiki/" `isPrefixOf`) $
    map (unQuote . show . (\(TagOpen _ ((_,href):_)) -> href) . head) $
    partitions (~== "<a>") $
    takeWhile (~/= "<div class=mw-headline") $
    dropWhile (~/= "<p>") $
    parseTags text
