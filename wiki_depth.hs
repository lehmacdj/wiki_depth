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
            let first = args !! 0
            (cycle, path) <- findCycle first [first]
            let path_length  = show $ length path
            putStrLn $ first
                ++ " has a path of length "
                ++ path_length
                ++ ":"
            printListIndented path
            let cycle_length = show $ length cycle
            putStrLn $ "with a cycle of length " ++ cycle_length ++ ":"
            printListIndented cycle
        else
            putStrLn "Usage: ./wiki_depth <wikipedia-page>"

printListIndented :: [String] -> IO ()
printListIndented list =
    putStrLn $ "    " ++ foldl1 (\acc s -> acc ++ "\n    " ++ s) list

-- Returns the cycle and the path to the cycle
findCycle :: String -> [String] -> IO ([String], [String])
findCycle url list = do
    text <- fetchPage (url)
    let first = head $ getLinkReferences text
    if first `elem` list
        then return $ ( reverse $ first : takeWhile (/=first) list
                      , reverse $ dropWhile (/=first) list
                      )
        else findCycle first (first : list)


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
