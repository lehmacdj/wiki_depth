import Control.Monad
import Network.HTTP.Conduit
import System.Environment
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
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
    let first = head $ getLinkReferences $ getBody text
    if first `elem` list
        then return $ ( reverse $ first : takeWhile (/=first) list
                      , reverse $ dropWhile (/=first) list
                      )
        else findCycle first (first : list)


fetchPage :: String -> IO L.ByteString
fetchPage article = simpleHttp ("https://en.wikipedia.org/wiki/" ++ article)

-- getLinkReferences :: L.ByteString -> [String]
getLinkReferences tags =
    filter (not . (':' `elem`)) $
    map (drop 6) $
    filter ("/wiki/" `isPrefixOf`) $
    map (unQuote . show . (\(TagOpen _ ((_,href):_)) -> href) . head) $
    partitions (~== "<a>") $ tags

getBody text =
    removeCoordinateReferences $
    findBottomLevelPTag [] $
    takeWhile (~/= "<div class=mw-headline") $
    drop 1 $
    dropWhile (~/= "<div id=mw-content-text") $
    parseTags text

unQuote :: String -> String
unQuote l = init $ tail l

findBottomLevelPTag :: [L.ByteString] -> [Tag L.ByteString] -> [Tag L.ByteString]
findBottomLevelPTag _ [] = error "ran out of tags"
findBottomLevelPTag [] (t:ts) =
    case t of TagOpen n _
                          | n == C.pack "p" -> ts
                          | otherwise -> findBottomLevelPTag [n] ts
              e -> findBottomLevelPTag [] ts
findBottomLevelPTag s@(x:xs) (t:ts)
    | TagOpen x [] ~== t = findBottomLevelPTag (x:s) ts
    | TagClose x ~== t = findBottomLevelPTag xs ts
    | otherwise = findBottomLevelPTag s ts

removeCoordinateReferences tags =
    if tags !! 1 ~== "<span id=coordinates>"
        then dropWhile (~/= "<p>") tags
        else tags
