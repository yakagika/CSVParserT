-- file CSV.Text.hs
-- CSV parser for the people who shoult do everithing within Hasekell Platform.


{-# LANGUAGE BangPatterns, TemplateHaskell, StrictData, Strict #-}

module CSV.Text             ( parseCSV
                            , parseCSVErr
                            , readCSVWin
                            , readCSV
                            , writeCSV
                            , hPutCsvLn
                            , toCsvText
                            , loadCSV
                            , getSingleCol
                            , getTwoColAsMap
                            , transpose )         where

import              Data.List.Utils             (replace)
import qualified    Data.Text                   as T
import              Data.Text                   (Text)
import qualified    Data.Text.IO                as TO
import qualified    Data.List                   as L
import qualified    Data.Map.Strict             as Map
import              Data.Map.Strict             (Map)
import              Data.Attoparsec.Text
import              Control.Applicative
import              Data.Maybe
import              System.IO
import              Control.DeepSeq
import              Data.Either
import              Language.Haskell.TH
import              Language.Haskell.TH.Syntax  (Lift(..))
import              Control.Parallel.Strategies hiding (parMap)
import              Control.Parallel
import              Text.Show.Unicode           (ushow)

------------------------------------------------------------------
-- * Genral Function
------------------------------------------------------------------
{-# INLINE parMap #-}
parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f !(a:as)  = do
    b  <- rpar (f a)
    bs <- parMap f as
    return (b:bs)

{-# INLINE transpose #-}
transpose :: [[Text]] -> [[Text]]
transpose mx = runEval $ parMap (getIndices mx) $ [0 .. ((lenLine mx )- 1)]
    where
     lenLine :: [[Text]] -> Int
     lenLine line = L.maximum $ L.map L.length line

     getIndices :: [[Text]] -> Int -> [Text]
     getIndices xs i = L.map (\x -> if (L.length x) > i then x !! i else T.empty) xs

------------------------------------------------------------------
-- * Parser
------------------------------------------------------------------
{-# INLINE noneOf #-}
noneOf cs           = satisfy (\c -> not (elem c cs))

{-# INLINE csvFile #-}
csvFile = sepBy line eol

{-# INLINE line #-}
line = sepBy cell (char ',')

{-# INLINE cell #-}
cell = (quotedCell <|> many' (noneOf ",\n\r")) >>= (\res -> return $! T.pack res)

{-# INLINE quotedCell #-}
quotedCell =  char  '"'
           >> many' quotedChar >>= \content
           -> (char '"' <?> "quote at end of cell")
           >> return content

{-# INLINE quotedChar #-}
quotedChar =
        noneOf "\""
    <|> try (string  (T.pack "\"\"") >> return '"')

eol =   try (string (T.pack "\n\r"))
    <|> try (string (T.pack "\r\n"))
    <|> string (T.pack "\n")
    <|> string (T.pack "\r")
    <?> "end of line"

{-# INLINE parseCSV #-}
parseCSV :: Text -> Result [[Text]]
parseCSV input = parse csvFile input

{-# INLINE parseCSVErr #-}
parseCSVErr :: Text -> [[Text]]
parseCSVErr input = case parseOnly csvFile input of
    Right r     -> r
    Left  err   -> error $ "Can not parse :" ++ ushow err

------------------------------------------------------------------
-- * Input and Output
------------------------------------------------------------------

-- ^ encoding for Japanese on Windows
cpWin = "cp932"
type Encode = String

-- | note : lines を使っているので \r\n がUnixでは処理できない
readCSVWin :: FilePath -> IO [[Text]]
readCSVWin path    =   openFile path ReadMode  >>= \h
                    ->  mkTextEncoding cpWin
                    >>= hSetEncoding h
                    >>  TO.hGetContents h      >>= \cs
                    ->  return $ concat . runEval $ parMap parseCSVErr $ T.lines cs

-- | note : lines を使っているので \r\n がUnixでは処理できない
readCSV :: FilePath -> IO [[Text]]
readCSV path   = openFile  path ReadMode   >>= \h
                ->  TO.hGetContents h      >>= \cs
                ->  return $ concat . runEval $ parMap parseCSVErr $ T.lines cs

-- | Convert to CSV format
{-# INLINE toCsvStr #-}
toCsvStr :: String -> String
toCsvStr  = ((<> "\"") . ("\"" <>)) . (replace "\"" "\"\"")

{-# INLINE toCsvText #-}
toCsvText :: Text -> Text
toCsvText  = ((<> quo) . (quo <>)) . (T.replace quo quoq)
    where
        quo  = T.pack "\""
        quoq = T.pack "\"\""

-- | output File as a CSV
{-# INLINE hPutCsvLn #-}
hPutCsvLn :: Handle -> [Text] ->  IO ()
hPutCsvLn wHandle = (TO.hPutStrLn wHandle)
                    .T.concat
                    .(L.intersperse (T.pack ","))
                    . map toCsvText


-- | Use this if output data is not written in CSV formart
-- once you use parser, use this.
writeCSV :: FilePath -> [[Text]] -> IO ()
writeCSV path xs   =  openFile path WriteMode >>= \handle
                    -> mapM_ (hPutCsvLn handle) xs
                    >> hClose handle

-- | Use this for CSV formatted files
writeData :: FilePath -> [[Text]] -> IO ()
writeData path xs   =  openFile path WriteMode >>= \handle
                    -> mapM_ ((TO.hPutStrLn handle).T.concat.(L.intersperse (T.pack ","))) xs
                    >> hClose handle



------------------------------------------------------------------
-- * Load ; for TemplateHaskell
------------------------------------------------------------------

-- instance Lift Text where
--   lift t = [| T.pack $(lift $ T.unpack t) |]

{- | Load Csv File while compiling

use this like,

aFile = getSingleCol $( loadCSVT "hoge.csv")

-}
loadCSV :: FilePath -> Q Exp
loadCSV filepath = do
    cs <-  runIO $ TO.readFile filepath
    [e| cs |]

getSingleCol :: Text -> [Text]
getSingleCol ts = head $ transpose $ parseCSVErr ts

getTwoColAsMap :: Text -> Map Text Text
getTwoColAsMap ts =  let ys = transpose (parseCSVErr ts)
          in Map.fromList $ zip (ys L.!! 0) (ys L.!! 1)














































