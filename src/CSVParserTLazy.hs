-- file csvParserT.hs

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module CSVParserTLazy   ( parseCSVT
                        , parseCSVTErr
                        , readCSVTWin
                        , readCSVT
                        , writeCSVT
                        , hPutCsvLn
                        , toCsvText
                        , loadCSVT)            where

import qualified    Data.Text                   as T
import              Data.Text                   (Text)
import qualified    Data.Text.Lazy              as TL    
import qualified    Data.Text.IO                as TO
import qualified    Data.Text.Lazy.IO           as TLO
import qualified    Data.List                   as L
import              Text.Parsec
import              Text.Parsec.Text.Lazy
import              Data.Maybe
import              System.IO
import              Control.DeepSeq
import              Data.Either
import Language.Haskell.TH
import Control.Parallel.Strategies hiding (parMap)
import Control.Parallel


------------------------------------------------------------------
-- * Genral Function
------------------------------------------------------------------

-- List Utils for Haskell Platform Environment
startswith :: Eq a => [a] -> [a] -> Bool
startswith = L.isPrefixOf


spanList :: ([a] -> Bool) -> [a] -> ([a], [a])

spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (startswith delim) str
        in
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim
                                                 (drop (length delim) x)

join :: [a] -> [[a]] -> [a]
join delim l = concat (L.intersperse delim l)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = join new . split old $ l

parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f !(a:as)  = do
    b  <- rpar (f a)
    bs <- parMap f as
    return (b:bs) 


------------------------------------------------------------------
-- * Parser
------------------------------------------------------------------

csvFile = sepBy line eol 
line = sepBy cell (char ',') 
cell = (quotedCell <|> many (noneOf ",\n\r")) >>= (\res -> return $! TL.pack res)
  
quotedCell =  char  '"'
           >> many quotedChar >>= \content 
           -> (char '"' <?> "quote at end of cell")
           >> return content

quotedChar = 
        noneOf "\""
    <|> try (string  "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSVT :: TL.Text -> Either ParseError [[TL.Text]]
parseCSVT input = parse csvFile "Can not Parse Strings" input

parseCSVTErr :: TL.Text -> [[TL.Text]]
parseCSVTErr input = case parse csvFile "Can not Parse Strings" input of 
    Right a     -> a
    Left  err   -> error $ "Can not parse :" ++ show err 

------------------------------------------------------------------
-- * Input and Output 
------------------------------------------------------------------

-- ^ encoding for Japanese on Windows
cpWin = "cp932" 
type Encode = String 

readCSVTWin :: FilePath -> IO [[TL.Text]]
readCSVTWin path    =   openFile path ReadMode  >>= \h 
                    ->  mkTextEncoding cpWin 
                    >>= hSetEncoding h
                    >>  TLO.hGetContents h      >>= \cs 
                    ->  return $ concat . runEval $ parMap parseCSVTErr $ TL.lines cs 


readCSVT :: FilePath -> IO [[TL.Text]]
readCSVT path   = openFile  path ReadMode   >>= \h 
                ->  TLO.hGetContents h      >>= \cs 
                ->  return $ concat . runEval $ parMap parseCSVTErr $ TL.lines cs 


-- String をCSVとして出力可能にする.
toCsvStr :: String -> String
toCsvStr  = ((<> "\"") . ("\"" <>)) . (replace "\"" "\"\"")  

toCsvText :: TL.Text -> TL.Text 
toCsvText  = ((<> quo) . (quo <>)) . (TL.replace quo quoq)  
    where 
        quo  = TL.pack "\""
        quoq = TL.pack "\"\""

hPutCsvLn :: Handle -> [TL.Text] ->  IO ()
hPutCsvLn wHandle = (TLO.hPutStrLn wHandle)
                    .TL.concat
                    .(L.intersperse (TL.pack ","))
                    . map toCsvText


writeCSVT :: FilePath -> [[TL.Text]] -> IO ()
writeCSVT path xs   =  openFile path WriteMode >>= \handle 
                    -> mapM_ (hPutCsvLn handle) xs
                    >> hClose handle

------------------------------------------------------------------
-- * Load ; for TemplateHaskell
------------------------------------------------------------------
-- | Load Csv File while compiling 
loadCSVT :: FilePath -> Q Exp
loadCSVT filepath = do 
    str <- runIO $ readCSVT filepath
    [e| x |]





















































