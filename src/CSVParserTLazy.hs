-- file csvParserT.hs

{-# LANGUAGE BangPatterns #-}

module CSVParserTLazy  (parseCSVT,readCSVTWin,readCSVT,writeCSVT) where

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
import Control.Parallel.Strategies hiding (parMap)
import Control.Parallel


------------------------------------------------------------------
-- Genral Function
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
-- Parser
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



-- encoding for Japanese on Windows
cpWin = "cp932" 
type Encode = String 

readCSVTWin :: FilePath -> IO [[TL.Text]]
readCSVTWin path    =   openFile path ReadMode  >>= \h 
                    ->  mkTextEncoding cpWin 
                    >>= hSetEncoding h
                    >>  TLO.hGetContents h      >>= \cs 
                    ->  return $ concat . runEval $ parMap ((fromRight []) . parseCSVT) $ TL.lines cs 
                    -- Errorも扱えるようにする


readCSVT :: FilePath -> IO [[TL.Text]]
readCSVT path   = openFile  path ReadMode   >>= \h 
                ->  TLO.hGetContents h      >>= \cs 
                ->  return $ concat . runEval $ parMap ((fromRight []) . parseCSVT) $ TL.lines cs 

{-
                        
                        Left e  -> putStrLn "Error parsing input:"
                                >> print e >> return [] 
                        Right r ->  return r            
-}

-- String をCSVとして出力可能にする.
toCsvStr :: String -> String
toCsvStr  = ((<> "\"") . ("\"" <>)) . (replace "\"" "\"\"")  

hPutCsvLn :: Handle -> [TL.Text] ->  IO ()
hPutCsvLn wHandle = (TLO.hPutStrLn wHandle)
                    .TL.concat
                    .(L.intersperse (TL.pack ","))
                    

writeCSVT :: FilePath -> [[TL.Text]] -> IO ()
writeCSVT path xs   =  openFile path WriteMode >>= \handle 
                    -> mapM_ (hPutCsvLn handle) xs
                    >> hClose handle


