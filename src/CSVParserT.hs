-- file csvParserT.hs

{-# LANGUAGE BangPatterns #-}

module CSVParserT  (parseCSVT,readCSVTWin,readCSVT,writeCSVT) where

import qualified    Data.Text                   as T
import              Data.Text                   (Text)
import qualified    Data.Text.Lazy              as TL    
import qualified    Data.Text.IO                as TO
import qualified    Data.Text.Lazy.IO           as TLO
import qualified    Data.List                   as L
import              Text.Parsec
import              Text.Parsec.Text
import              Data.Maybe
import              System.IO
import              Control.DeepSeq
import              Data.Either
import Control.Parallel.Strategies hiding (parMap)
import Control.Parallel

parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f !(a:as)  = do
    b  <- rpar (f a)
    bs <- parMap f as
    return (b:bs) 


csvFile = sepBy line eol 
line = sepBy cell (char ',') 
cell = (quotedCell <|> many (noneOf ",\n\r")) >>= (\res -> return $! T.pack res)
  
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

parseCSVT :: T.Text -> Either ParseError [[Text]]
parseCSVT input = parse csvFile "Can not Parse Strings" input



-- encoding for Japanese on Windows
cpWin = "cp932" 
type Encode = String 

readCSVTWin :: FilePath -> IO [[Text]]
readCSVTWin path    =   openFile path ReadMode  >>= \h 
                    ->  mkTextEncoding cpWin 
                    >>= hSetEncoding h
                    >>  TLO.hGetContents h      >>= \cs 
                    ->  return $ concat . runEval $ parMap ((fromRight []) . parseCSVT . TL.toStrict) $ TL.lines cs 
                    -- Errorも扱えるようにする


readCSVT :: FilePath -> IO [[Text]]
readCSVT path   = openFile  path ReadMode   >>= \h 
                ->  TLO.hGetContents h      >>= \cs 
                ->  return $ concat . runEval $ parMap ((fromRight []) . parseCSVT . TL.toStrict) $ TL.lines cs 

{-
                        
                        Left e  -> putStrLn "Error parsing input:"
                                >> print e >> return [] 
                        Right r ->  return r            
-}

writeCSVT :: FilePath -> [[T.Text]] -> IO ()
writeCSVT path xs   =  openFile path WriteMode >>= \handle 
                    -> mapM_ ((TO.hPutStrLn handle) . T.concat . (L.intersperse (T.pack ","))) xs
                    >> hClose handle


-- for test
main = do 
    readCSVTWin "H24.txt" >>=  mapM_ (mapM_ TO.putStrLn)

