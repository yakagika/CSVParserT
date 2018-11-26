--file csvParser.hs

module CSVParserT  (parseCSVT,readCSVTWin,readCSVT) where

import qualified    Data.Text           as T
import qualified    Data.Text.IO        as TO
import              Text.Parsec
import              Text.Parsec.Text
import              Data.Maybe
import              System.IO

csvFile = do x <- sepBy line eol
             eof
             return x

line = sepBy cell (char ',') 

cell = (quotedCell <|> many (noneOf ",\n\r")) >>= return . T.pack
  
quotedCell = 
    do char  '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string  "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSVT :: T.Text -> Either ParseError [[T.Text]]
parseCSVT input = parse csvFile "Can not Parse Strings" input



-- encoding for Japanese on Windows
cpWin = "cp932" 
type Encode = String 
type Path = String

readCSVTWin :: Path -> IO [[T.Text]]
readCSVTWin path    = openFile path ReadMode
                    >>= \h -> mkTextEncoding cpWin 
                    >>= hSetEncoding h
                    >> TO.hGetContents h 
                    >>= \cs ->  
                    case parseCSVT cs of
                        Left e  -> putStrLn "Error parsing input:"
                                >> print e >> return []
                    
                        Right r ->  return r
                     

readCSVT :: Path -> IO [[T.Text]]
readCSVT path   = openFile  path ReadMode   >>= \h 
                ->  TO.hGetContents h      >>= \cs 
                ->  case parseCSVT cs of
                        Left e  -> putStrLn "Error parsing input:"
                                >> print e >> return []
                        Right r ->  return r

