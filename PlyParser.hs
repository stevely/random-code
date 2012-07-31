{-
  PlyParser.hs
  By Steven Smith
-}

import System.Environment
import Data.Maybe
import Text.ParserCombinators.Parsec

data Header = Header {
    format   :: Format,
    elements :: [Element]
    }
    deriving Show

data Format = Ascii | BinaryBigEndian | BinaryLittleEndian
    deriving Show

data Element = Element {
    name  :: String,
    ecount :: Int,
    props :: [Property]
    }
    deriving Show

data Property = Property {
    pname :: String,
    ptype :: PropType
    }
    deriving Show

data PropType = Small PropSmallType
              | PList PropSmallType PropSmallType
    deriving Show

data PropSmallType = PropChar Int
                   | PropUChar Int
                   | PropShort Int
                   | PropUShort Int
                   | PropInt Int
                   | PropUInt Int
                   | PropFloat Int
                   | PropDouble Int
    deriving Show

headerParser = do
    fmt <- formatParser
    els <- many elementParser 
    return $ Header {
        format = fmt,
        elements = catMaybes els
        }

formatParser = do { string "format "
                  ; fmt <- fmtTypeParser
                  ; string " 1.0"
                  ; newline
                  ; return fmt
                  }
             <?> "format"

fmtTypeParser = do { string "ascii"
                   ; return Ascii
                   }
            <|> do { try $ string "binary_big_endian"
                   ; return BinaryBigEndian
                   }
            <|> do { string "binary_little_endian"
                   ; return BinaryLittleEndian
                   }

elementParser = do { string "comment "
                   ; many $ noneOf "\n"
                   ; newline
                   ; return Nothing
                   }
            <|> do { try $ string "element "
                   ; ename <- many1 $ letter <|> char '_'
                   ; char ' '
                   ; digits <- many1 digit
                   ; newline
                   ; ps <- many1 propertyParser
                   ; return $ Just $ Element {
                        name = ename,
                        ecount = read digits,
                        props = ps
                        }
                   }
             <?> "element"

propertyParser = do { string "property "
                    ; typ <- propTypeParser
                    ; char ' '
                    ; name <- many1 $ letter <|> char '_'
                    ; newline
                    ; return $ Property {
                        pname = name,
                        ptype = typ
                        }
                    }
                <?> "property"

propTypeParser = do { small <- propSmallTypeParser
                    ; return $ Small small
                    }
             <|> do { string "list "
                    ; l <- propSmallTypeParser
                    ; char ' '
                    ; r <- propSmallTypeParser
                    ; return $ PList l r
                    }
             <?> "property type"

propSmallTypeParser = do { try $ string "char"
                         ; n <- option "8" bitsParser
                         ; return $ PropChar (read n)
                         }
                  <|> do { try $ string "uchar"
                         ; n <- option "8" bitsParser
                         ; return $ PropUChar (read n)
                         }
                  <|> do { try $ string "short"
                         ; n <- option "16" bitsParser
                         ; return $ PropShort (read n)
                         }
                  <|> do { try $ string "ushort"
                         ; n <- option "16" bitsParser
                         ; return $ PropUShort (read n)
                         }
                  <|> do { try $ string "int"
                         ; n <- option "32" bitsParser
                         ; return $ PropInt (read n)
                         }
                  <|> do { try $ string "uint"
                         ; n <- option "32" bitsParser
                         ; return $ PropUInt (read n)
                         }
                  <|> do { try $ string "float"
                         ; n <- option "32" bitsParser
                         ; return $ PropFloat (read n)
                         }
                  <|> do { try $ string "double"
                         ; n <- option "64" bitsParser
                         ; return $ PropDouble (read n)
                         }
                  <?> "small property type"

bitsParser = choice $ map string ["8", "16", "32", "64"]

plyHeaderParser = do
    string "ply\n"
    header <- headerParser
    string "end_header\n"
    return header

-- For now we ignore formats other than ASCII
valuesParser header = mapM elementValueParser $ elements header

elementValueParser element = do { values <- count (ecount element) propsParse
                                ; return (name element, values)
                                }
                            <?> "element value"
          where propsParse = do { values <- mapM propertyValueParser $ props element
                                ; newline
                                ; return $ concat values
                                }

propertyValueParser property = do { values <- p property
                                  ; option ' ' $ char ' '
                                  ; return values
                                  }
                where p prop = case ptype prop of
                                Small t -> num >>= (\n -> return [n])
                                PList i t -> do { n <- index
                                                ; char ' '
                                                ; values <- count n (andSpace num)
                                                ; return values
                                                }
                      andSpace p = do { value <- p
                                      ; option ' ' $ char ' '
                                      ; return value
                                      }

index = do { digits <- many1 digit
           ; return (read digits :: Int)
           }

num = do { sign <- option ' ' $ char '-'
         ; left <- many1 digit
         ; right <- option "0" $ char '.' >> many1 digit
         ; return (read $ (sign:left) ++ ('.':right) :: Float)
         }
     <?> "num"

plyParser = do { header <- plyHeaderParser
               ; values <- valuesParser header
               ; return values
               }

main = do
    args <- getArgs
    case args of
        []       -> putStrLn "No file given."
        (file:_) -> parseFromFile plyParser file >>= (\x -> case x of
                        Left err -> print err
                        Right result -> print result)
