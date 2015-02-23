-- # LANGUAGE OverloadedRecordFields #
module Data.WhatsApp where

import Control.Applicative
import Control.Monad
import Text.Parsec hiding ((<|>), many)

type Parser = Parsec String ()
data WhatsAppMessage = WhatsAppMessage {_date :: Date, _from :: String, _message :: String} deriving (Eq, Ord, Show)

parseWhatsAppChat :: String -> Either ParseError [WhatsAppMessage]
parseWhatsAppChat = runParser whatsAppChatParser () ""

parseWhatsAppChatFromFile :: String -> IO (Either ParseError  [WhatsAppMessage])
parseWhatsAppChatFromFile = readFile >=> return . parseWhatsAppChat

whatsAppChatParser :: Parser [WhatsAppMessage]
whatsAppChatParser = many whatsAppMessageParser

whatsAppMessageParser :: Parser WhatsAppMessage
whatsAppMessageParser = do
	dat      <- whatsAppDateParser
	from'    <- whatsAppNameParser
	message' <- manyTill anyChar $ try (void newline *> (lookAhead (void whatsAppDateParser) <|> eof))
	return $ WhatsAppMessage dat from' message'

whatsAppNameParser :: Parser String
whatsAppNameParser = manyTill anyChar $ string ": "

numberParser :: Parser Int
numberParser = many1 digit >>= \d -> return (read d :: Int)

data Date = Date {_year :: Int, _month :: Int, _day :: Int, _hour :: Int, _minute :: Int}
	deriving (Eq, Ord, Show)

clockParser :: Parser (Int, Int)
clockParser = do
	ho <- numberParser
	string ":"
	mi <- numberParser
	return (ho,mi)

monthParser :: Parser Int
monthParser =      (string "ene." *> pure 1)
	       <|>     (string "feb." *> pure 2)
	       <|> try (string "mar." *> pure 3)
	       <|> try (string "abr." *> pure 4)
	       <|> try (string "may." *> pure 5)
	       <|> try (string "jun." *> pure 6)
	       <|> try (string "jul." *> pure 7)
	       <|> try (string "ago." *> pure 8)
	       <|>     (string "sep." *> pure 9)
	       <|>     (string "oct." *> pure 10)
	       <|>     (string "nov." *> pure 11)
	       <|>     (string "dic." *> pure 12)

whatsAppDateParser :: Parser Date
whatsAppDateParser = try whatsAppDateParser1 <|> try whatsAppDateParser2

clockParser' :: Parser (Int, Int)
clockParser' = do
	string ", "
	(ho, mi) <- clockParser
	string " - "
	return (ho,mi)

whatsAppDateParser1 :: Parser Date
whatsAppDateParser1 = do
	d <- numberParser
	string " de "
	m <- monthParser
	(ho, mi) <- clockParser'
	return $ Date 2015 m d ho mi

whatsAppDateParser2 :: Parser Date
whatsAppDateParser2 = do
	d <- numberParser
	string "/"
	m <- numberParser
	string "/"
	y <- numberParser
	(ho, mi) <- clockParser'
	return $ Date y m d ho mi
