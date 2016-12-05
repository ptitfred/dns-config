module Lib
    ( readZone
    , printZone
    , query
    , Query
    , Record
    , Zone
    , mxRecords
    , cnameRecords
    , aRecords
    , txtRecords
    , ofType
    ) where

import Control.Monad (guard)
import Data.Char     (isSpace)
import Data.List     (intercalate)
import Data.Maybe    (catMaybes)

import Text.Parsec
import Text.Parsec.String

printZone :: Zone -> String
printZone (Zone mtl records) = intercalate "\n" formattedLines
  where headers  Nothing  = []
        headers (Just tl) = [unwords ["$TTL", show tl]]
        formattedLines = headers mtl ++ map (prettyShow indent) records
        indent = replicate indentWidth ' '
        indentWidth = maximum $ map nameLength records
        nameLength (Explicit n _ _ _ _) = length n
        nameLength _ = 0

simplifyZone :: Zone -> Zone
simplifyZone (Zone mtl []) = Zone mtl []
simplifyZone (Zone mtl [r]) = Zone mtl [r]
simplifyZone (Zone mtl (r:rs)) = Zone mtl (r:rs')
  where rs' = map shrinkClass rs
        shrinkClass (Explicit     n t v tl _) = Explicit     n t v tl Nothing
        shrinkClass (Continuation   t v tl _) = Continuation   t v tl Nothing

type Query = Zone -> [Record]
type RecordPredicate = Record -> Bool

query :: RecordPredicate -> Query
query p (Zone _ records) = filter p records

mxRecords :: Query
mxRecords = query (ofType MX)

cnameRecords :: Query
cnameRecords = query (ofType CNAME)

aRecords :: Query
aRecords = query (ofType A)

txtRecords :: Query
txtRecords = query (ofType TXT)

ofType :: RecordType -> RecordPredicate
ofType t1 (Explicit   _ t2 _ _ _) = t1 == t2
ofType t1 (Continuation t2 _ _ _) = t1 == t2

data Zone = Zone (Maybe TTL) [Record] deriving Show

data Record = Explicit RecordName RecordType RecordValue (Maybe TTL) (Maybe Class)
            | Continuation RecordType RecordValue (Maybe TTL) (Maybe Class) deriving Show

type RecordName  = String
data RecordType  = SOA | NS | MX | A | AAAA | TXT | SRV | CNAME | PTR deriving (Show, Eq)
type DomainName  = String
data RecordValue = Name DomainName
                 | IPv4 String
                 | IPv6 String
                 | StringValue String
                 | MailExchange Int String
                 | Service Int Int Int String
                 | ServerAuthority DomainName DomainName Serial Refresh Retry Expire Minimum
                 deriving Show

type Serial  = Int
type Refresh = Int
type Retry   = Int
type Expire  = Int
type Minimum = Int

type TTL = Int
data Class = IN | CS | CH | HS deriving Show

prettyShow :: String -> Record -> String
prettyShow indent (Explicit     n t v tl cl) = showRecord n' tl cl t v
  where n' = n ++ drop (length n) indent
prettyShow indent (Continuation   t v tl cl) = showRecord indent tl cl t v

prettyShowRecordType :: RecordType -> String
prettyShowRecordType SOA   = "SOA  "
prettyShowRecordType NS    = "NS   "
prettyShowRecordType MX    = "MX   "
prettyShowRecordType A     = "A    "
prettyShowRecordType AAAA  = "AAAA "
prettyShowRecordType TXT   = "TXT  "
prettyShowRecordType SRV   = "SRV  "
prettyShowRecordType CNAME = "CNAME"
prettyShowRecordType PTR   = "PTR  "

prettyShowRecordValue :: RecordValue -> String
prettyShowRecordValue (Name s)             = s
prettyShowRecordValue (IPv4 s)             = s
prettyShowRecordValue (IPv6 s)             = s
prettyShowRecordValue (StringValue s)      = s
prettyShowRecordValue (MailExchange p v)   = unwords [show p, v]
prettyShowRecordValue (Service i1 i2 i3 v) = unwords [show i1, show i2, show i3, v]
prettyShowRecordValue (ServerAuthority n1 n2 i1 i2 i3 i4 i5) =
  unwords [n1, n2] ++ " (" ++ unwords [show i1, show i2, show i3, show i4, show i5] ++ ")"

showRecord :: String -> Maybe TTL -> Maybe Class -> RecordType -> RecordValue -> String
showRecord n tl cl t v = unwords [n, tl', cl', t', v']
  where tl' = maybe "   " show tl
        cl' = maybe  "  " show cl
        t'  = prettyShowRecordType t
        v'  = prettyShowRecordValue v

readZone :: FilePath -> IO (Either String Zone)
readZone file = fmap simplifyZone . readMessage . parse zoneParser "" <$> readFile file

readMessage :: Either ParseError a -> Either String a
readMessage (Left pe) = Left (show pe)
readMessage (Right r) = Right r

zoneParser :: Parser Zone
zoneParser = Zone <$> globalTtl
                  <*> records
                  <*  spaces <* eof
  where globalTtl = optionMaybe (string "$TTL" *> space *> ttl <* newline)
        records = catMaybes <$> many record

ttl :: Parser TTL
ttl = int

int :: Parser Int
int = read <$> many1 digit

class' :: Parser Class
class' = tries [ "IN" ~> IN
               , "CS" ~> CS -- TODO implement / raise ?
               , "CH" ~> CH -- TODO implement / raise ?
               , "HS" ~> HS -- TODO implement / raise ?
               ]

record :: Parser (Maybe Record)
record = tries [ Nothing <$ manyTill space endOfLine
               , Just <$> record'
               ]

record' :: Parser Record
record' = do
  n <- nature
  (mttl, cl) <- ttlAndClass
  t <- recordType <* inlineSpaces1
  v <- recordValue t
  return (mkRecord n t v mttl cl)
    where nature = tries [ Nothing <$  space <* spaces
                         , Just    <$> recordName <* inlineSpaces1
                         ]
          maybeTtl    = optionMaybe (ttl <* inlineSpaces1)
          maybeClass  = optionMaybe (class' <* inlineSpaces1)
          ttlAndClass = tries [ (,)      <$> maybeTtl   <*> maybeClass
                              , flip (,) <$> maybeClass <*> maybeTtl
                              ]
          mkRecord (Just en) = Explicit en
          mkRecord  Nothing  = Continuation

inlineSpaces1 :: Parser String
inlineSpaces1 = many1 inlineSpace

inlineSpaces :: Parser String
inlineSpaces = many inlineSpace

inlineSpace :: Parser Char
inlineSpace = satisfy (\ c -> isSpace c && not (isNewline c))
  where isNewline c = c == '\n' || c == '\r'

recordName :: Parser RecordName
recordName = many1 notSpace
  where notSpace = satisfy (not.isSpace)

recordType :: Parser RecordType
recordType = tries [ "SOA"   ~> SOA
                   , "NS"    ~> NS
                   , "MX"    ~> MX
                   , "AAAA"  ~> AAAA
                   , "A"     ~> A
                   , "TXT"   ~> TXT
                   , "SRV"   ~> SRV
                   , "CNAME" ~> CNAME
                   , "PTR  " ~> PTR
                   ]

(~>) :: String -> a -> Parser a
s ~> v = v <$ string s

tries :: [Parser a] -> Parser a
tries = choice . map try

dot :: Parser String
dot = string "."

byte :: Parser String
byte = do
  i <- int
  guard (i >= 0 && i <= 255)
  return (show i)

ipv4 :: Parser String
ipv4 = sentence [byte, dot, byte, dot, byte, dot, byte]

ipv6 :: Parser String
ipv6 = intercalate "::" <$> parts
  where colon = string ":"
        singleColon = colon !> colon
        doubleColon = string "::"
        doubleByte = many1 hexDigit
        bytes = doubleByte `sepBy1` singleColon
        part = intercalate ":" <$> bytes
        parts = atMost 2 $ part `sepBy1` doubleColon

atMost :: Int -> Parser [a] -> Parser [a]
atMost most p = do
  v <- p
  guard (length v <= most)
  return v

(!>) :: Show b => Parser a -> Parser b -> Parser a
p !> failure = try $ do
  v <- p
  notFollowedBy failure
  return v

sentence :: [Parser String] -> Parser String
sentence = fmap concat . sequence

recordValue :: RecordType -> Parser RecordValue
recordValue A     = IPv4 <$> ipv4
recordValue AAAA  = IPv6 <$> ipv6
recordValue CNAME = Name <$> domainName
recordValue PTR   = Name <$> domainName
recordValue NS    = Name <$> text
recordValue MX    = MailExchange <$> int <* inlineSpaces1
                                 <*> text
recordValue SRV   = Service <$> int <* inlineSpaces1
                            <*> int <* inlineSpaces1
                            <*> int <* inlineSpaces1
                            <*> text
recordValue SOA   = ServerAuthority <$> (inlineSpaces *> domainName <* spaces)
                                    <*> domainName <* spaces
                                    <*  try (optional (char '(' <* inlineSpaces))
                                    <*> int <* inlineSpaces1
                                    <*> int <* inlineSpaces1
                                    <*> int <* inlineSpaces1
                                    <*> int <* inlineSpaces1
                                    <*> int
                                    <*  try (optional (inlineSpaces <* char ')'))
recordValue _     = StringValue <$> text

domainName :: Parser DomainName
domainName = anyChar `manyTill` space -- TODO see https://tools.ietf.org/html/rfc1034

text :: Parser String
text = manyTill anyChar (() <$ endOfLine <|> eof)
