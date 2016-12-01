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

import Data.Char  (isSpace)
import Data.Maybe (catMaybes)

import Text.Parsec
import Text.Parsec.String

printZone :: Zone -> String
printZone (Zone mtl records) = unlines formattedLines
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
data RecordType  = SOA | NS | MX | A | TXT | SRV | CNAME | PTR deriving (Show, Eq)
data RecordValue = Name String
                 | IPv4 String
                 | StringValue String
                 | MailExchange Int String
                 | Service Int Int Int String

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
prettyShowRecordType TXT   = "TXT  "
prettyShowRecordType SRV   = "SRV  "
prettyShowRecordType CNAME = "CNAME"
prettyShowRecordType PTR   = "PTR  "

showRecord :: String -> Maybe TTL -> Maybe Class -> RecordType -> RecordValue -> String
showRecord n (Just tl) (Just cl) t v = unwords [n, show tl, show cl, prettyShowRecordType t, show v]
showRecord n (Just tl)  Nothing  t v = unwords [n, show tl,    "  ", prettyShowRecordType t, show v]
showRecord n  Nothing  (Just cl) t v = unwords [n,   "   ", show cl, prettyShowRecordType t, show v]
showRecord n  Nothing   Nothing  t v = unwords [n,   "   ",    "  ", prettyShowRecordType t, show v]

instance Show RecordValue where
  show (Name s) = s
  show (IPv4 s) = s
  show (Service i1 i2 i3 v) = unwords [show i1, show i2, show i3, v]
  show (MailExchange p v) = unwords [show p, v]
  show (StringValue s) = s

readZone :: FilePath -> IO (Either String Zone)
readZone file = fmap simplifyZone . readMessage . parse zoneParser "" <$> readFile file

readMessage :: Either ParseError a -> Either String a
readMessage (Left pe) = Left (show pe)
readMessage (Right r) = (Right r)

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
               , "CS" ~> CS
               , "CH" ~> CH
               , "HS" ~> HS
               ]

record :: Parser (Maybe Record)
record = tries [ Nothing <$ manyTill space endOfLine
               , Just <$> record'
               ]

record' :: Parser Record
record' = do
  n <- tries [ Nothing <$  space <* spaces
             , Just    <$> recordName <* inlineSpaces1
             ]
  (mttl, cl) <- tries [ (,)      <$> maybeTtl   <*> maybeClass
                      , flip (,) <$> maybeClass <*> maybeTtl
                      ]
  t    <- recordType    <* inlineSpaces1
  v    <- recordValue t
  return $ case n of Just en -> Explicit en  t v mttl cl
                     Nothing -> Continuation t v mttl cl
    where maybeTtl = optionMaybe (ttl <* inlineSpaces1)
          maybeClass = optionMaybe (class' <* inlineSpaces1)

inlineSpaces1 :: Parser String
inlineSpaces1 = many1 inlineSpace

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

recordValue :: RecordType -> Parser RecordValue
recordValue A     = IPv4 <$> text
recordValue CNAME = Name <$> text
recordValue PTR   = Name <$> text
recordValue NS    = Name <$> text
recordValue MX    = MailExchange <$> int <* inlineSpaces1
                                 <*> text
recordValue SRV   = Service <$> int <* inlineSpaces1
                            <*> int <* inlineSpaces1
                            <*> int <* inlineSpaces1
                            <*> text
recordValue _     = StringValue <$> text

text :: Parser String
text = manyTill anyChar (() <$ endOfLine <|> eof)
