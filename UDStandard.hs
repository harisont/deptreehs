{-|
Module      : UDStandard
Description : Utilities for reading, writing and validating of UD treebanks
License     : BSD-2
Maintainer  : arianna.masciolini@gu.se
Stability   : experimental
Portability : POSIX

Utilities for reading, writing and validating UD treebanks in CoNNL-U format, 
as specified at [universaldependencies.org](universaldependencies.org))
-}

module UDStandard where

import Data.List
import Data.List.Split
import Data.Char
import Data.Either

import Utils

-- * UD objects
-- $ 
-- UD sentences, word tokens, token IDs and other CoNNL-U fields are all 
-- UD objects than can be printed, parsed and validated

type ErrorMsg = String

-- | Typeclass for all UD objects 
class UDObject a where
  -- | Print in CoNLL-U format
  prt  :: a -> String
  
  -- | Parse from CoNLL-U. Return either the parsed object or a list of parse 
  -- errors
  prs  :: String -> Either a [ErrorMsg] 
  prs s = prss [s]
  
  -- | Parse from multiple separate (CoNLL-U) lines
  prss :: [String] -> Either a [ErrorMsg]
  prss ss = prs (unlines ss)

  -- | Return a list of CoNLL-U format errors coming from additional (i.e. 
  -- post-parse errors, such as "invalid UPOS tag")
  errors :: a -> [ErrorMsg]
  errors _ = []

-- | Instance for all pipe-separated lists 
-- (e.g. FEATS field of a CoNNL-U file)
instance UDObject d => UDObject [d] where
  prt ds = case ds of
    [] -> "_" 
    _ -> concat (intersperse "|" (map prt ds))
  prs s = case (strip s) of
    "_" -> Left []
    _ -> 
      let xs = map (prs . strip) (getSeps '|' s)
      in if all isLeft xs then Left $ lefts xs else Right $ concat $ rights xs
  errors ds = concatMap errors ds

-- * CoNNL-U fields

type Field = String

-- | List of standard CoNNL-U field names
allFieldNames :: [Field]
allFieldNames = [
  "ID", 
  "FORM", 
  "LEMMA", 
  "UPOS", 
  "XPOS", 
  "FEATS", 
  "HEAD", 
  "DEPREL",
  "DEPS",
  "MISC"]

-- $
-- Some CoNNL-U fields have dedicated data types, while others, such as FORM
-- and LEMMA, are simply treated as strings

-- ** ID

-- | Token ID (corresponds to the first column of a CoNNL-U file)
data UDId =
    UDIdInt Int         -- ^ normal case: the ID is an integer
  | UDIdRange Int Int   -- ^ multiword (e.g. "don't"): the ID is a range X-Y
  | UDIdEmpty Float     -- ^ empty node: the ID is a float > 0  
  | UDIdNone            -- ^ missing ID (_)
   deriving (Eq,Ord,Read,Show)

instance UDObject UDId where
  prt i = case i of
    UDIdInt n -> show n
    UDIdRange m n -> show m ++ "-" ++ show n
    UDIdEmpty f -> show f
    UDIdNone -> "_"
  prs s = case (strip s) of
    "0" -> Left $ rootID
    "_" -> Left $ UDIdNone
    _ | all isDigit s -> Left $ UDIdInt (read s)
    _ -> case break (flip elem ".-") s of
      (a,'-':b@(_:_)) | all isDigit (a++b) -> Left $ UDIdRange (read a) (read b)
      (a,'.':b@(_:_)) | all isDigit (a++b) -> Left $ UDIdEmpty (read s)
      _ -> Right $ ["ERROR: invalid ID in\n\n" ++ s]

-- | Convert an integer into an `UDId`
int2id :: Int -> UDId
int2id n = case n of
  0 -> rootID
  _ -> UDIdInt n

-- | Convert a `UDId` into an integer. Calling this function on a range ID,
-- such as 3-5, returns the lower bound (3). 
-- Other non-int IDs (_) are mapped to 0
id2int :: UDId -> Int
id2int i = case i of
  UDIdRange m n -> m
  UDIdInt n -> n
  _ -> 0

-- | The ID of the root of a UD tree is always 0
rootID :: UDId
rootID = UDIdInt 0

-- ** UPOS

-- | Universal POS tag (fourth column of a CoNNL-U file)
type UPOS = String

-- | Only for backwards compatibility
type POS = UPOS

checkUPOS :: String -> [String]
checkUPOS = checkInList "UPOS tag" (map fst allUPOSs)

-- | Complete list of UPOS tags paired with explanations. Used for validation
allUPOSs :: [(String,String)]
allUPOSs = [
  ("ADJ", "adjective"),
  ("ADP", "adposition"),
  ("ADV", "adverb"),
  ("AUX", "auxiliary"),
  ("CCONJ", "coordinating conjunction"),
  ("DET", "determiner"),
  ("INTJ", "interjection"),
  ("NOUN", "noun"),
  ("NUM", "numeral"),
  ("PART", "particle"),
  ("PRON", "pronoun"),
  ("PROPN", "proper noun"),
  ("PUNCT", "punctuation"),
  ("SCONJ", "subordinating conjunction"),
  ("SYM", "symbol"),
  ("VERB", "verb"),
  ("X", "other")
  ]

-- ** XPOS
-- | Language-specific POS tag (fifth column of a CoNNL-U file)
type XPOS = String

-- ** FEATS and MISC

-- | Representation of the key-values pairs used in the 'FEATS' and 'MISC' 
-- fields with the (syntax: @Arg1=Val1,Val2,Val3@) (sixth column of a CoNNL-U
-- file)
data UDData = UDData {
  udArg  :: String, -- ^ argument name
  udVals ::[String] -- ^ values
  }
   deriving (Eq,Read,Show,Ord)

instance UDObject UDData where
  prt d = udArg d ++ "=" ++ concat (intersperse "," (udVals d))
  prs s = case break (=='=') (strip s) of
    (a,_:vs@(_:_)) -> Left $ UDData a (getSepsEsc ',' vs) 
    (a,_) -> Left $ UDData a []

-- ** DEPREL

-- | Dependency label (eight column of a CoNNL-U file)
type Label = String

-- | Complete list of dependency labels (without subtypes) 
-- paired with explanations. Used for validation
allLabels :: [(Label,String)]
allLabels = [
  ("acl", "clausal modifier of noun (adjectival clause)"),
  ("advcl", "adverbial clause modifier"),
  ("advmod", "adverbial modifier"),
  ("amod", "adjectival modifier"),
  ("appos", "appositional modifier"),
  ("aux", "auxiliary"),
  ("case", "case marking"),
  ("cc", "coordinating conjunction"),
  ("ccomp", "clausal complement"),
  ("clf", "classifier"),
  ("compound", "compound"),
  ("conj", "conjunct"),
  ("cop", "copula"),
  ("csubj", "clausal subject"),
  ("dep", "unspecified dependency"),
  ("det", "determiner"),
  ("discourse", "discourse element"),
  ("dislocated", "dislocated elements"),
  ("expl", "expletive"),
  ("fixed", "fixed multiword expression"),
  ("flat", "flat multiword expression"),
  ("goeswith", "goes with"),
  ("iobj", "indirect object"),
  ("list", "list"),
  ("mark", "marker"),
  ("nmod", "nominal modifier"),
  ("nsubj", "nominal subject"),
  ("nummod", "numeric modifier"),
  ("obj", "object"),
  ("obl", "oblique nominal"),
  ("orphan", "orphan"),
  ("parataxis", "parataxis"),
  ("punct", "punctuation"),
  ("reparandum", "overridden disfluency"),
  ("root", "root"),
  ("vocative", "vocative"),
  ("xcomp", "open clausal complement")
  ]

checkLabel :: String -> [String]
checkLabel s = let (t,a) = break (==':') s in
  checkInList "UD label" (map fst allLabels) t

-- | Dummy label
depLabel = "dep"

-- | "head"
headLabel = "head"

-- | "root"
rootLabel = "root"

-- * Word tokens

-- | UD word token, corresponding to a tab-separated text line describing a
-- token in a CoNNL-U file
data UDWord = UDWord {
  udID     :: UDId,     -- ^ ID (word position, counting form 1)
  udFORM   :: String,   -- ^ surface word form
  udLEMMA  :: String,   -- ^ lemma
  udUPOS   :: UPOS,     -- ^ Universal Part Of Speech tag
  udXPOS   :: XPOS,     -- ^ language-specific POS tag
  udFEATS  :: [UDData], -- ^ morphological features
  udHEAD   :: UDId,     -- ^ ID of the syntactic head of the token
  udDEPREL :: Label,    -- ^ dependency label
  udDEPS   :: String,   -- ^ enhanced dependency graph
  udMISC   :: [UDData]  -- ^ any other annotation
  } deriving (Read,Show,Eq,Ord)

-- TODO:
--instance Read UDWord where
--  readsPrec _ s = [(prs s :: UDWord, "")]

instance UDObject UDWord where
  prt w = intercalate "\t" $ prUDWordParts w
  -- the code below is obnoxious but idk how to do it in a better way (see 
  -- comments)
  prs s = case getSeps '\t' (strip s) of
    id:fo:le:up:xp:fe:he:de:ds:mi:_ ->
      let 
        -- can't map prs . strip because that would try to parse all as UDId
        uid = prs $ strip id :: Either UDId [ErrorMsg]
        ufe = prs $ strip fe :: Either [UDData] [ErrorMsg]
        uhe = prs $ strip he :: Either UDId [ErrorMsg]
        umi = prs $ strip mi :: Either [UDData] [ErrorMsg]
      -- can't do all isLeft (lefts xs) because types are now different
      in if and [isLeft uid, isLeft ufe, isLeft uhe, isLeft umi]  
          then 
            let w = UDWord 
                        (fromLeft UDIdNone uid)
                        fo 
                        le 
                        up 
                        xp 
                        (fromLeft [] ufe) 
                        (fromLeft UDIdNone uhe) 
                        de 
                        ds 
                        (fromLeft [] umi) 
            in case errors w of 
              [] -> Left w
              es -> Right es
          -- again can't map (fromRight []) because the Eithers have different
          -- left-types
          else Right $ 
                fromRight [] uid 
             ++ fromRight [] ufe 
             ++ fromRight [] uhe 
             ++ fromRight [] umi
    _ -> Right ["ERROR: incomplete line in\n\n" ++ s]
  errors w@(UDWord id fo le up xp fe he de ds mi) =
    concat [
      errors id, 
      checkUPOS up,
      errors fe, 
      errors he, 
      checkLabel de, 
      errors mi] ++ case w of
      _ | not   ((udHEAD w /= rootID || udDEPREL w == "root") 
             && (udHEAD w == rootID || udDEPREL w /= "root"))
          -> ["ERROR: DEPREL==root iff ID==0 does not hold in:\n\n",prt w]
      _ -> []

-- | Shorthand to create an "empty" UDWord with an integer ID
initUDWord :: Int -> UDWord
initUDWord i = UDWord (UDIdInt i) "" "" "" "" [] UDIdNone "" "" []

-- | Print UDWord as a list of CoNLL-U fields, e.g.
-- @["3","much","much","ADJ","JJ","Degree=Pos","9","nsubj","9:nsubj","_"]@
prUDWordParts :: UDWord -> [String]
prUDWordParts (UDWord id fo le up xp fe he de ds mi) =
    [prt id,fo,le,up,xp,prt fe,prt he,de,ds,prt mi]

-- | Distance between a word and its syntectic head
dependencyDistance :: UDWord -> Int
dependencyDistance w = abs (id2int (udID w) - id2int (udHEAD w))

-- * CoNNL-U sentences

-- | UD sentence as a sequence of 'UDWord's (+ comments). 
-- For conversions to and from a tree representation, see 'UDTree'
data UDSentence = UDSentence {
  udCommentLines :: [String], -- ^ comment lines
  udWordLines    :: [UDWord]  -- ^ word tokens
  }

instance Show UDSentence where
  show = prt

instance UDObject UDSentence where
  prt s = prtReducedUDSentence "xxxxxxxxxx" s
  prss ss = case span ((=="#") . take 1) ss of
    (cs,ws) -> 
      let uws = map (prs . strip) ws
      in if all isLeft uws then 
        let s = UDSentence cs (lefts uws)
        in case errors s of 
          [] -> Left s
          es -> Right es
        else Right $ concat $ rights uws
  errors s = checkUDWords (udWordLines s)
    where 
      checkUDWords ws = concatMap errors ws ++ case ws of
        _ | length (filter ((==rootID) . udHEAD) ws) /= 1
              -> ["no unique root in:", pws]
        _ | ids /= [1 .. length ids]
              -> ["word id sequence not 1..n in " ++ pws]
        _ | not (null [i | UDIdInt i <- map udHEAD ws, i > lws || i < 0])
              -> ["head outside sentence in " ++ pws]
        _ -> []
       where
         ids = [n | UDIdInt n <- map udID ws]
         pws = unlines (map prt ws)
         lws = length ids

-- $
-- Sentences can be printed in CoNNL-U format via the 'prt' function available
-- for all instances of 'UDObject'. The functions below are used to produce  
-- strings in less standard CoNLL variants

-- | String consisting of 10 'x' and '_' characters, used to specify which
-- CoNLL-U fields are to be printed when outputting reduced CoNLL-U like 
-- format. The pattern '"xxxx\_\_xx\_\_"', for instance, is used for producing
-- the simplified CoNNL-U format (ID, FORM, LEMMA, UPOS, HEAD and DEPREL)
-- used in the Computational Syntax course of the MLT programme at the 
-- University of Gothenburg
type PrintPattern = String

-- | Print a 'UDSentence' in CoNLL-U format excluding one or more fields,
-- according to the input 'PrintPattern'
prtReducedUDSentence :: PrintPattern -> UDSentence -> String
prtReducedUDSentence parts s = 
  unlines (udCommentLines s ++ map prReducedUDWord (udWordLines s))
  where
    prReducedUDWord w = 
      intercalate "\t" [p | (p,b) <- zip (prUDWordParts w) pattern, b]
    pattern = map (/='_') parts

-- | Shorthand for producing simplified CoNNL-U without explicitly providing 
-- the corresponding 'PrintPattern' (equivalent to 
-- 'prtReducedUDSentence '"xxxx\_\_xx\_\_"')
prtSimplifiedUDSentence :: UDSentence -> String
prtSimplifiedUDSentence = prtReducedUDSentence "xxxx__xx__"

-- $ 
-- Parsing CoNNL-U format is done via the 'prs' function available for all
-- instances of 'UDObject'. The functions below are used to parse full files
-- and less standard CoNLL variants

-- | Parse a reduced CoNNL-U sentence. Requires a 'PrintPattern' specifying
-- what fields are omitted
prsReducedUDSentence :: PrintPattern -> [String] -> Either UDSentence [ErrorMsg]
prsReducedUDSentence parts givens = if all isLeft cws 
  then Left $ UDSentence {
                udCommentLines = cs,
                udWordLines = lefts cws
                }
  else Right $ concat $ rights cws
 where 
  (cs,ws) = break ((/="#") . take 1) givens
  cws = (map ((completeReducedUDWord parts) . words) ws)
  completeReducedUDWord parts = 
    prs . concat .  (intersperse "\t") . complete pattern
    where
      complete ps gs = case (ps,gs) of
        (True :pp, g:gg) -> g   : complete pp gg
        (False:pp, _)    -> "_" : complete pp gs
        _                -> []
      pattern = map (/='_') parts ++ replicate (10 - length parts) False

-- | Shorthand for parsing simplified CoNNL-U without explicitly providing 
-- the corresponding 'PrintPattern' (equivalent to 
-- 'prsReducedUDSentence '"xxxx\_\_xx\_\_"')
prsSimplifiedUDSentence :: String -> Either UDSentence [ErrorMsg]
prsSimplifiedUDSentence = prss . map completeUDWord . getSeps ";" . words
 where
  completeUDWord ws = case ws of
    index:word:lemma:pos:goal:label:_ -> 
      (concat 
        (intersperse "\t" [index,word,lemma,pos,dum,dum,goal,label,dum,dum]))
    _ -> error $ "no UD word from: " ++ unwords ws
  dum = "_"

-- | Return the sent_id of a sentence (if any)
sentId :: UDSentence -> Maybe String 
sentId s = if hasSentId s 
            then Just $ head $ words $ head idEtc 
            else Nothing
  where
    hasSentId s = (not . null) idEtc
    (_:idEtc) = 
        splitOn "sent_id = " (unwords (concatMap words (udCommentLines s)))

-- | Extract @form:\<pos\>@ sequences
sentence2poswords :: UDSentence -> String
sentence2poswords s = 
  unwords [udFORM u ++ ":<" ++ udUPOS u ++ ">" | u <- udWordLines s]

-- | Extract @word:\<pos_feats\>@ sequences
ud2posfeatswords :: UDSentence -> String
ud2posfeatswords s = unwords 
  [udFORM u ++ ":<" ++ udUPOS u ++ "_" ++ prt (udFEATS u) ++ ">" 
    | u <- udWordLines s]

-- * Parsing full CoNNL-U files

-- | Parse a CoNNL-U file. 
-- Returns either a list of 'UDSentence's or a list of 'ErrorMsg'
prsUDFile :: FilePath -> IO (Either [UDSentence] [ErrorMsg])
prsUDFile f = readFile f >>= return . prsUDText

-- | Parse a CoNNL-U string. 
-- Returns either a list of 'UDSentence's or a list of 'ErrorMsg'
prsUDText :: String -> Either [UDSentence] [ErrorMsg]
prsUDText text =
  let
    results = map prss . stanzas . filter (not . isGeneralComment) . lines $ text
  in
    if null $ rights results then
      Left $ lefts results
    else
      Right $ map unlines $ rights results

-- | Predicate that checks if a 'String' is a general comment, i.e., if it starts with ##
isGeneralComment :: String -> Bool
isGeneralComment line = "##" `isPrefixOf` line

-- | Splits a list of lines into a list of stanzas separated by blank lines
stanzas :: [String] -> [[String]]
stanzas ls = case dropWhile (all isSpace) ls of
  []  -> []
  wls -> case break (all isSpace) wls of
    (s,ss) -> s : stanzas ss
