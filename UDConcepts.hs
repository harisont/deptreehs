{-|
Module      : UDConcepts
Description : Basic Universal Dependencies concepts
License     : BSD-2
Maintainer  : arianna.masciolini@gu.se
Stability   : experimental
Portability : POSIX

Basic Universal Dependencies concepts, as defined in the CoNNL-U format 
specification at [universaldependencies.org](universaldependencies.org).
-}

module UDConcepts where

import Data.List
import Data.List.Split
import Data.Char

import RTree
import UDStandard
import Utils

-- * UD objects
-- $ 
-- UD sentences, word tokens, token IDs and other CoNNL-U fields are all 
-- UD objects than can be printed, parsed and validated

-- | Typeclass for all UD objects 
class UDObject a where
  -- | Print in CoNLL-U format
  prt  :: a -> String
  
  -- | Parse from CoNLL-U
  prs  :: String -> a 
  prs s = prss [s]
  
  -- | Parse from multiple separate (CoNLL-U) lines
  prss :: [String] -> a
  prss ss = prs (unlines ss)

  -- | Return a list of CoNLL-U format errors
  errors :: a -> [String]
  errors _ = []

  -- | Check and return either the checked object or a list of format errors
  check :: a -> Either a [String] 
  check x = case errors x of 
    [] -> Left x ; 
    ss -> Right (("ERROR in " ++ prt x ++ ": ") : ss)

-- | Instance for all pipe-separated lists 
-- (e.g. FEATS field of a CoNNL-U file)
instance UDObject d => UDObject [d] where
  prt ds = case ds of
    [] -> "_" 
    _ -> concat (intersperse "|" (map prt ds))
  prs s = case (strip s) of
    "_" -> []
    _ -> map (prs . strip) (getSeps '|' s)
  errors ds = concatMap errors ds

-- * CoNNL-U fields
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
   deriving (Eq,Ord,Show)

instance UDObject UDId where
  prt i = case i of
    UDIdInt n -> show n
    UDIdRange m n -> show m ++ "-" ++ show n
    UDIdEmpty f -> show f
    UDIdNone -> "_"
  prs s = case (strip s) of
    "0" -> rootID
    "_" -> UDIdNone
    _ | all isDigit s -> UDIdInt (read s)
    _ -> case break (flip elem ".-") s of
      (a,'-':b@(_:_)) | all isDigit (a++b) -> UDIdRange (read a) (read b)
      (a,'.':b@(_:_)) | all isDigit (a++b) -> UDIdEmpty (read s)
      _ -> error ("ERROR:" ++ s ++ " invalid UDId")

-- | Convert an integer into an `UDId`
int2id :: Int -> UDId
int2id n = case n of
  0 -> rootID
  _ -> UDIdInt n

-- | Convert a `UDId` into an integer. Calling this function on a range ID,
-- such as 3-5, returns the lower bound (3). Missing IDs (_) are mapped to 0
id2int :: UDId -> Int
id2int i = case i of
  UDIdRange m n -> m
  UDIdInt n -> n
  _ -> 0 --- Root, None, Float

-- | The ID of the root of a UD tree is always 0
rootID :: UDId
rootID = UDIdInt 0

-- ** UPOS

-- | Universal POS tag (fourth column of a CoNNL-U file)
type UPOS = String

checkUPOS :: String -> [String]
checkUPOS = checkInList "UD Pos tag" (map fst allUDPos)

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
   deriving (Eq,Show,Ord)

instance UDObject UDData where
  prt d = udArg d ++ "=" ++ concat (intersperse "," (udVals d))
  prs s = case break (=='=') (strip s) of
    (a,_:vs@(_:_)) -> UDData a (getSepsEsc ',' vs) 
    (a,_) -> UDData a []

-- ** DEPREL

-- | Dependency label (eight column of a CoNNL-U file)
type Label = String

checkLabel :: String -> [String]
checkLabel s = let (t,a) = break (==':') s in
  checkInList "UD label" (map fst allUDLabels) t

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
  } deriving (Show,Eq,Ord)

instance Read UDWord where
  readsPrec _ s = [(prs s :: UDWord, "")]

instance UDObject UDWord where
  prt w = intercalate "\t" $ prUDWordParts w
  prs s = case getSeps '\t' (strip s) of
    id:fo:le:up:xp:fe:he:de:ds:mi:_ ->
      UDWord 
        (prs $ strip id) 
        fo 
        le 
        up 
        xp 
        (prs $ strip fe) 
        (prs $ strip he) 
        de 
        ds 
        (prs $ strip mi) 
    _ -> error ("ERROR: " ++ s ++ " incomplete UDWord")
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
          -> ["root iff 0 does not hold in:",prt w]
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

-- * CoNNL-U sentence representations
-- $
-- CoNNL-U sentences can be represented as sequences ('UDSentence's) or trees
-- ('UDTree's)

-- ** Sentences

-- | UD sentence as sequence of 'UDWord's
data UDSentence = UDSentence {
  udCommentLines :: [String], -- ^ comment lines
  udWordLines    :: [UDWord]  -- ^ word tokens
  }

instance UDObject UDSentence where
  prt s = prtReducedUDSentence "xxxxxxxxxx" s
  prss ss = case span ((=="#") . take 1) ss of
    (cs,ws) -> UDSentence cs (map (prs . strip) ws)
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
prsReducedUDSentence :: PrintPattern -> [String] -> UDSentence
prsReducedUDSentence parts givens = UDSentence {
  udCommentLines = cs,
  udWordLines = map ((completeReducedUDWord parts) . words) ws 
  }
 where 
  (cs,ws) = break ((/="#") . take 1) givens
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
prsSimplifiedUDSentence :: String -> UDSentence
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

-- ** Trees

-- | Tree represenation of a UD sentence. A 'UDTree' is a 'RTree' whose nodes
-- are 'UDWord's
type UDTree = RTree UDWord

-- | Print 'UDTree' as nested 'UDWord's
prtUDTree :: UDTree -> String
prtUDTree = prIndented prt

-- | "Linearize" a 'UDTree' into a one-line sentence
prtUDTreeLin :: UDTree -> String
prtUDTreeLin t = unwords [udFORM n | n <- sortOn udID (allNodes t)]

-- | Convert a UD subtree into a complete tree that can be converted into 
-- valid CoNNL-U by rescaling IDs to be in the range 1-n and creating a root
-- node labelled as such  
subtree2tree :: UDTree -> UDTree
subtree2tree = sentence2tree . rescaleIds . tree2sentence . createRoot
  where 
    createRoot tree = tree {
      root = (root tree) {
        udDEPREL = rootLabel, 
        udHEAD = rootID, 
        udMISC = 
          UDData "ORIG_LABEL" [udDEPREL (root tree)]:udMISC (root tree)}}
    rescaleIds uds =
      if ids == [1..length ws]
      then uds
      else uds{udWordLines = map fix ws}
      where
       ws = udWordLines uds
       ids = [n | UDIdInt n <- map udID ws]
       fixes = zip (map udID ws) (map UDIdInt [1..length ws])
       fix udw = udw {
         udID = let idw = udID udw in maybe idw id (lookup idw fixes),
         udHEAD = let idw = udHEAD udw in maybe idw id (lookup idw fixes),
         udMISC = UDData "ADJUSTED" ["True"] : udMISC udw}

-- | Return 'True' if a 'UDTree' is projective, i.e. if it does not have
-- any crossing arcs, 'False' otherwise
isProjective :: UDTree -> Bool
isProjective udt = length nodes - 1 == maxId - minId
 where
   nodes = map (id2int . udID) (allNodes udt)
   maxId = maximum nodes
   minId = minimum nodes

-- ** Conversions between sentences and trees

-- | Convert a 'UDSentence' into a 'UDTree'
sentence2tree :: UDSentence -> UDTree
sentence2tree s = s2t rootWord where
  s2t hd = RTree hd [s2t w | w <- ws, udHEAD w == udID hd]
  rootWord =
    case [w | w <- ws, udHEAD w == rootID] of -- unique if check succeeds
      x:_ -> x
      []  -> error $ 
              "sentence2tree: root word expected to have " 
              ++ show rootID ++ " as root, got instead\n" 
              ++ (prt (UDSentence [] ws))
  ws = udWordLines s

-- | Convert a 'UDTree' into a 'UDSentence' 
tree2sentence :: UDTree -> UDSentence
tree2sentence t = UDSentence {
  udCommentLines = [],
  udWordLines = sortOn udID (allNodes t)
  }

-- | * Parsing full CoNNL-U files

-- | Parse a CoNNL-U file as a list of 'UDSentence's
prsUDFile :: FilePath -> IO [UDSentence]
prsUDFile f = readFile f >>= return . prsUDText

-- | Parse a CoNNL-U string as a list of 'UDSentence's
prsUDText :: String -> [UDSentence]
prsUDText = map prss . stanzas . filter (not . isGeneralComment) . lines
  where 
    isGeneralComment line = "##" `isPrefixOf` line
    stanzas ls = case dropWhile (all isSpace) ls of
      []  -> []
      wls -> case break (all isSpace) wls of
        (s,ss) -> s : stanzas ss