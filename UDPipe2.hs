{-|
Module      : UDPipe2
Description : Automatic UD annotation via the UDPipe2 REST API.
License     : BSD-2
Maintainer  : arianna.masciolini@gu.se
Stability   : experimental
Portability : POSIX

Automatic UD annotation via the 
[UDPipe2 REST API](https://lindat.mff.cuni.cz/services/udpipe/api-reference.php).
-}

{-# LANGUAGE DeriveGeneric #-}

module UDPipe2 where

import GHC.Generics
import Data.Either
import Data.ByteString.Lazy.UTF8 (fromString)
import Network.Curl
import Network.URI.Encode (encode)
import Data.Aeson hiding (encode)
import UDStandard

-- | UDPipe model name, such as "swedish-talbanken-ud-2.12-230717" (see
-- [ufal.mff.cuni.cz/udpipe/2/models](ufal.mff.cuni.cz/udpipe/2/models)
-- for a complete list of models)
type Model = String

-- $ 
-- Tokenizer, tagger/lemmatizer and parser options (for more information, see 
-- [ufal.mff.cuni.cz/udpipe/1/users-manual](https://ufal.mff.cuni.cz/udpipe/1/users-manual))
type TokOpts = String
type TagOpts = String
type PrsOpts = String

-- | Supported input formats
data InputFormat = Plain | CoNLLU | GenericTokenizer | Horizontal | Vertical
  deriving Eq

instance Show InputFormat where
  show Plain = "" -- not used
  show CoNLLU = "conllu"
  show GenericTokenizer = "generic_tokenizer"
  show Horizontal = "horizontal"
  show Vertical = "vertical"

-- | UDPipe 2 API respone
data UDPipe2Response = UDPipe2Response {
  model :: Model,
  acknowledgements :: [String],
  result :: String
} deriving (Generic, Show)

instance ToJSON UDPipe2Response where
instance FromJSON UDPipe2Response where

-- | Annotate text in any input format supported by the UDPipe2 API, returning
-- `UDSentence`s
annotateText :: 
  String            -- ^ string to be annotated
  -> InputFormat    -- ^ input format
  -> Model          -- ^ UDPipe2 model
  -> (Bool,TokOpts) -- ^ whether the input needs tokenization + options
  -> (Bool,TagOpts) -- ^ whether the input needs POS tagging, lemmatization
                    -- and morpho analysis + options
  -> (Bool,PrsOpts) -- ^ whether the input needs parsing + options
  -> IO [UDSentence]
annotateText s f m (tk,tko) (tg,tgo) (pr,pro) = do
  (code,str) <- curlGetString
    "http://lindat.mff.cuni.cz/services/udpipe/api/process"
    [CurlPost True, CurlPostFields fields]
  if code /= CurlOK
    then error $ "Failed to use the UDPipe 2 API. curl code: " ++ show code
    else do
      case decode (fromString str) :: Maybe UDPipe2Response of
        Nothing -> 
          error $ "Got a malformed response string from UDPipe 2: " ++ str
        Just json -> do
          let prsRes = prsUDText (result json)
          if isLeft prsRes -- it better be, cause it comes from a parser!
            then return $ fromLeft [] prsRes 
            else do 
              mapM_ putStrLn (fromRight [] prsRes) 
              return []
  where
    fields = ["data=" ++ encode s, "model=" ++ m, "input=" ++ show f]
          ++ (if tk || f == Plain then ["tokenizer=" ++ tko] else [])
          ++ (if tg then ["tagger=" ++ tgo] else [])
          ++ (if pr then ["parser=" ++ pro] else [])

-- | Shorthand to annotate plain text from scratch
annotatePlainText :: String -> Model -> IO [UDSentence]
annotatePlainText s m = annotateText s Plain m (True,"") (True,"") (True,"")

-- | Shorthand to annotate files
annotateFile :: 
  FilePath          -- ^ path to the file to be annotated 
  -> InputFormat    -- ^ input format
  -> Model          -- ^ UDPipe2 model
  -> (Bool,TokOpts) -- ^ whether the input needs tokenization + options
  -> (Bool,TagOpts) -- ^ whether the input needs POS tagging, lemmatization
                    -- and morpho analysis + options
  -> (Bool,PrsOpts) -- ^ whether the input needs parsing + options
  -> IO [UDSentence]
annotateFile p f m (tk,tko) (tg,tgo) (pr,pro) = do
  s <- readFile p 
  annotateText s f m (tk,tko) (tg,tgo) (pr,pro)
  
-- | Shorthand to annotate plain text files from scratch
annotatePlainFile :: FilePath -> Model -> IO [UDSentence]
annotatePlainFile p m = annotateFile p Plain m (True,"") (True,"") (True,"")