{-|
Module      : UDVisualizations
Description : UD tree visualizations
License     : BSD-2
Maintainer  : arianna.masciolini@gu.se
Stability   : experimental
Portability : POSIX

Functions to generate LaTeX and SVG UD tree visualizations
-}

module UDVisualizations where

import Prelude hiding ((<>)) -- GHC 8.4.1 clash with Text.PrettyPrint
import qualified Data.Map as Map
import Data.List (intersperse,nub,mapAccumL,find,groupBy,sortBy,partition)
import Data.Ord (comparing)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, fromJust, isJust, catMaybes)
import Data.List.Split
import Text.PrettyPrint
import System.Environment (getArgs)

import UDStandard

-- * Generating LaTeX visualizations

-- | Generate standalone LaTeX document for the given treebank
sentences2latexDoc :: [UDSentence] -> String
sentences2latexDoc =
  render .
  latexDoc .
  vcat .
  intersperse (text "" $+$ app "vspace" (text "4mm")) .
  map (ppLaTeX . sentence2latex) 

-- | Generate LaTeX fragment for the given sentence
sentence2latexFragment :: UDSentence -> String
sentence2latexFragment = render . ppLaTeX . sentence2latex

-- * Generating SVG visualizations

-- | Generate standalone HTML document with embedded SVG visualizations
-- for the given treebank
sentences2htmlDoc :: [UDSentence] -> String
sentences2htmlDoc =
  render .
  embedInHTML .
  map (ppSVG . latex2svg . sentence2latex)

-- | Generate SVG code for the given sentence
sentence2svgFragment :: UDSentence -> String
sentence2svgFragment = render . ppSVG . latex2svg . sentence2latex

-- | Internal representations and conversions
sentence2latex :: UDSentence -> [LaTeX]
sentence2latex = visual2latex . sentence2visual

-- * Internal representations and conversion functions

-- | Intermediate data type to store sentence info to be visualized
data Visual = Visual {
    wordLength :: Int -> Double           -- ^ length of n-th word
  , tokens :: [(String,(String,String))]  -- ^ list of minimal token info in 
                                          -- (FORM,(UPOS,DEPREL)) format
  , deps :: [((Int,Int),String)]          -- ^ list of dependencies in
                                          -- ((dependent,head),label) format 
  , root :: Int                           -- position of the root token
  }

-- ** DSL for LaTeX (limited to comments and pictures)
data LaTeX = Comment String | Picture UnitLengthMM Size [DrawingCommand]
data DrawingCommand = Put Position Object
data Object = Text String | TinyText String | OvalTop Size | ArrowDown Length
type UnitLengthMM = Double
type Size = (Double,Double)
type Position = (Double,Double)
type Length = Double 

-- ** DSL for SVG
data SVG = CharData String | Elem TagName Attrs [SVG]
type TagName = String
type Attrs = [(String,String)]

-- ** Conversions

-- | Convert a 'UDSentence' into a 'Visual'
sentence2visual :: UDSentence -> Visual
sentence2visual s = Visual {
    wordLength = wld 
  , tokens = ts
  , deps = ds
  , root = head $ [id2pos (udID t) | t <- wls, udDEPREL t == rootLabel]}
 where
  wls = udWordLines s
  wld i = maximum (0:[charWidth * fromIntegral (length w) | 
                       w <- let (f,(p,d)) = ts !! i in [f,p]])
  ts = [(udFORM t,(udUPOS t,udDEPREL t)) | t <- wls]
  ds = [((id2pos $ udHEAD t, id2pos $ udID t), udDEPREL t) | 
          t <- wls, udDEPREL t /= rootLabel]
  id2pos id = max 0 (id2int id - 1)
  charWidth = 1.8

-- | Convert a into a 'Visual' list of LaTeX commands and comments
visual2latex :: Visual -> [LaTeX]
visual2latex d =
  [Comment (unwords (map fst (tokens d))),
   Picture defaultUnit (width,height) (
    [Put (wpos rwld i,0) (Text w) | (i,w) <- zip [0..] (map fst (tokens d))]   -- words
  ++ [Put (wpos rwld i,15) (TinyText w) | (i,(w,_)) <- zip [0..] (map snd (tokens d))]   -- pos tags 15u above bottom
---  ++ [Put (wpos rwld i,-15) (TinyText w) | (i,(_,w)) <- zip [0..] (map snd (tokens d))]   -- features 15u below bottom -> DON'T SHOW
  ++ concat leftDeps -- arcs and labels
  ++ [Put (wpos rwld r + 15,height) (ArrowDown (height-arcbase))]
  ++ [Put (wpos rwld r + 20,height - 10) (TinyText "root")]
  ++ concat rightDeps
  )]
 where
  r = root d
  -- split into deps at the left & right of the root so that deps are in the same order as words and pos
  (leftDeps,rightDeps) = splitAt r [putArc rwld (aheight x y) x y label | ((x,y),label) <- deps d]
  wld = wordLength d  -- >= 20.0
  rwld i = wld i / defaultWordLength       -- >= 1.0
  aheight x y = depth (min x y) (max x y) + 1    ---- abs (x-y)
  arcs = [(min u v, max u v) | ((u,v),_) <- deps d]
  depth x y = case [(u,v) | (u,v) <- arcs, (x < u && v <= y) || (x == u && v < y)] of ---- only projective arcs counted
    [] -> 0
    uvs -> 1 + maximum (0:[depth u v | (u,v) <- uvs])
  width = {-round-} (sum [wsize rwld w | (w,_) <- zip [0..] (tokens d)]) + spaceLength * fromIntegral (length (tokens d) - 1 - 1)
  height = 50 + 20 * {-round-} (maximum (0:[aheight x y | ((x,y),_) <- deps d]))
  
  -- general measures
  defaultWordLength = 20.0  -- the default fixed width word length, making word 100 units
  defaultUnit       = 0.2   -- unit in latex pictures, 0.2 millimetres
  spaceLength       = 10.0
  labellength l  = fromIntegral (length l) * 4.5  -- assuming each char is 4.5 units wide
  wsize rwld  w  = 100 * rwld w + spaceLength                   -- word length, units
  wpos rwld i    = sum [wsize rwld j | j <- [0..i-1]]           -- start position of the i'th word
  arcbase        = 30.0               -- arcs start and end 40u above the bottom
  wdist rwld x y = sum [wsize rwld i | i <- [min x y .. max x y - 1]]    -- distance between words x and y
  labelheight h  = h + arcbase + 3    -- label just above arc; 25 would put it just below
  labelstart c l = c - (labellength l)/2 -- label starts half of its length left of arc centre
  arcfactor r    = r * 600            -- reduction of arc size from word distance
  xyratio        = 3                  -- width/height ratio of arcs

  putArc :: (Int -> Double) -> Int -> Int -> Int -> String -> [DrawingCommand]
  putArc frwld height x y label = [oval,arrowhead,labelling] where
    oval = Put (ctr,arcbase) (OvalTop (wdth,hght))
    arrowhead = Put (endp,arcbase + 5) (ArrowDown 5)   -- downgoing arrow 5u above the arc base
    labelling = Put (labelstart ctr label,labelheight (hght/2)) (TinyText label)
    dxy  = wdist frwld x y             -- distance between words, >>= 20.0
    ndxy = 100 * rwld * fromIntegral height  -- distance that is indep of word length
    hdxy = dxy / 2                     -- half the distance
    wdth = dxy - (arcfactor rwld)/dxy  -- longer arcs are wider in proportion
    hght = ndxy / (xyratio * rwld)      -- arc height is independent of word length
    begp = min x y                     -- begin position of oval
    ctr  = wpos frwld begp + hdxy + (if x < y then 20 else  10)  -- LR arcs are farther right from center of oval
    endp = (if x < y then (+) else (-)) ctr (wdth/2)            -- the point of the arrow
    rwld = 0.5 ----

-- | Convert a list of 'LaTeX' commands to SVG
latex2svg :: [LaTeX] -> [SVG]
latex2svg = concatMap toSVG1
  where
    toSVG1 el =
      case el of
        Comment s -> []
        Picture unit size@(w,h) cmds ->
          [Elem "svg" ["width".=x1,"height".=y0+5,
                       ("viewBox",unwords (map show [0,0,x1,y0+5])),
                       ("version","1.1"),
                       ("xmlns","http://www.w3.org/2000/svg")]
                       (concatMap draw cmds)]
          where
            draw (Put pos obj) = objectSVG pos obj

            objectSVG pos obj =
              case obj of
                Text s -> [text 16 pos s]
                TinyText s -> [text 10 pos s]
                OvalTop size -> [ovalTop pos size]
                ArrowDown len -> arrowDown pos len

            text h (x,y) s =
              Elem "text" ["x".=xc x,"y".=yc y-2,"font-size".=h]
                          [CharData s]

            ovalTop (x,y) (w,h) =
              Elem "path" [("d",path),("stroke","black"),("fill","none")] []
              where
                x1 = x-w/2
                x2 = min x (x1+r)
                x3 = max x (x4-r)
                x4 = x+w/2
                y1 = y
                y2 = y+r
                r = h/2
                sx = show . xc
                sy = show . yc
                path = unwords ["M",sx x1,sy y1,"Q",sx x1,sy y2,sx x2,sy y2,
                                 "L",sx x3,sy y2,"Q",sx x4,sy y2,sx x4,sy y1]
            arrowDown (x,y) len =
                [Elem "line" ["x1".=xc x,"y1".=yc y,"x2".=xc x,"y2".=y2,
                              ("stroke","black")] [],
                 Elem "path" [("d",unwords arrowhead)] []]
               where
                 x2 = xc x
                 y2 = yc (y-len)
                 arrowhead = "M":map show [x2,y2,x2-3,y2-6,x2+3,y2-6]

            xc x = num x+5
            yc y = y0-num y
            x1 = num w+10
            y0 = num h+20
            num x = round (scale*x)
            scale = unit*5
            
            infix 0 .=
            n.=v = (n,show v)

-- ** LaTeX utils

-- | "Pretty pring" a list of 'LaTeX' commands/comments, returning a 'Doc'
ppLaTeX :: [LaTeX] -> Doc 
ppLaTeX = vcat . map ppLaTeX1
  where
    ppLaTeX1 el =
      case el of
        Comment s -> comment s
        Picture unit size cmds ->
          app "setlength{\\unitlength}" (text (show unit ++ "mm"))
          $$ hang (app "begin" (text "picture")<>text (show size)) 2
                  (vcat (map ppDrawingCommand cmds))
          $$ app "end" (text "picture")
          $$ text ""
    ppDrawingCommand (Put pos obj) = put pos (ppObject obj)
    ppObject obj =
      case obj of
        Text s -> text s
        TinyText s -> small (text s)
        OvalTop size -> text "\\oval" <> text (show size) <> text "[t]"
        ArrowDown len -> app "vector(0,-1)" (text (show len))
    put p@(_,_) = app ("put" ++ show p)
    small w = text "{\\tiny" <+> w <> text "}"
    comment s = text "%%" <+> text s -- line break show follow

-- | Apply LaTeX macro to a 'Doc'
app :: 
  String -- ^ macro name 
  -> Doc -- ^ argument of the macro
  -> Doc -- ^ resulting 'Doc'
app macro arg = text "\\" <> text macro <> text "{" <> arg <> text "}"

-- | Convert a LaTeX fragment into a complete, standalone LaTeX document
latexDoc :: Doc -> Doc
latexDoc body =
  vcat [text "\\documentclass{article}",
        text "\\usepackage[a4paper,margin=0.5in,landscape]{geometry}",
        text "\\usepackage[utf8]{inputenc}",
        text "\\begin{document}",
        body,
        text "\\end{document}"]

-- ** SVG utils

-- | "Pretty pring" a list of 'SVG' elements, returning a 'Doc'
ppSVG :: [SVG] -> Doc 
ppSVG svg =
  vcat (map ppSVG1 svg) -- It should be a single <svg> element...
  where
    ppSVG1 svg1 =
      case svg1 of
        CharData s -> text (encode s)
        Elem tag attrs [] ->
            text "<"<>text tag<>cat (map attr attrs) <> text "/>"
        Elem tag attrs svg ->
            cat [text "<"<>text tag<>cat (map attr attrs) <> text ">",
                 nest 2 (cat (map ppSVG1 svg)),
                 text "</"<>text tag<>text ">"]

    attr (n,v) = text " "<>text n<>text "=\""<>text (encode v)<>text "\""

    encode = foldr encodeEntity ""

    encodeEntity = encodeEntity' (const False)
    encodeEntity' esc c r =
      case c of
        '&' -> "&amp;"++r
        '<' -> "&lt;"++r
        '>' -> "&gt;"++r
        _ -> c:r

-- | Embed SVG fragments in an HTML document
embedInHTML :: [Doc] -> Doc
embedInHTML svgs =
  vcat
     [
      text "<!DOCTYPE html>",
      text "<html>",
      text "<body>",
      vcat svgs,
      text "</body>",
      text "</html>"
     ] 
