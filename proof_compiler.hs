import Control.Monad (foldM) -- For Maximum Monad Fuckery
import Data.Char (toLower)

-- Types ----------------------------------------------
data ProofTree = Inference {
                     antecedents :: [ProofTree],
                     lineType    :: LineType,
                     label       :: Label,
                     consequent  :: String }
               | Axiom String

type LineType = (LineCount, LineStyle) -- Everything required to define the rendering of the line
data LineCount = NoLine       --       *Not Visible*
               | SingleLine   --       -------------
               | DoubleLine   --       =============
  deriving Show
data LineStyle = DottedLine
               | DashedLine
               | SolidLine
  deriving Show

data Label = Label {
    leftLabel  :: String,
    rightLabel :: String
}

-- Bussproofs Compiler -------------------------------------------
data BussproofsErr = NoAntecedent
                   | TooManyAntecendts
                   | UnexpectedAxiom
    deriving Show

proofTreeToBussproof :: ProofTree -> Either BussproofsErr String
proofTreeToBussproof = fmap unlines' . helper
  where
    helper (Axiom ax) = Right ["\\AxiomC{ " <> ax <> " }"]
    -- helper inf = concatM $ (inf <$?> [antHelper, labelHelper, lineHelper, infHelper])
    helper inf = concatM $ [antHelper inf, infHelper inf] -- simplified for experimentation

antHelper :: ProofTree -> Either BussproofsErr [String]
antHelper (Axiom _) = Left UnexpectedAxiom
antHelper inf = concatM . map (fmap (:[]) . proofTreeToBussproof) . antecedents $ inf

labelHelper :: ProofTree -> Either BussproofsErr [String]
labelHelper (Axiom _) = Left UnexpectedAxiom
labelHelper inf= Right ["\\LeftLabel{ " <> ll <> " }", "\\RightLabel{ " <> rl <> " }"]
  where
    ll = leftLabel . label $ inf
    rl = rightLabel . label $ inf

lineHelper :: ProofTree -> Either BussproofsErr [String]
lineHelper (Axiom _) = Left UnexpectedAxiom
lineHelper inf = Right [lineCountCode, lineStyleCode]
  where
    lineCountCode = "\\" <> (appFst toLower . show . fst . lineType $ inf)
    lineStyleCode = "\\" <> (appFst toLower . show . snd . lineType $ inf)

infType :: ProofTree -> Either BussproofsErr [String]
infType (Axiom _) = Left NoAntecedent
infType inf | antCount == 1 = Right ["\\UnaryInfC"]
            | antCount == 2 = Right ["\\BinaryInfC"]
            | antCount == 3 = Right ["\\TernaryInfC"]
            | antCount == 4 = Right ["\\QuaternaryInfC"]
            | antCount == 5 = Right ["\\QuinaryInfC"]
            | otherwise     = Left TooManyAntecendts
  where
    antCount = length $ antecedents inf

infHelper :: ProofTree -> Either BussproofsErr [String]
infHelper (Axiom _) = Left UnexpectedAxiom
infHelper inf = map (<> "{ " <> consequent inf <> " }" )  <$> infType inf

-- Helper Functions ----------------------------------
concatM :: Monad m => [ m [a] ] -> m [a]
concatM = foldM (\b -> \a -> (b <>) <$> a) []

(<$?>) :: a -> [a -> b] -> [b] -- Also known as, then AnTiMaP :o (takes a list of functions and applies then to an argument)
inp <$?> fns = ($ inp) <$> fns

appFst :: (a -> a) -> [a] -> [a]
appFst _ []= []
appFst f (x:xs) = f x : xs

fancyShow :: Either BussproofsErr String -> String
fancyShow (Left x) = show x
fancyShow (Right x) = x

unlines' = init . unlines
