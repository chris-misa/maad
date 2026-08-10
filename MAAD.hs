{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-
 - Copyright: 2026 Chris Misa
 - License: (See ./LICENSE)
 -
 - Entry point for running various multifractal analysis of IP address space data.
 -
 -}

module MAAD where

import System.Environment
import System.Exit
import System.IO
import Data.Function ((&))
import Control.Arrow
import Control.Monad

import Data.Word
import Data.Bits
import Data.Maybe

import qualified Data.List as L

import Data.Aeson ((.=), Value, encode, object)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL8

import Options.Applicative -- optparse-applicative

import qualified Data.Vector.Unboxed as VU
import qualified Statistics.Sample as SS
import qualified Statistics.Regression as Reg

import Data.TreeFold (treeFold)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import qualified Numeric.LinearAlgebra as LA
import Statistics.Distribution.FDistribution (fDistribution)
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution (cumulative)

-- Local imports
import Common
import PrefixMap (Prefix(..), PrefixMap)
import qualified PrefixMap as PM

defaultFullThreshold :: Double
defaultFullThreshold = 0.05

defaultAutoStopThreshold :: Double
defaultAutoStopThreshold = 0.001

defaultMinPrefixLength :: Int
defaultMinPrefixLength = 8

defaultMaxPrefixLength :: Int
defaultMaxPrefixLength = 24

deltaQ :: Double
deltaQ = 1.0 / 8.0

-- Min q based on theoretic range of normalicy of tauTilde(q)
minQ :: Double
minQ = -1.0 / 2.0

-- Max q based on theoretic range of convergence of tauTilde(q) because it's less sensitive in the Legendre transform
maxQ :: Double
maxQ = 3.5

qs :: [Double]
qs = [minQ, minQ+deltaQ..maxQ]

data OutputFormat = OutputCsv | OutputJson
  deriving (Eq, Show)

data Config = Config
  { cfgFilepath :: String
  , cfgOutPrefix :: String
  , cfgFormat :: OutputFormat
  , cfgStructure :: Bool
  , cfgSpectrum :: Bool
  , cfgDimensions :: Bool
  , cfgPartitions :: Bool
  , cfgSingularities :: Bool
  , cfgWeights :: Bool
  , cfgTest :: Bool
  , cfgTestFile :: Maybe String
  , cfgCsv :: Bool
  , cfgAddrCol :: Maybe Int
  , cfgMeasureCol :: Maybe Int
  , cfgSkipFirst :: Bool
  , cfgFullThresh :: Double
  , cfgAutoStop :: Bool
  , cfgBTarget :: Double
  , cfgForceMinPrefixLength :: Maybe Int
  , cfgForceMaxPrefixLength :: Maybe Int
  , cfgPrefixLengths :: [Int]
  }
  deriving (Show)

requestedAnalysisCount :: Config -> Int
requestedAnalysisCount conf =
  [ cfgStructure
  , cfgSpectrum
  , cfgDimensions
  , cfgPartitions
  , cfgSingularities
  , cfgWeights
  , cfgTest
  , isJust . cfgTestFile
  ]
  & fmap (\f -> if f conf then 1 else 0)
  & foldl1 (+)

data Metadata = Metadata
  { metaInput :: String
  , metaMinPrefixLength :: Int
  , metaMaxPrefixLength :: Int
  , metaTotalAddrs :: Int
  , metaDidAutoStop :: Maybe Int
  , metaPreFilterPrefixCounts :: [(Int, Int)]
  , metaPrefixCounts :: [(Int, Int)]
  , metaMultinomialFits :: [(Double, Double, Double)]
  , metaPerPrefixLengthVars :: [(Int, Double, Double, Double)]
  }

data Results = Results
  { resStructure :: Maybe [(Double, Double, Double)]
  , resSpectrum :: Maybe [(Double, Double)]
  , resDimensions :: Maybe [(Double, Double)]
  , resPartitions :: Maybe [(Double, [(Double, Double)])]
  , resSingularities :: Maybe [(Double, (Word32, Double, Double, Int))]
  , resWeights :: Maybe [(Int, Word32, Double, Double, Double)]
  , resTest :: Maybe (Double, Double, (Double, Double), (Double, Double))
  , resCompare :: Maybe (Double, Double, Int, Int)
  }

parseOutputFormat :: String -> Either String OutputFormat
parseOutputFormat "csv" = Right OutputCsv
parseOutputFormat "json" = Right OutputJson
parseOutputFormat _ = Left "FORMAT must be one of: csv, json"

formatName :: OutputFormat -> String
formatName OutputCsv = "csv"
formatName OutputJson = "json"

optparser :: Parser Config
optparser = Config
  <$> strOption ( long "input"
                  <> metavar "FILEPATH"
                  <> help "File to read (csv or one address on each line)."
                )
  <*> strOption ( long "output"
                  <> metavar "OUT_PREFIX"
                  <> help "Prefix for output files (for FORMAT = csv), output file (for FORMAT = json), or - for stdout."
                )
  <*> option (eitherReader parseOutputFormat) ( long "format"
                                                <> metavar "FORMAT"
                                                <> value OutputCsv
                                                <> showDefaultWith formatName
                                                <> help "Output format: csv or json."
                                              )
  <*> switch ( long "structure" <> short 't'
               <> help "Compute structure function (OUT_PREFIX_structure.csv)."
             )
  <*> switch ( long "spectrum" <> short 's'
               <> help "Compute multifractal spectrum (OUT_PREFIX_spectrum.csv)."
             )
  <*> switch ( long "dimensions" <> short 'd'
               <> help "Compute generalized dimensions (OUT_PREFIX_dimensions.csv)."
             )
  <*> switch ( long "partitions" <> short 'p'
               <> help "Compute partition functions (OUT_PREFIX_partitions.csv). (Note that this uses a different range of q values than the other estimates.)"
             )
  <*> switch ( long "singularities" <> short 'e'
               <> help "Compute the singularities or Hölder exponents estimated at each IP address."
             )
  <*> switch ( long "weights" <> short 'w'
               <> help "Compute the weights at each node in the prefix tree."
             )
  <*> switch ( long "test"
               <> help "Perform simple t-test for nonlinearity of the structure function based on q = 0 and q = 2."
             )
  <*> optional (strOption ( long "compare" <> metavar "FILEPATH2"
                            <> help "Perform Hotelling's t^2 test of the null hypothesis that the addresses in FILEPATH2 come from the same distribution as the addresses in FILEPATH (using the structure function). Assumes that FILEPATH2 follows the same line format as FILEPATH (e.g., csv or raw list of addresses, etc.)."
                            ))
  <*> switch ( long "csv"
               <> help "Input file is csv (with multiple columns that need to be parsed)."
             )
  <*> optional (option auto ( long "addr-col" <> metavar "COL" <> short 'a'
                              <> help "If input is a csv file, this (zero-based) column contains the IP addresses to analyze. Default to column 0."
                            ))
  <*> optional (option auto ( long "meas-col" <> metavar "COL" <> short 'm'
                              <> help "If the input is a csv file, this (zero-based) column contains the measure associated with each IP address. If not specified, each address will receive constant measure 1.0 (even if --csv is specified)."))
  <*> switch ( long "skip-first"
               <> help "Skip the first (header) row before reading the data."
             )
  <*> option auto ( long "full-threshold" <> metavar "C"
                    <> value defaultFullThreshold <> showDefault
                    <> help "Threshold for determining nearly-full prefixes (based on how close log_2(mu) is to capacity at prefix length)."
                  )
  <*> switch ( long "auto-stop" <> help "Automatically stop reading addresses when the maximum estimated CI around counts at the (estimated) maximum significant prefix length is less than B_TARGET."
             )
  <*> option auto ( long "b-target" <> metavar "B_TARGET" <> value defaultAutoStopThreshold <> showDefault
                  <> help "The target CI width used if auto-stop is enabled."
                  )
  <*> optional (option auto ( long "force-min-prefix-length" <> metavar "MIN_LEN"
                            <> help "Override automatic determination of minimum prefix length."
                            ))
  <*> optional (option auto ( long "force-max-prefix-length" <> metavar "MAX_LEN"
                            <> help "Override automatic determination of maximum prefix length."
                            ))
  <*> pure []

opts :: ParserInfo Config
opts = info (optparser <**> helper)
  ( fullDesc
    <> progDesc "Compute a combination of different multifractal analyses of a given set of IP addresses, optimally based on a per-address measure."
    <> header "MAAD - Multifractal Address-space Anomaly Detection"
  )

dieWith :: String -> IO ()
dieWith msg = hPutStrLn stderr msg >> exitFailure

main :: IO ()
main = do
  conf <- execParser opts

  -- Verify that the configuration given in the arguments is valid
  when (requestedAnalysisCount conf <= 0) $
    dieWith "Must specify one of --structure, --spectrum, --dimensions, --partitions, --singularities, --weights, --test, or --compare to compute."
    
  when ((isJust (cfgAddrCol conf) || isJust (cfgMeasureCol conf)) && not (cfgCsv conf)) $
    dieWith "To specify --addr-col or --meas-col, you must also indicate the input is a csv file by specifying --csv"

  when (cfgOutPrefix conf == "-" && cfgFormat conf == OutputCsv && requestedAnalysisCount conf > 1) $
    dieWith "CSV stdout only supports a single requested analysis; use --format json or a file prefix."

  -- Run with verified configuration
  run conf

{-
 - Run analysis and write results as described by the given configuration.
 -}
run :: Config -> IO ()
run conf = do

  (pfxs, didAutoStop) <- loadAddresses conf (cfgFilepath conf)

  (validLengths, validPfxs, preFilterPrefixCounts) <- buildValidPrefixes conf pfxs didAutoStop

  let conf' = conf { cfgPrefixLengths = validLengths }

      (taus, perPrefixLengthVars) = computeTauTilde conf' validPfxs

      -- TDOD add an option so we output per-prefix results only if asked for... 
  
      -- Compute the metadata
      metadata = Metadata
        { metaInput = cfgFilepath conf'
        , metaMinPrefixLength = foldl1 min (cfgPrefixLengths conf')
        , metaMaxPrefixLength = foldl1 max (cfgPrefixLengths conf')
        , metaTotalAddrs = length (PM.leaves pfxs)
        , metaDidAutoStop = didAutoStop
        , metaPreFilterPrefixCounts = preFilterPrefixCounts
        , metaPrefixCounts = fmap (second (HM.size . fst)) (cfgPrefixLengths conf' `zip` validPfxs)
        , metaMultinomialFits = fmap (multinomialFit conf') (cfgPrefixLengths conf' `zip` fmap fst validPfxs)
        , metaPerPrefixLengthVars = perPrefixLengthVars
        }

  
      -- Compute what was requested
      structureRows = if cfgStructure conf' then Just (VU.toList taus) else Nothing
      spectrumRows = if cfgSpectrum conf' then Just (computeSpectrumRows taus) else Nothing
      dimensionRows = if cfgDimensions conf' then Just (computeDimensionRows conf' taus pfxs) else Nothing
      partitionsRows = if cfgPartitions conf' then Just (computePartitions conf' pfxs) else Nothing
      singularitiesRows = if cfgSingularities conf' then Just (computeSingularities conf' pfxs) else Nothing
      weightsRows = if cfgWeights conf' then Just (computeWeights conf' validLengths validPfxs) else Nothing
      testResult = if cfgTest conf' then Just (computeZTest conf' taus) else Nothing
  compareResult <- case cfgTestFile conf' of
    Just testfile -> fmap Just (computeT2Test conf' testfile perPrefixLengthVars)
    Nothing -> return Nothing

  -- Write output to csv files, std out, or json
  emitResults conf' metadata $ Results
    { resStructure = structureRows
    , resSpectrum = spectrumRows
    , resDimensions = dimensionRows
    , resPartitions = partitionsRows
    , resSingularities = singularitiesRows
    , resWeights = weightsRows
    , resTest = testResult
    , resCompare = compareResult
    }

computeZTest :: Config -> VU.Vector (Double, Double, Double) -> (Double, Double, (Double, Double), (Double, Double))
computeZTest conf taus =
  let (_, tauTilde0, sd0) = case VU.find (\(q, _, _) -> q == 0.0) taus of
        Just t -> t
        Nothing -> error $ "Failed to find q == 0.0!"
      (_, tauTilde2, sd2) = case VU.find (\(q, _, _) -> q == 2.0) taus of
        Just t -> t
        Nothing -> error $ "Failed to find q == 2.0!"

  -- Under null hypothesis that the measure is not multifractal, we have tau0 + tau2 == 0 (because tau1 == 0)
  -- Also, tauTilde0 + tauTilde2 is Normal with mean tau0 + tau2 = 0 and variance sd0^2 + sd1^2
  -- use two-tailed z-test
      z = (tauTilde0 + tauTilde2) / sqrt (sd0 ** 2.0 + sd2 ** 2.0)
      p = 2.0 * cumulative (normalDistr 0.0 1.0) (- abs z)
  in (p, z, (tauTilde0, sd0), (tauTilde2, sd2))

  -- turns out this doesn't work that well because the variances are super low---so it's over sensitive.
  -- e.g., for uniform the absolute value tauTilde0 + tauTilde1 is smaller, but the variance is so low that the z statistic becomes way more extreme than in the real-world case where the variance is higher...

  -- interestingly, for the Cantor-set construction, we get much higher p-values (e.g., 0.83 using /8 - /16)
  -- this is not because the thing is more curved, but because the variance is higher (similar to real-world).

  -- still should check if there's any reason to suspect the O&W variance is under-estimating!
  -- the O&W estimators variance approaches zero in the perfectly-uniform case...
  -- also, it goes to zero faster than tauTilde0 + tauTilde2 goes to zero...

computeWeights :: Config
               -> [Int]
               -> [(HashMap Prefix Double, HashMap Prefix Double)]
               -> [(Int, Word32, Double, Double, Double)]
computeWeights conf pls pfxs =
  let onePl (pl, (thisPl, nextPl)) = thisPl
        & HM.toList
        & fmap (\(pfx, mu) ->
                  let addr = PM.prefixToAddress pfx
                      left = HM.lookupDefault 0.0 (Prefix addr (pl + 1)) nextPl
                      -- for sanity checking, also compute the right weight manually like this
                      right = HM.lookupDefault 0.0 (Prefix (addr .|. (1 `shiftL` (32 - (pl + 1)))) (pl + 1)) nextPl
                  in (pl, addr, mu, left / mu, right / mu)
               )
  in concatMap onePl (pls `zip` pfxs)

{-
 - Load addresses from file using parameters specified in the configuration
 -}
loadAddresses :: Config -> String -> IO (PrefixMap Double, Maybe Int)
loadAddresses conf filepath = do
  let extractSingleAddr :: [ByteString] -> ByteString
      extractSingleAddr (addr:_) = addr
      extractSingleAddr [] = error "Expected at least one column in each input row"

  -- Load in the addresses and optional associated "weights"
  let autoStopConf = case cfgAutoStop conf of
          True -> Just (cfgBTarget conf)
          False -> Nothing

  if cfgCsv conf
    then let extract_addr = flip (!!) (fromMaybe 0 (cfgAddrCol conf)) -- default to column 0
             extract_meas =
               case cfgMeasureCol conf of
                 Just col -> read . B.unpack . flip (!!) col
                 Nothing -> const 1.0 -- default to constant 1.0 for each address
         in PM.fromFile filepath (cfgSkipFirst conf) autoStopConf extract_addr extract_meas
    else PM.fromFile filepath (cfgSkipFirst conf) autoStopConf extractSingleAddr (const 1.0)

{-
 - Figure out prefix length range and build list of valid prefixes (and next-child prefixes) at each prefix length.
 -
 - In IO because it might need to print some warnings...
 -}
buildValidPrefixes :: Config -> PrefixMap Double -> Maybe Int -> IO ([Int], [(HashMap Prefix Double, HashMap Prefix Double)], [(Int, Int)])
buildValidPrefixes conf pfxs didAutoStop = do
  let minPrefixLength = case cfgForceMinPrefixLength conf of
        Just pl -> pl
        Nothing -> defaultMinPrefixLength
  let maxPrefixLength = case cfgForceMaxPrefixLength conf of
        Just pl -> pl
        Nothing -> case didAutoStop of
          Just autoStopPl -> autoStopPl
          Nothing -> defaultMaxPrefixLength

  hPutStrLn stderr $ "Min prefix length: " ++ show minPrefixLength
  hPutStrLn stderr $ "Max prefix length: " ++ show maxPrefixLength
  when (minPrefixLength >= maxPrefixLength) (error "Invalid prefix length range. If this happens automatically, consider overriding prefix length range with --force-min-prefix-length and --force-max-prefix-length")
  
  let initialPrefixLengths = [minPrefixLength .. maxPrefixLength]

      -- Compute pre-filter per-prefix-length counts
      preFilterPrefixCounts :: [(Int, Int)]
      preFilterPrefixCounts = [(pl, length $ PM.leaves $ PM.sliceAtLength pl pfxs) | pl <- initialPrefixLengths]
  
      -- Compute the sets of prefixes at each length with valid scaling behavior
      validPfxsEmpties :: [(HashMap Prefix Double, HashMap Prefix Double)]
      validPfxsEmpties = fmap (filterValidPrefixes conf pfxs) initialPrefixLengths

      -- Filter out prefix lengths where there are actually zero valid prefixes
      validPfxsLengths = zip initialPrefixLengths validPfxsEmpties
                         & filter ((> 0) . HM.size . fst . snd)

      validLengths = fmap fst validPfxsLengths
      validPfxs = fmap snd validPfxsLengths

  -- Warn if we filtered any prefix lengths due to zero valid prefixes
  when (length validPfxsEmpties /= length validPfxs) $ do
    hPutStrLn stderr $ "WARNING: dropping the following prefix lengths because they had no valid prefixes:" ++ show (initialPrefixLengths & filter (not . flip elem validLengths))

  return (validLengths, validPfxs, preFilterPrefixCounts)


{-
 - Filters the prefix map to remove atomic and nearly-full prefixes at pl.
 - Returns maps for the valid prefixes at pl and their children at pl + 1
 -}
filterValidPrefixes :: Config -> PrefixMap Double -> Int -> (HashMap Prefix Double, HashMap Prefix Double)
filterValidPrefixes conf pm pl =
  let removeAtomicAndFull count pfx _ =
        let pl' = PM.prefixLength pfx
            delta = cfgFullThresh conf
        in count > 1 && logBase 2 (fromIntegral count) / (32.0 - fromIntegral pl') < 1.0 - delta
        
      thisPl = pm
        & PM.sliceAtLength pl
        & PM.filterCount removeAtomicAndFull
        & PM.leaves
        & filter ((== pl) . PM.prefixLength . fst) -- catch any leaves shorter than pl that filterCount might have left in
        & HM.fromList

      nextPl = pm
        & PM.sliceAtLength (pl + 1)
        & PM.leaves
        & filter ((`HM.member` thisPl) . (flip PM.preserve_upper_bits32 pl) . fst)
        & HM.fromList
        
  in (thisPl, nextPl)


{-
 - Computes the tauTilde estimator using the given prefix lengths and per-prefix-length maps
 - Returns both the averaged tauTilde vs. q result as well as the per-prefix-length estimates
 -}
computeTauTilde :: Config -> [(HashMap Prefix Double, HashMap Prefix Double)] -> (VU.Vector (Double, Double, Double), [(Int, Double, Double, Double)])
computeTauTilde conf validPfxs =

  -- Compute tauTilde at each prefix length, each value of q
  let allMoments :: [(Double, [(Double, Double)])]
      allMoments = [(q, [oneMoment conf q pfxs | pfxs <- validPfxs]) | q <- qs]


      -- Compute the structure function from all tauTildes
      oneTau (q, moms) = 
        let n = fromIntegral (length moms)
            tauTilde = moms
              & fmap fst
              & VU.fromList
              & SS.mean
            sd = moms
              & fmap snd
              & treeFold (+) 0.0
              & ((/ n) . sqrt)
        in (q, tauTilde, sd)

      taus = allMoments & fmap oneTau & VU.fromList

      -- Just dump all the per-prefix-length variances and summarize later
      perPrefixLengthVars :: [(Int, Double, Double, Double)]
      perPrefixLengthVars =
        [ (pl, q, tau, v) | (q, moms) <- allMoments, (pl, (tau, v)) <- (cfgPrefixLengths conf `zip` moms)]
  in (taus, perPrefixLengthVars)


{-
 - Load the addresses in testfile and compare them against the addresses represented by baselinePerPrefixLengths
 - The null hypothesis is that the addresses in testfile have the same distribution as baselinePerPrefixLengths.
 -
 - Returns:
 - * the p-value of the test (probability of the observation if the null hypothesis is true)
 - * the raw value of the F-distributed estimator
 - * the number of prefix lengths used (i.e., number of samples)
 - * the number of q values used (i.e., the dimension of the assumed underlying multivariate Normal distribution)
 -}
computeT2Test :: Config -> String -> [(Int, Double, Double, Double)] -> IO (Double, Double, Int, Int)
computeT2Test conf testfile baselinePerPrefixLengths = do

  -- First load the test addresses and compute their tauTilde values
  -- Override the prefix lengths and disable auto-stop to make comparison more direct
  let testConf = conf { cfgAutoStop = False
                      , cfgForceMinPrefixLength = Just (minimum (cfgPrefixLengths conf))
                      , cfgForceMaxPrefixLength = Just (maximum (cfgPrefixLengths conf))
                      }
        
  (testPfxs, _) <- loadAddresses testConf testfile
  
  (testLengths, testPfxsValid, _) <- buildValidPrefixes testConf testPfxs Nothing

  let (_, testPerPrefixLengths) = computeTauTilde (testConf { cfgPrefixLengths = testLengths }) testPfxsValid

      -- Figure out intersection of cfgPrefixLengths conf and testLengths and only use those in the following
      validLengths = cfgPrefixLengths conf `L.intersect` testLengths

      -- Filter both baseline and test prefixes based on validLengths
  let baselines = baselinePerPrefixLengths
        & filter (\(pl, _, _, _) -> elem pl validLengths)

      tests = testPerPrefixLengths
        & filter (\(pl, _, _, _) -> elem pl validLengths)

      -- Need number of qs values smaller than number of prefix lengths
      -- could be a more elegant way to handle this...
      testQs = filter (\q -> q >= 0.0 && q <= 2.0) qs
        & take (length validLengths - 1)

  when (length testQs >= length validLengths) $
    error $ "Test doesn't work if there are not more valid prefix lengths than q values! Current intersection of valid prefix lengths in both baseline and test sets is " ++ show validLengths ++ " and current list of q values is " ++ show testQs

      -- Number of samples: each prefix length is considered a sample
  let n = length validLengths

      -- Size of each sample: each q value is considered a dimension of the sample
      p = length testQs

      -- Form sample matrices: each row is a q value, each column is a prefix length value
      x = LA.fromLists
        ( testQs & fmap (\target_q -> baselines
                      & filter (\(_, q, _, _) -> q == target_q)
                      & fmap (\(_, _, tau, sd) -> tau)
                    )
        )

      -- mean over all columns
      xBar = [0 .. p - 1]
        & fmap (\row_idx -> LA.sumElements (x LA.?? (LA.Pos (LA.idxs [row_idx]), LA.All)))
        & LA.vector
        & (/ fromIntegral p)

  
      y = LA.fromLists
        ( testQs & fmap (\target_q -> tests
                      & filter (\(_, q, _, _) -> q == target_q)
                      & fmap (\(_, _, tau, sd) -> tau)
                    )
        )

      -- mean over all columns
      yBar = [0 .. p - 1]
        & fmap (\row_idx -> LA.sumElements (y LA.?? (LA.Pos (LA.idxs [row_idx]), LA.All)))
        & LA.vector
        & (/ fromIntegral p)

      z = y - x
      zBar = yBar - xBar

      sHat = [0 .. n - 1]
        & fmap (\col_idx ->
                  let zi = LA.flatten (z LA.?? (LA.All, LA.Pos (LA.idxs [col_idx])))
                  in LA.outer (zi - zBar) (zi - zBar)
               )
        & foldl1 (+)
        & (/ fromIntegral n)

      gamma = ((fromIntegral n - fromIntegral p) / fromIntegral p) * (zBar LA.<.> (LA.inv sHat LA.#> zBar))

      pValue = 1.0 - cumulative (fDistribution p (n - p)) gamma

  return (pValue, gamma, n, p)


{-
 - Estimate multinomial CIs
 - TODO: think about if we really need this since we're already doing it pre-filter now?
 -}
multinomialFit :: Config -> (Int, HashMap Prefix Double) -> (Double, Double, Double)
multinomialFit conf (len, pfxs)
  | HM.size pfxs > 0 =
    let n = HM.foldl' (+) 0.0 pfxs
        b = 35.1967321136596 -- Upper tail of the (0.05 / 2^24)-quantile of the Chi distribution with one degree of freedom (Computed in R with: qchisq(p = 0.05 / (2^24), df = 1, lower.tail = FALSE))
        lower_limit = sqrt b / 16
        (maxP, maxB) = HM.elems pfxs
          & fmap (/ n) -- [Double] -- the pi_i's
          & fmap (\pi -> (pi, sqrt (b * pi * (1.0 - pi) / n))) -- [(Double, Double)] -- add the b_i's
          & L.maximumBy (\l r -> compare (snd l) (snd r))
    in (maxP, maxB, lower_limit)
  | otherwise = (0, 0, 0)
  


{-
 - Compute the modified O&W estimator for a single prefix length and q pair
 -
 - Returns the estimated tau(q) and variance
 -}
oneMoment :: Config -> Double -> (HashMap Prefix Double, HashMap Prefix Double) -> (Double, Double)
oneMoment conf q (thisPl, nextPl) =

  -- Note that any normalization cancels out, but we do it anyway because it may help numeric precision (i.e., to avoid sums of super large/small values)
  let total = treeFold (+) 0.0 (HM.elems thisPl)

      thisZ = HM.elems thisPl
        & fmap ((** q) . (/ total))
        & treeFold (+) 0.0

      nextZ = HM.elems nextPl
        & fmap ((** q) . (/ total))
        & treeFold (+) 0.0

      oneD2 (pfx, count) =
        let childSum = PM.children pfx -- [Prefix]
              & fmap (`HM.lookup` nextPl) -- [Maybe Double]
              & filter isJust
              & fmap fromJust -- [Double]
              & (\l -> if length l == 0 then error ("empty child list for prefix " ++ show pfx ++ " with count " ++ show count) else l)
              & fmap ((** q) . (/ total))
              & foldl1 (+)
            mu = count / total
        in (((mu ** q) / thisZ) - (childSum / nextZ)) ** 2.0
              
      d2 = thisPl
        & HM.toList
        & fmap oneD2
        & treeFold (+) 0.0

  in (logBase 2 thisZ - logBase 2 nextZ, d2)

{-
 - Compute multifractal spectrum rows.
 -}
computeSpectrumRows :: VU.Vector (Double, Double, Double) -> [(Double, Double)]
computeSpectrumRows taus =
  -- Estimate alpha and f(alpha) for each q
  let alphas = [1..VU.length taus - 2]
        & fmap (\i ->
                  let (_, prevTau, _) = taus VU.! (i - 1)
                      (q, tau, _) = taus VU.! i
                      (_, nextTau, _) = taus VU.! (i + 1)
                      alpha = (nextTau - prevTau) / (2 * deltaQ)
                      f = q * alpha - tau
                  in (alpha, f)
               )

      -- Filter for range where alpha is monotonic decreasing
      -- Note this always skips the first alpha. Should be ok if we have enough alpha samples...
      diffs = zip alphas (drop 1 alphas)
        & fmap (\((a1, _), (a2, f2)) -> (a1 > a2, (a2, f2)))
        & dropWhile (not . fst) -- assume it only turns around once at beginning and once at end...
        & takeWhile fst
        & fmap snd
  in diffs

{-
 - Compute generalized dimension rows.
 -}
computeDimensionRows :: Config -> VU.Vector (Double, Double, Double) -> PrefixMap Double -> [(Double, Double)]
computeDimensionRows conf taus pfxs =
  let d1 = infoDim conf pfxs
      otherDims = taus
        & VU.toList
        & filter (\(q, _, _) -> q == 0.0 || q == 2.0)
        & fmap (\(q, tauTilde, _) -> (q, tauTilde / (q - 1.0)))
  in (1.0, d1) : otherDims

{-
 - Compute D_1, the information dimension
 -}
infoDim :: Config -> PrefixMap Double -> Double
infoDim conf pfxs =
  -- lim_{r to 0} ( sum_i p_i * log(p_i) ) / log(r)
  -- lim_{l to infty} (sum_i p_i * log(p_i)) / -l
  let total = treeFold (+) 0.0 $ fmap snd $ PM.leaves pfxs
      oneEntropy :: Int -> Double
      oneEntropy pl = pfxs
        & PM.sliceAtLength pl
        & PM.leaves
        & fmap (\(_, weight) ->
                  let p = weight / total in p * logBase 2 p
               )
        & treeFold (+) 0.0
      entropies = cfgPrefixLengths conf
        & fmap oneEntropy
        & VU.fromList
      pls = VU.generate (VU.length entropies) (negate . fromIntegral)
      (coef, _r2) = Reg.olsRegress [pls] entropies
  in coef VU.! 0

computePartitions :: Config -> PrefixMap Double -> [(Double, [(Double, Double)])]
computePartitions conf pfxs =
  let total = treeFold (+) 0.0 $ fmap snd $ PM.leaves pfxs

      getZ q pl =
        let z = pfxs
              & PM.sliceAtLength pl
              & PM.leaves
              & fmap ((** q) . (/ total) . snd)
              & treeFold (+) 0.0
        in (fromIntegral pl, z)
        
      oneQ q =
        let zs = fmap (getZ q) [0..32]
        in (q, zs)

  in fmap oneQ [-2.0, -1.9..4.0]

{-
 - Report the singularity estimates of each address w.r.t. the prefix map
 - Returns (alpha, (address, intercept, r2, number of prefix-lengths actually used))
 -}
computeSingularities :: Config -> PrefixMap Double -> [(Double, (Word32, Double, Double, Int))]
computeSingularities conf pfxs =
  let addrs = PM.leaves pfxs

      total = treeFold (+) 0.0 $ fmap snd addrs

      getSingularity :: (Prefix, Double) -> (Double, (Word32, Double, Double, Int))
      getSingularity (Prefix addr 32, _) =
        let oneLevel l =
              let pfx = PM.preserve_upper_bits32 (Prefix addr 32) l
                  mu = fromJust $ PM.lookup pfx pfxs
                  muNorm = mu  / total
              in (- logBase 2 muNorm, mu /= 1)
  
            muLogs = VU.generate 33 oneLevel & VU.takeWhile snd & VU.map fst
            pl = VU.generate (VU.length muLogs) fromIntegral

            (coef, r2) = Reg.olsRegress [pl] muLogs
        in (coef VU.! 0, (addr, coef VU.! 1, r2, VU.length muLogs))
      getSingularity (Prefix _ pl, _) = error $ "Got a /" ++ show pl ++ " prefix as a leaf in computeSingularities. Something's broken."

  in addrs
     & fmap getSingularity
     & L.sortOn fst

{-
 - Emit results in the requested output format.
 - Just dispatch based on csv or json.
 -}
emitResults :: Config
            -> Metadata
            -> Results
            -> IO ()
emitResults conf =
  case cfgFormat conf of
    OutputCsv -> emitCsvResults conf
    OutputJson -> emitJsonResults conf

{-
 - Emit csv results to stdout or files.
 -}
emitCsvResults :: Config
               -> Metadata
               -> Results
               -> IO ()
emitCsvResults conf metadata res =
  if cfgOutPrefix conf == "-"
  then do
    maybe (return ()) (writeStructureCsv stdout) (resStructure res)
    maybe (return ()) (writeSpectrumCsv stdout) (resSpectrum res)
    maybe (return ()) (writeDimensionsCsv stdout) (resDimensions res)
    maybe (return ()) (writePartitionsCsv stdout) (resPartitions res)
    maybe (return ()) (writeSingularities stdout) (resSingularities res)
    maybe (return ()) (writeWeights stdout) (resWeights res)
    maybe (return ()) (writeZTestResult stdout) (resTest res)
    maybe (return ()) (writeT2TestResult stdout) (resCompare res)
  else do
    writeMetadata conf metadata
    maybe (return ()) (writeStructureFile conf) (resStructure res)
    maybe (return ()) (writeSpectrumFile conf) (resSpectrum res)
    maybe (return ()) (writeDimensionsFile conf) (resDimensions res)
    maybe (return ()) (writePartitionsFile conf) (resPartitions res)
    maybe (return ()) (writeSingularitiesFile conf) (resSingularities res)
    maybe (return ()) (writeWeightsFile conf) (resWeights res)
    maybe (return ()) (writeZTestResultFile conf) (resTest res)
    maybe (return ()) (writeT2TestResultFile conf) (resCompare res)

{-
 - Write some metadata to keep track of config and parameters that were auto-generated here
 -}
writeMetadata :: Config -> Metadata -> IO ()
writeMetadata conf metadata = do
  let outfile = cfgOutPrefix conf ++ "_metadata.csv"
  hPutStrLn stderr $ "Writing metadata to " ++ outfile
  withFile outfile WriteMode $ \hdl -> do
    hPutStrLn hdl "key,value"
    hPutStrLn hdl $ "input," ++ metaInput metadata
    hPutStrLn hdl $ "min_prefix_length," ++ show (metaMinPrefixLength metadata)
    hPutStrLn hdl $ "max_prefix_length," ++ show (metaMaxPrefixLength metadata)
    hPutStrLn hdl $ "total_addrs," ++ show (metaTotalAddrs metadata)
    hPutStrLn hdl $ "did_auto_stop," ++ case (metaDidAutoStop metadata) of
      Just max_pl -> "True"
      Nothing -> "False"
    forM_ (metaPreFilterPrefixCounts metadata) $ \(pl, count) ->
      hPutStrLn hdl $ "pre_filter_prefix_count/" ++ show pl ++ "," ++ show count
    forM_ (metaPrefixCounts metadata) $ \(pl, count) ->
      hPutStrLn hdl $ "prefix_count/" ++ show pl ++ "," ++ show count
    forM_ (metaMultinomialFits metadata `zip` metaPrefixCounts metadata) $ \((maxP, maxB, lower_limit), (pl, _)) ->
      hPutStrLn hdl $ "multinomial_fit/" ++ show pl ++ "," ++ show maxP ++ ":" ++ show maxB ++ ":" ++ show lower_limit
    forM_ (metaPerPrefixLengthVars metadata) $ \(pl, q, tau, v) ->
      hPutStrLn hdl $ "var/" ++ show pl ++ "," ++ show q ++ ":" ++ show tau ++ ":" ++ show v

{-
 - Write the structure function
 -}
writeStructureFile :: Config -> [(Double, Double, Double)] -> IO ()
writeStructureFile conf rows = do
  let outfile = cfgOutPrefix conf ++ "_structure.csv"
  hPutStrLn stderr $ "Writing structure function to " ++ outfile
  withFile outfile WriteMode (\hdl -> writeStructureCsv hdl rows)

writeStructureCsv :: Handle -> [(Double, Double, Double)] -> IO ()
writeStructureCsv hdl rows = do
  hPutStrLn hdl "q,tauTilde,sd"
  forM_ rows $ \(q, tauTilde, sd) ->
    hPutStrLn hdl (show q ++ "," ++ show tauTilde ++ "," ++ show sd)


{-
 - Write multifractal spectrum.
 -}
writeSpectrumFile :: Config -> [(Double, Double)] -> IO ()
writeSpectrumFile conf rows = do
  let outfile = cfgOutPrefix conf ++ "_spectrum.csv"
  hPutStrLn stderr $ "Writing multifractal spectrum to " ++ outfile
  withFile outfile WriteMode (\hdl -> writeSpectrumCsv hdl rows)

writeSpectrumCsv :: Handle -> [(Double, Double)] -> IO ()
writeSpectrumCsv hdl rows = do
  hPutStrLn hdl "alpha,f"
  forM_ rows $ \(alpha, f) ->
    hPutStrLn hdl (show alpha ++ "," ++ show f)


{-
 - Write generalized dimensions.
 -}
writeDimensionsFile :: Config -> [(Double, Double)] -> IO ()
writeDimensionsFile conf rows = do
  let outfile = cfgOutPrefix conf ++ "_dimensions.csv"
  hPutStrLn stderr $ "Writing generalized dimensions to " ++ outfile
  withFile outfile WriteMode (\hdl -> writeDimensionsCsv hdl rows)

writeDimensionsCsv :: Handle -> [(Double, Double)] -> IO ()
writeDimensionsCsv hdl rows = do
  hPutStrLn hdl "q,dim"
  forM_ rows $ \(q, dim) ->
    hPutStrLn hdl (show q ++ "," ++ show dim)

{-
 - Write partition functions.
 -}
writePartitionsFile :: Config -> [(Double, [(Double, Double)])] -> IO ()
writePartitionsFile conf rows = do
  let outfile = cfgOutPrefix conf ++ "_partitions.csv"
  hPutStrLn stderr $ "Writing partition functions to " ++ outfile
  withFile outfile WriteMode (\hdl -> writePartitionsCsv hdl rows)

writePartitionsCsv :: Handle -> [(Double, [(Double, Double)])] -> IO ()
writePartitionsCsv hdl rows = do
  hPutStrLn hdl "q,pl,z"
  forM_ rows $ \(q, zs) ->
    forM_ zs $ \(pl, z) ->
                 hPutStrLn hdl (show q ++ "," ++ show pl ++ "," ++ show z)

{-
 - Write singularities
 -}
writeSingularitiesFile :: Config -> [(Double, (Word32, Double, Double, Int))] -> IO ()
writeSingularitiesFile conf rows = do
  let outfile = cfgOutPrefix conf ++ "_singularities.csv"
  hPutStrLn stderr $ "Writing singularities to " ++ outfile
  withFile outfile WriteMode (\hdl -> writeSingularities hdl rows)

writeSingularities :: Handle -> [(Double, (Word32, Double, Double, Int))] -> IO ()
writeSingularities hdl rows = do
  hPutStrLn hdl "alpha,addr,intercept,r2,num_levels"
  forM_ rows $ \(alpha, (addr, intercept, r2, num_levels)) -> do
    hPutStrLn hdl $ show alpha
      ++ "," ++ B.unpack (ipv4_to_string addr)
      ++ "," ++ show intercept
      ++ "," ++ show r2
      ++ "," ++ show num_levels

{-
 - Write singularities
 -}
writeWeightsFile :: Config -> [(Int, Word32, Double, Double, Double)] -> IO ()
writeWeightsFile conf rows = do
  let outfile = cfgOutPrefix conf ++ "_weights.csv"
  hPutStrLn stderr $ "Writing weightsto " ++ outfile
  withFile outfile WriteMode (\hdl -> writeWeights hdl rows)

writeWeights :: Handle -> [(Int, Word32, Double, Double, Double)] -> IO ()
writeWeights hdl rows = do
  hPutStrLn hdl "pl,addr,mu,left,right"
  forM_ rows $ \(pl, addr, mu, left, right) -> do
    hPutStrLn hdl $ show pl
      ++ "," ++ B.unpack (ipv4_to_string addr)
      ++ "," ++ show mu
      ++ "," ++ show left
      ++ "," ++ show right


{-
 - Write t-test results
 -}
writeZTestResultFile :: Config -> (Double, Double, (Double, Double), (Double, Double)) -> IO ()
writeZTestResultFile conf res = do
  let outfile = cfgOutPrefix conf ++ "_ttest.csv"
  hPutStrLn stderr $ "Writing test results to " ++ outfile
  withFile outfile WriteMode (\hdl -> writeZTestResult hdl res)

writeZTestResult :: Handle -> (Double, Double, (Double, Double), (Double, Double)) -> IO ()
writeZTestResult hdl (p, t, (tauTilde0, sd0), (tauTilde2, sd2)) = do
  hPutStrLn hdl "p_value,t_value,tauTilde_0,sd_0,tauTilde_2,sd_2"
  hPutStrLn hdl $ show p
    ++ "," ++ show t
    ++ "," ++ show tauTilde0
    ++ "," ++ show sd0
    ++ "," ++ show tauTilde2
    ++ "," ++ show sd2

{-
 - Write t2-test comparison results
 -}
writeT2TestResultFile :: Config -> (Double, Double, Int, Int) -> IO ()
writeT2TestResultFile conf res = do
  let outfile = cfgOutPrefix conf ++ "_compare.csv"
  hPutStrLn stderr $ "Writing test results to " ++ outfile
  withFile outfile WriteMode (\hdl -> writeT2TestResult hdl res)

writeT2TestResult :: Handle -> (Double, Double, Int, Int) -> IO ()
writeT2TestResult hdl (p_val, gamma, n, p) = do
  hPutStrLn hdl "p_value,gamma,n,p"
  hPutStrLn hdl $ show p_val
    ++ "," ++ show gamma
    ++ "," ++ show n
    ++ "," ++ show p

{-
 - Emit json results to stdout or file.
 -}
emitJsonResults :: Config
                -> Metadata
                -> Results
                -> IO ()
emitJsonResults conf metadata res = do
  let payload = encodeResultsJson metadata res
  if cfgOutPrefix conf == "-"
  then BL8.putStrLn payload
  else do
    let outfile = cfgOutPrefix conf ++ ".json"
    hPutStrLn stderr $ "Writing json results to " ++ outfile
    BL8.writeFile outfile (payload <> "\n")

encodeResultsJson :: Metadata
                  -> Results
                  -> BL8.ByteString
encodeResultsJson metadata res =
  encode $
    object $
      [ "schemaVersion" .= (1 :: Int)
      , "metadata" .= encodeMetadataJson metadata
      ]
      ++ maybe [] (\rows -> ["structure" .= encodeStructureRowsJson rows]) (resStructure res)
      ++ maybe [] (\rows -> ["spectrum" .= encodeSpectrumRowsJson rows]) (resSpectrum res)
      ++ maybe [] (\rows -> ["dimensions" .= encodeDimensionRowsJson rows]) (resDimensions res)
      ++ maybe [] (\rows -> ["partitions" .= encodePartitionsRowsJson rows]) (resPartitions res)
      ++ maybe [] (\rows -> ["singularities" .= encodeSingularitiesRowsJson rows]) (resSingularities res)
      -- TODO: add testResult: both t-test and t2-test/compare !!

encodeMetadataJson :: Metadata -> Value
encodeMetadataJson metadata =
  object
    [ "input" .= metaInput metadata
    , "minPrefixLength" .= metaMinPrefixLength metadata
    , "maxPrefixLength" .= metaMaxPrefixLength metadata
    , "totalAddrs" .= metaTotalAddrs metadata
    , "didAutoStop" .= metaDidAutoStop metadata
    , "prefix_counts" .= encodePrefixCountsJson (metaPrefixCounts metadata)
    -- TODO: add pre-filter prefix counts, multinomial fits and per-prefix-length variances!!!
    ]

encodePrefixCountsJson :: [(Int, Int)] -> [Value]
encodePrefixCountsJson counts =
  fmap
    (\(pl, count) ->
       object
         [ "pl" .= pl
         , "count" .= count
         ]
    )
    counts

encodeMultinomialFits :: [(Double, Double, Double)] -> [Value]
encodeMultinomialFits fits =
  fmap
    (\(maxP, maxB, lower_limit) ->
       object
         [ "maxP" .= maxP
         , "maxB" .= maxB
         , "lower_limit" .= lower_limit
         ]
    )
    fits

encodeStructureRowsJson :: [(Double, Double, Double)] -> [Value]
encodeStructureRowsJson rows =
  fmap
    (\(q, tauTilde, sd) ->
      object
        [ "q" .= q
        , "tauTilde" .= tauTilde
        , "sd" .= sd
        ]
    )
    rows

encodeSpectrumRowsJson :: [(Double, Double)] -> [Value]
encodeSpectrumRowsJson rows =
  fmap
    (\(alpha, f) ->
      object
        [ "alpha" .= alpha
        , "f" .= f
        ]
    )
    rows

encodeDimensionRowsJson :: [(Double, Double)] -> [Value]
encodeDimensionRowsJson rows =
  fmap
    (\(q, dim) ->
      object
        [ "q" .= q
        , "dim" .= dim
        ]
    )
    rows

encodePartitionsRowsJson :: [(Double, [(Double, Double)])] -> [Value]
encodePartitionsRowsJson =
  concatMap
    (\(q, zs) ->
       fmap (\(pl, z) ->
               object
               [ "q" .= q
               , "pl" .= pl
               , "z" .= z
               ]
            ) zs
    )

encodeSingularitiesRowsJson :: [(Double, (Word32, Double, Double, Int))] -> [Value]
encodeSingularitiesRowsJson =
  fmap
    (\(alpha, (addr, intercept, r2, num_levels)) ->
        object
        [ "alpha" .= alpha
        , "addr" .= B.unpack (ipv4_to_string addr)
        , "intercept" .= intercept
        , "r2" .= r2
        , "num_levels" .= num_levels
        ]
    )


