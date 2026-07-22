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
import Data.Maybe

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

-- Local imports
import Common
import PrefixMap (Prefix(..), PrefixMap)
import qualified PrefixMap as PM

defaultAtomicThreshold :: Double
defaultAtomicThreshold = 0.0

defaultFullThreshold :: Double
defaultFullThreshold = 0.05

-- Hard max prefix length to avoid other nastiness at long prefix lengths (e.g., dynamic addressing, extreme sparseness)
maxPrefixLength :: Int
maxPrefixLength = 24

defaultAutoStopLength :: Int
defaultAutoStopLength = 24

defaultAutoStopThreshold :: Double
defaultAutoStopThreshold = 0.01

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
  , cfgCsv :: Bool
  , cfgAddrCol :: Maybe Int
  , cfgMeasureCol :: Maybe Int
  , cfgSkipFirst :: Bool
  , cfgAtomicThresh :: Double
  , cfgFullThresh :: Double
  , cfgAutoStop :: Maybe (Int, Double)
  , cfgPrefixLengths :: [Int]
  }
  deriving (Show)

requestedAnalysisCount :: Config -> Int
requestedAnalysisCount conf = [ cfgStructure, cfgSpectrum, cfgDimensions, cfgPartitions ]
  & fmap (\f -> if f conf then 1 else 0)
  & foldl1 (+)

data Metadata = Metadata
  { metaInput :: String
  , metaMinPrefixLength :: Int
  , metaMaxPrefixLength :: Int
  , metaTotalAddrs :: Int
  , metaDidAutoStop :: Bool
  , metaPrefixCounts :: [(Int, Int)]
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
                  <> metavar "FILEPATH_PREFIX"
                  <> help "Prefix for output files, or - for stdout."
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
  <*> option auto ( long "atomic-threshold" <> metavar "THRESH"
                    <> value defaultAtomicThreshold <> showDefault
                    <> help "Determine minimum prefix length as smallest prefix length where the fraction of atomic prefixes are at least THRESH."
                  )
  <*> option auto ( long "full-threshold" <> metavar "DELTA"
                    <> value defaultFullThreshold <> showDefault
                    <> help "Threshold for determining when a prefix is estimated to be full. Mostly only important for determining max prefix length."
                  )
  <*> flag Nothing (Just (defaultAutoStopLength, defaultAutoStopThreshold)) ( long "auto-stop"
                                                                              <> help ("Automatically stop reading addresses after the estimated normalized CI around /" ++ show defaultAutoStopLength ++ " prefixes is smaller than " ++ show defaultAutoStopThreshold ++ ".")
                                                                            )
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
    dieWith "Must specify one of --structure, --spectrum, --dimensions, or --partitions to compute."
    
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

  let extractSingleAddr :: [ByteString] -> ByteString
      extractSingleAddr (addr:_) = addr
      extractSingleAddr [] = error "Expected at least one column in each input row"

  -- Load in the addresses and optional associated "weights"
  (pfxs, didAutoStop) <-
        if cfgCsv conf
        then let extract_addr = flip (!!) (fromMaybe 0 (cfgAddrCol conf)) -- default to column 0
                 extract_meas =
                   case cfgMeasureCol conf of
                     Just col -> read . B.unpack . flip (!!) col
                     Nothing -> const 1.0 -- default to constant 1.0 for each address
             in PM.fromFile (cfgFilepath conf) (cfgSkipFirst conf) (cfgAutoStop conf) extract_addr extract_meas
        else PM.fromFile (cfgFilepath conf) (cfgSkipFirst conf) (cfgAutoStop conf) extractSingleAddr (const 1.0)

  let !firstAtomicLength = 8 -- PM.firstAtomicLengthThreshold (cfgAtomicThresh conf) pfxs
  let !firstFullLength = 24 -- maxPrefixLength -- HACKED
        -- case PM.firstFullLength (cfgFullThresh conf) pfxs of
        --   x | x < maxPrefixLength -> x
        --     | otherwise -> maxPrefixLength

  hPutStrLn stderr $ "Min prefix length: " ++ show firstAtomicLength
  hPutStrLn stderr $ "Max prefix length: " ++ show firstFullLength
  let conf' = conf { cfgPrefixLengths = [firstAtomicLength .. firstFullLength] }

      -- Compute the sets of prefixes at each length with valid scaling behavior
      validPfxs :: [(HashMap Prefix Double, HashMap Prefix Double)]
      validPfxs = fmap (filterValidPrefixes conf' pfxs) (cfgPrefixLengths conf')

      -- TODO: compute the multinomial thing for each measure subset in validPfxs
  
      metadata = Metadata
        { metaInput = cfgFilepath conf
        , metaMinPrefixLength = foldl1 min (cfgPrefixLengths conf')
        , metaMaxPrefixLength = foldl1 max (cfgPrefixLengths conf')
        , metaTotalAddrs = length (PM.leaves pfxs)
        , metaDidAutoStop = didAutoStop
        , metaPrefixCounts = fmap (second (HM.size . fst)) (cfgPrefixLengths conf' `zip` validPfxs)
        }

      -- Compute the structure function
      oneTau q = 
        let moms :: [(Double, Double)]
            moms = fmap (oneMoment conf' q) validPfxs
            
            n = fromIntegral (length moms)
            tauTilde = moms
              & fmap fst
              & VU.fromList
              & SS.mean
            sd = moms
              & fmap snd
              & treeFold (+) 0.0
              & ((/ n) . sqrt)
        in (q, tauTilde, sd)

      taus = qs & VU.fromList & VU.map oneTau

      -- Compute the other stuff if requested
      structureRows = if cfgStructure conf' then Just (VU.toList taus) else Nothing
      spectrumRows = if cfgSpectrum conf' then Just (computeSpectrumRows taus) else Nothing
      dimensionRows = if cfgDimensions conf' then Just (computeDimensionRows conf' taus pfxs) else Nothing
      partitionsRows = if cfgPartitions conf' then Just (computePartitions conf' pfxs) else Nothing

  -- Write output to csv files, std out, or json
  emitResults conf' metadata structureRows spectrumRows dimensionRows partitionsRows


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
 - Emit results in the requested output format.
 - Just dispatch based on csv or json.
 -}
emitResults :: Config
            -> Metadata
            -> Maybe [(Double, Double, Double)]
            -> Maybe [(Double, Double)]
            -> Maybe [(Double, Double)]
            -> Maybe [(Double, [(Double, Double)])]
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
               -> Maybe [(Double, Double, Double)]
               -> Maybe [(Double, Double)]
               -> Maybe [(Double, Double)]
               -> Maybe [(Double, [(Double, Double)])]
               -> IO ()
emitCsvResults conf metadata structureRows spectrumRows dimensionRows partitionsRows =
  if cfgOutPrefix conf == "-"
  then do
    maybe (return ()) (writeStructureCsv stdout) structureRows
    maybe (return ()) (writeSpectrumCsv stdout) spectrumRows
    maybe (return ()) (writeDimensionsCsv stdout) dimensionRows
    maybe (return ()) (writePartitionsCsv stdout) partitionsRows
  else do
    writeMetadata conf metadata
    maybe (return ()) (writeStructureFile conf) structureRows
    maybe (return ()) (writeSpectrumFile conf) spectrumRows
    maybe (return ()) (writeDimensionsFile conf) dimensionRows
    maybe (return ()) (writePartitionsFile conf) partitionsRows

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
    hPutStrLn hdl $ "did_auto_stop," ++ show (metaDidAutoStop metadata)
    forM_ (metaPrefixCounts metadata) $ \(pl, count) ->
      hPutStrLn hdl $ "prefix_count/" ++ show pl ++ "," ++ show count

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
 - Emit json results to stdout or file.
 -}
emitJsonResults :: Config
                -> Metadata
                -> Maybe [(Double, Double, Double)]
                -> Maybe [(Double, Double)]
                -> Maybe [(Double, Double)]
                -> Maybe [(Double, [(Double, Double)])]
                -> IO ()
emitJsonResults conf metadata structureRows spectrumRows dimensionRows partitionsRows = do
  let payload = encodeResultsJson metadata structureRows spectrumRows dimensionRows partitionsRows
  if cfgOutPrefix conf == "-"
  then BL8.putStrLn payload
  else do
    let outfile = cfgOutPrefix conf ++ ".json"
    hPutStrLn stderr $ "Writing json results to " ++ outfile
    BL8.writeFile outfile (payload <> "\n")

encodeResultsJson :: Metadata
                  -> Maybe [(Double, Double, Double)]
                  -> Maybe [(Double, Double)]
                  -> Maybe [(Double, Double)]
                  -> Maybe [(Double, [(Double, Double)])]
                  -> BL8.ByteString
encodeResultsJson metadata structureRows spectrumRows dimensionRows partitionsRows =
  encode $
    object $
      [ "schemaVersion" .= (1 :: Int)
      , "metadata" .= encodeMetadataJson metadata
      ]
      ++ maybe [] (\rows -> ["structure" .= encodeStructureRowsJson rows]) structureRows
      ++ maybe [] (\rows -> ["spectrum" .= encodeSpectrumRowsJson rows]) spectrumRows
      ++ maybe [] (\rows -> ["dimensions" .= encodeDimensionRowsJson rows]) dimensionRows
      ++ maybe [] (\rows -> ["partitions" .= encodePartitionsRowsJson rows]) partitionsRows

encodeMetadataJson :: Metadata -> Value
encodeMetadataJson metadata =
  object
    [ "input" .= metaInput metadata
    , "minPrefixLength" .= metaMinPrefixLength metadata
    , "maxPrefixLength" .= metaMaxPrefixLength metadata
    , "totalAddrs" .= metaTotalAddrs metadata
    , "didAutoStop" .= metaDidAutoStop metadata
    , "prefix_counts" .= encodePrefixCountsJson (metaPrefixCounts metadata)
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
    

