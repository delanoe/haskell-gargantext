{-# LANGUAGE TypeFamilies #-}
module Gargantext.Utils.Jobs (
  -- * Serving the JOBS API
    serveJobsAPI
  -- * Parsing and reading @GargJob@s from disk
  , readPrios
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (ToJSON)
import Data.Monoid (Dual)
import Prelude
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

import Gargantext.API.Admin.EnvTypes
import Gargantext.API.Prelude
import qualified Gargantext.Utils.Jobs.Internal as Internal
import Gargantext.Utils.Jobs.Map
import Gargantext.Utils.Jobs.Monad

import qualified Servant.Job.Async as SJ

jobErrorToGargError
  :: JobError -> GargError
jobErrorToGargError = GargJobError

serveJobsAPI
  :: (
     Foldable callbacks
   , Ord (JobType m)
   , Show (JobType m)
   , ToJSON (JobEventType m)
   , ToJSON (JobOutputType m)
   , MonadJobStatus m Dual
   , m ~ (GargM env GargError)
   )
  => JobType m
  -> (Internal.JobHandle -> input -> Logger (JobEventType m) -> m (JobOutputType m))
  -> SJ.AsyncJobsServerT' ctI ctO callbacks (JobEventType m) input (JobOutputType m) m
serveJobsAPI jobType f = Internal.serveJobsAPI ask jobType jobErrorToGargError $ \env jHandle i l -> do
  putStrLn ("Running job of type: " ++ show jobType)
  runExceptT $ runReaderT (f jHandle i l) env

parseGargJob :: String -> Maybe GargJob
parseGargJob s = case s of
  "tablengrams" -> Just TableNgramsJob
  "forgotpassword" -> Just ForgotPasswordJob
  "updatengramslistjson" -> Just UpdateNgramsListJobJSON
  "updatengramslistcsv" -> Just UpdateNgramsListJobCSV
  "addcontact" -> Just AddContactJob
  "addfile" -> Just AddFileJob
  "documentfromwritenode" -> Just DocumentFromWriteNodeJob
  "updatenode" -> Just UpdateNodeJob
  "updateframecalc" -> Just UploadFrameCalcJob
  "updatedocument" -> Just UploadDocumentJob
  "newnode" -> Just NewNodeJob
  "addcorpusquery" -> Just AddCorpusQueryJob
  "addcorpusform" -> Just AddCorpusFormJob
  "addcorpusfile" -> Just AddCorpusFileJob
  "addannuaireform" -> Just AddAnnuaireFormJob
  "recomputegraph" -> Just RecomputeGraphJob
  _ -> Nothing

parsePrios :: [String] -> IO [(GargJob, Int)]
parsePrios []       = return []
parsePrios (x : xs) = (:) <$> go x <*> parsePrios xs
  where go s = case break (=='=') s of
          ([], _) -> error "parsePrios: empty jobname?"
          (prop, valS)
            | Just val <- readMaybe (tail valS)
            , Just j   <- parseGargJob prop -> return (j, val)
            | otherwise                  -> error $
              "parsePrios: invalid input. " ++ show (prop, valS)

readPrios :: FilePath -> IO [(GargJob, Int)]
readPrios fp = do
  exists <- doesFileExist fp
  case exists of
    False -> do
      putStrLn $
        "Warning: " ++ fp ++ " doesn't exist, using default job priorities."
      return []
    True -> parsePrios . lines =<< readFile fp
