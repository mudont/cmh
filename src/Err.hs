module Err where

import ClassyPrelude
import Control.Monad.Except
import qualified Data.ByteString.Lazy as LBS
import Protolude (ConvertText (toS))
import Servant
import Text.Blaze
import Text.Blaze.Renderer.Pretty (renderMarkup)
import Types

format :: ToMarkup a => a -> LBS.ByteString
format err = fromString $ renderMarkup bsErr
  where
    bsErr = toMarkup err

appToErr :: ServerError -> Text -> ServerError
appToErr x msg =
  x
    { errBody = format errTmp,
      errHeaders = [("Content-Type", "text/html")]
    }
  where
    tmp = toS (errReasonPhrase x)
    errTmp = Err tmp msg

unauthorized :: (MonadError ServerError m) => Text -> m a
unauthorized = throwError . unauthorizedErr

unauthorizedErr :: Text -> ServerError
unauthorizedErr = appToErr err401

forbidden :: (MonadError ServerError m) => Text -> m a
forbidden = throwError . forbiddenErr

forbiddenErr :: Text -> ServerError
forbiddenErr = appToErr err403

notFound :: (MonadError ServerError m) => Text -> m a
notFound = throwError . notFoundErr

notFoundErr :: Text -> ServerError
notFoundErr = appToErr err404

preconditionFailed :: (MonadError ServerError m) => Text -> m a
preconditionFailed = throwError . preconditionFailedErr

preconditionFailedErr :: Text -> ServerError
preconditionFailedErr = appToErr err412

serverError :: (MonadError ServerError m) => Text -> m a
serverError = throwError . serverErrorErr

serverErrorErr :: Text -> ServerError
serverErrorErr = appToErr err500