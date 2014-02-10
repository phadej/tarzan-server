module Text.Tarzan.Application where

import Network.HTTP.Types.Status

import Network.Wai
import Network.Wai.Application.Static

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE

import qualified Filesystem.Path.CurrentOS as F

import Control.Applicative

import Text.Tarzan.Evaluator (execute)

static :: Application
static = staticApp (defaultWebAppSettings $ F.decodeString "static")

tarzanApplication :: Application
tarzanApplication req
  | pathInfo req == []              = static req { pathInfo = [ T.pack "index.html" ] }
  | pathInfo req == [T.pack "api"]  = tarzanApp req   
  | otherwise                       = static req      

tarzanApp :: Application
tarzanApp req = do 
  body <- LT.unpack . LE.decodeUtf8 <$> lazyRequestBody req
  return $ responseLBS status200 [] (LE.encodeUtf8 . LT.pack $ execute body)
