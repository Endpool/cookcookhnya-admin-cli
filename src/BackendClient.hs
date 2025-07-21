{-# LANGUAGE DeriveAnyClass #-}

module BackendClient
  ( searchIngredients, queryIngredientsWithThreshold
  , SearchResp(..)
  , IngredientResp(..)
  ) where

import Data.Text (Text)
import Servant.API ((:>), Get, JSON, QueryParam)
import Servant.Client (ClientM, client)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

import Ids
import Data.Proxy (Proxy(..))

data IngredientResp = IngredientResp
  { id :: IngredientId
  , name :: Text
  } deriving (Generic, FromJSON)

data SearchResp a = SearchResp
  { results :: [a]
  , found :: Int
  } deriving (Generic, FromJSON)

type SearchIngredients
  =  "public" :> "ingredients"
  :> QueryParam "query" Text :> QueryParam "threshold" Int
  :> QueryParam "size" Int :> QueryParam "offset" Int
  :> Get '[JSON] (SearchResp IngredientResp)

searchIngredients
  :: Maybe Text -> Maybe Int
  -> Maybe Int -> Maybe Int
  -> ClientM (SearchResp IngredientResp)
searchIngredients = client (Proxy @SearchIngredients)

queryIngredientsWithThreshold :: Text -> Int -> ClientM (SearchResp IngredientResp)
queryIngredientsWithThreshold query threshold =
  searchIngredients (Just query) (Just threshold) Nothing Nothing


