{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import System.Environment (getArgs)
import GHC.Generics (Generic)

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (FromJSON, decodeFileStrict, ToJSON)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.UUID (UUID, fromLazyASCIIBytes)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Data.Text as T
import Servant.API ((:>), Get, JSON, QueryParam, Capture, ReqBody, PlainText, MimeUnrender (mimeUnrender), Post)
import Servant.Client

instance MimeUnrender PlainText UUID where
  mimeUnrender _
    = maybe (Left "Failed to parse UUID") Right
    . fromLazyASCIIBytes

data Recipe = Recipe
  { name :: Text
  , url :: Text
  , ingredients :: [Text]
  } deriving (Generic, FromJSON, Show)

data IngredientResp = IngredientResp
  { id :: IngredientId
  , name :: Text
  } deriving (Generic, FromJSON)

data SearchResp a = SearchResp
  { results :: [a]
  , found :: Int
  } deriving (Generic, FromJSON)

type IngredientId = UUID
type RecipeId = UUID
type UserId = UUID

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

type GetIngredient
  =  "public" :> "ingredients"
  :> Capture "ingredient-id" IngredientId
  :> Get '[JSON] IngredientResp

getIngredient :: IngredientId -> ClientM IngredientResp
getIngredient = client (Proxy @GetIngredient)

data CreateRecipeReqBody = CreateRecipeReqBody
  { name :: Text
  , sourceLink :: Maybe Text
  , ingredients :: [IngredientId]
  } deriving (Generic, ToJSON)

type CreatePublicRecipe
  =  "admin" :> "recipes"
  :> ReqBody '[JSON] CreateRecipeReqBody
  :> Post '[PlainText] RecipeId

createPublicRecipe :: CreateRecipeReqBody -> ClientM RecipeId
createPublicRecipe = client (Proxy @CreatePublicRecipe)

data RecipeCreatorResp = RecipeCreatorResp
  { id :: UserId
  , fullName :: Text
  } deriving (Generic, FromJSON)

data RecipeSearchResp = RecipeSearchResp
  { recipeId :: RecipeId
  , name :: Text
  } deriving (Generic, FromJSON)

type SearchRecipes
  =  "public" :> "recipes"
  :> QueryParam "query" Text :> QueryParam "threshold" Int
  :> QueryParam "size" Int :> QueryParam "offset" Int
  :> Get '[JSON] (SearchResp RecipeSearchResp)

searchRecipes
  :: Maybe Text -> Maybe Int
  -> Maybe Int -> Maybe Int
  -> ClientM (SearchResp RecipeSearchResp)
searchRecipes = client (Proxy @SearchRecipes)

queryIngredientsWithThreshold :: Text -> Int -> ClientM (SearchResp IngredientResp)
queryIngredientsWithThreshold query threshold =
  searchIngredients (Just query) (Just threshold) Nothing Nothing

queryRecipesWithThreshold :: Text -> Int -> ClientM (SearchResp RecipeSearchResp)
queryRecipesWithThreshold query threshold =
  searchRecipes (Just query) (Just threshold) Nothing Nothing

fetchIngredientWithName :: Text -> ClientM (Maybe (Int, IngredientId))
fetchIngredientWithName name = do
  liftIO $ putStrLn $ "Getting id for '" <> T.unpack name <> "'"
  getFirstJust $ map go [100, 99 .. 0]
  where
    go threshold = do
      resp <- queryIngredientsWithThreshold name threshold
      pure $ case resp.results of
        []    -> Nothing
        (x:_) -> Just (threshold, x.id)

getFirstJust :: Monad m => [m (Maybe a)] -> m (Maybe a)
getFirstJust [] = pure Nothing
getFirstJust (x:xs) = x >>= \case
  Nothing -> getFirstJust xs
  Just b  -> pure (Just b)

trustedThreshold :: Int
trustedThreshold = 97

createRecipe :: Recipe -> ClientM (Either String RecipeId)
createRecipe recipe = do
  let ingredients = map T.toLower recipe.ingredients
  ingredientIdsResults <- traverse getIngredientId ingredients
  forM (sequenceA ingredientIdsResults) $ \ingredientIds ->
    createPublicRecipe CreateRecipeReqBody
      { name        = recipe.name
      , sourceLink  = Just recipe.url
      , ingredients = ingredientIds
      }
  where
    getIngredientId :: Text -> ClientM (Either String IngredientId)
    getIngredientId ingredientName =
      fetchIngredientWithName ingredientName >>= \case
        Nothing -> pure $ Left $ "Ingredient '" <> T.unpack ingredientName <> "' not found"
        Just (threshold, ingredientId) -> do
          if threshold >= trustedThreshold then
            return $ Right ingredientId
          else do
            ingredient <- getIngredient ingredientId
            liftIO $ do
              putStrLn $ "Matched with ingredient: '"
                      <> T.unpack ingredient.name
                      <> "', but I am not sure if it is the ingredient"
              putStrLn "Is it? y/n (default: n)"
              getLine >>= \case
                ('y':_) -> return $ Right ingredientId
                _ -> do
                  let err = "Skipping recipe '" <> T.unpack recipe.name <> "'"
                  putStrLn err
                  return $ Left err

createRecipe_ :: Recipe -> ClientM ()
createRecipe_ recipe = do
  results <- queryRecipesWithThreshold recipe.name 100
  if results.found == 0 then
    createRecipe recipe >>= liftIO . putStrLn . \case
      Left err       -> err
      Right recipeId -> "Recipe '" <> show recipeId <> "' created"
  else
    liftIO $ putStrLn $ "Recipe '" <> T.unpack recipe.name <> "' already exists"

parseArgs :: IO (String, Int, String)
parseArgs =
  getArgs >>= \case
    (host : portStr : filename : _) -> do
      port <- readIO portStr
      pure (host, port, filename)
    _ -> fail "Expected arguments: <host> <port> <filename>"

main :: IO ()
main = do
  (host, port, filename) <- parseArgs
  recipes :: [Recipe] <- concat <$> decodeFileStrict filename
  putStrLn $ show (length recipes) <> " recipes found"
  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (BaseUrl Http host port "")
  traverse_ createRecipe_ recipes `runClientM` clientEnv
    >>= either print pure
