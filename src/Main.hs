{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (FromJSON, decodeFileStrict)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.UUID (UUID, fromLazyASCIIBytes)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Servant.API ((:>), Get, JSON, QueryParam, PlainText, MimeUnrender (mimeUnrender))
import Servant.Client
import System.Environment (getArgs)
import System.Posix.Signals
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import System.Exit (ExitCode (..))
import System.Posix (exitImmediately)
import Data.Maybe (mapMaybe)
import Data.List (nub)

tshow :: Show a => a -> Text
tshow = T.pack . show

quote :: Text -> Text
quote = T.cons '\'' . flip T.snoc '\''

newtype IngredientName = IngredientName { value :: Text }
  deriving Eq

data Ingredient = Ingredient
  { id :: IngredientId
  , name :: IngredientName
  }

instance MimeUnrender PlainText UUID where
  mimeUnrender _
    = maybe (Left "Failed to parse UUID") Right
    . fromLazyASCIIBytes

data InputRecipe = Recipe
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

data Recipe = CreateRecipeReqBody
  { name :: Text
  , sourceLink :: Text
  , ingredients :: [Ingredient]
  }

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

fetchIngredientWithName :: Text -> ClientM (Maybe (Int, Ingredient))
fetchIngredientWithName name = do
  liftIO $ putStrLn $ "Getting id for '" <> T.unpack name <> "'"
  getFirstJust $ map go [100, 99 .. 0]
  where
    go threshold = do
      resp <- queryIngredientsWithThreshold name threshold
      pure $ case resp.results of
        []    -> Nothing
        (x:_) -> Just
          ( threshold
          , Ingredient
            { id = x.id
            , name = IngredientName x.name
            }
          )

getFirstJust :: Monad m => [m (Maybe a)] -> m (Maybe a)
getFirstJust [] = pure Nothing
getFirstJust (x:xs) = x >>= \case
  Nothing -> getFirstJust xs
  Just b  -> pure (Just b)

trustedThreshold :: Int
trustedThreshold = 97

processRecipe :: InputRecipe -> ClientM (Either String Recipe)
processRecipe recipe = do
  let recipeIngredientNames = map T.toLower recipe.ingredients
  ingredientsResults <- traverse getIngredient recipeIngredientNames
  forM (sequenceA ingredientsResults) $ \ingredients ->
    pure CreateRecipeReqBody
      { name        = recipe.name
      , sourceLink  = recipe.url
      , ingredients = ingredients
      }
  where
    getIngredient :: Text -> ClientM (Either String Ingredient)
    getIngredient ingredientName =
      fetchIngredientWithName ingredientName >>= \case
        Nothing -> pure $ Left $ "Ingredient '" <> T.unpack ingredientName <> "' not found"
        Just (threshold, ingredient) -> do
          if threshold >= trustedThreshold then
            return $ Right ingredient
          else do
            liftIO $ do
              putStrLn $ "Matched with ingredient: '"
                      <> T.unpack ingredient.name.value
                      <> "', but I am not sure if it is the ingredient"
              putStrLn "Is it? y/n (default: n)"
              getLine >>= \case
                ('y':_) -> return $ Right ingredient
                _ -> do
                  let err = "Skipping recipe '" <> T.unpack recipe.name <> "'"
                  putStrLn err
                  return $ Left err

data DbRecipe = DbRecipe
  { id :: RecipeId
  , name :: Text
  , creatorId :: Maybe UserId
  , isPublished :: Bool
  , sourceLink :: Maybe Text
  }

dbRecipeInsertBegining :: Text
dbRecipeInsertBegining =
  "INSERT INTO recipes (id, name, creator_id, is_published, source_link) VALUES \n"

dbRecipeToInsertValuesTuple :: DbRecipe -> Text
dbRecipeToInsertValuesTuple DbRecipe{id, name, creatorId, isPublished, sourceLink}
  =  "("
  <>              (quote . tshow)         id <> ", "
  <>               quote                name <> ", "
  <> maybe "null" (quote . tshow)  creatorId <> ", "
  <>                       tshow isPublished <> ", "
  <> maybe "null"  quote          sourceLink
  <> ")"

data DbRecipeIngredient = DbRecipeIngredient
  { recipeId :: RecipeId
  , ingredientId :: IngredientId
  }

dbRecipeIngredientInsertBegining :: Text
dbRecipeIngredientInsertBegining =
  "INSERT INTO recipe_ingredients (recipe_id, ingredient_id) VALUES \n"

dbRecipeIngredientToInsertValuesTuple :: DbRecipeIngredient -> Text
dbRecipeIngredientToInsertValuesTuple DbRecipeIngredient{recipeId, ingredientId}
  =  "("
  <> quote (tshow     recipeId) <> ", "
  <> quote (tshow ingredientId)
  <> ")"

data DbIngredient = DbIngredient
  { id :: IngredientId
  , ownerId :: Maybe UserId
  , name :: IngredientName
  , isPublished :: Bool
  } deriving Eq

dbIngredientFromIngredient :: Ingredient -> DbIngredient
dbIngredientFromIngredient Ingredient {id, name} =
  DbIngredient{id, ownerId = Nothing, name, isPublished = True}

dbIngredientInsertBegining :: Text
dbIngredientInsertBegining =
  "INSERT INTO ingredients (id, owner_id, name, is_published) VALUES \n"

dbIngredientToInsertValuesTuple :: DbIngredient -> Text
dbIngredientToInsertValuesTuple DbIngredient{id, ownerId, name, isPublished}
  =  "("
  <> quote (tshow          id) <> ", "
  <> quote (tshow     ownerId) <> ", "
  <> quote         name.value  <> ", "
  <>        tshow isPublished
  <> ")"

publicRecipeToDb :: RecipeId -> Recipe -> (DbRecipe, [DbRecipeIngredient], [DbIngredient])
publicRecipeToDb recipeId CreateRecipeReqBody{name, sourceLink, ingredients} =
  ( DbRecipe{id = recipeId, name, creatorId = Nothing, isPublished = True, sourceLink = Just sourceLink}
  , DbRecipeIngredient recipeId <$> map (.id) ingredients
  , map dbIngredientFromIngredient ingredients
  )

parseArgs :: IO (String, Int, String)
parseArgs =
  getArgs >>= \case
    (host : portStr : filename : _) -> do
      port <- readIO portStr
      pure (host, port, filename)
    _ -> fail "Expected arguments: <host> <port> <filename>"

traverseAndAccumulateInto :: MonadIO m => IORef [b] -> (a -> m b) -> [a] -> m ()
traverseAndAccumulateInto ref f as =
  forM_ as $ \a -> do
    b <- f a
    liftIO $ modifyIORef' ref (b:)

ingredientsToInsertStatement :: [Ingredient] -> Text
ingredientsToInsertStatement = undefined

recipesWithIdsToInsertStatements :: [(RecipeId, Recipe)] -> Text
recipesWithIdsToInsertStatements recipesWithIds
  =  insertDbIngredients
  <> "\n\n"
  <> insertDbRecipes
  <> "\n\n"
  <> insertDbRecipeIngredients
  where
    dbRecipesWithIngredients = map (uncurry publicRecipeToDb) recipesWithIds

    dbIngredients       = nub $ concatMap (\(_,_,с) -> с) dbRecipesWithIngredients
    dbRecipes           =             map (\(a,_,_) -> a) dbRecipesWithIngredients
    dbRecipeIngredients =       concatMap (\(_,b,_) -> b) dbRecipesWithIngredients

    insertDbIngredientsValueTuples       = map       dbIngredientToInsertValuesTuple dbIngredients
    insertDbRecipesValueTuples           = map           dbRecipeToInsertValuesTuple dbRecipes
    insertDbRecipeIngredientsValueTuples = map dbRecipeIngredientToInsertValuesTuple dbRecipeIngredients

    insertDbIngredients
      =  dbIngredientInsertBegining
      <> T.intercalate ",\n" insertDbIngredientsValueTuples
      <> ";"
    insertDbRecipes
      =  dbRecipeInsertBegining
      <> T.intercalate ",\n" insertDbRecipesValueTuples
      <> ";"
    insertDbRecipeIngredients
      = dbRecipeIngredientInsertBegining
      <> T.intercalate ",\n" insertDbRecipeIngredientsValueTuples
      <> ";"

recipesToInsertStatements :: [Recipe] -> IO Text
recipesToInsertStatements
  = fmap recipesWithIdsToInsertStatements
  . traverse (\recipe -> (,recipe) <$> nextRandom)

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left  _) = Nothing
rightToMaybe (Right b) = Just b

outputRecipes :: IORef [Either String Recipe] -> IO ()
outputRecipes recipesRef = do
  recipes <- mapMaybe rightToMaybe <$> readIORef recipesRef
  finalResult <- recipesToInsertStatements recipes
  T.putStrLn finalResult
  putStrLn "Writing all of that to 'init_db.sql'"
  T.writeFile "init_db.sql" finalResult

main :: IO ()
main = do
  recipesRef <- newIORef []

  installHandler sigINT (Catch $ outputRecipes recipesRef >> exitImmediately ExitSuccess) Nothing

  (host, port, filename) <- parseArgs

  parsedRecipes <- concat <$> decodeFileStrict filename
  putStrLn $ show (length parsedRecipes) <> " recipes found"

  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (BaseUrl Http host port "")
  traverseAndAccumulateInto recipesRef processRecipe parsedRecipes `runClientM` clientEnv
  outputRecipes recipesRef
