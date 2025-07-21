{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Control.Concurrent (newChan, readChan, takeMVar)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad (forever)
import Data.Aeson (FromJSON, decodeFileStrict)
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashTable.IO as HT
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client (ClientM, BaseUrl (BaseUrl), Scheme (Http), mkClientEnv, runClientM)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure)
import System.Posix (exitImmediately)
import System.Posix.Signals (Handler(Catch), sigINT, installHandler)

import BackendClient
import Ids
import SqlGeneration
import Utils
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import System.Timeout (timeout)

type HashMap k v = HT.BasicHashTable k v

newtype IngredientName = IngredientName { value :: Text }
  deriving (Eq, Show)

data Ingredient = Ingredient
  { id :: IngredientId
  , name :: IngredientName
  } deriving Show

dbIngredientFromIngredient :: Ingredient -> DbIngredient
dbIngredientFromIngredient Ingredient{id, name} =
  DbIngredient{id, ownerId = Nothing, name = name.value, isPublished = True}

data InputRecipe = InputRecipe
  { name :: Text
  , url :: Text
  , ingredients :: [Text]
  } deriving (Generic, FromJSON, Show)

data Recipe = Recipe
  { name :: Text
  , sourceLink :: Text
  , ingredients :: [Ingredient]
  } deriving (Show)

trustedThreshold :: Int
trustedThreshold = 97

fetchIngredientWithName :: Text -> ClientM (Maybe (Int, Ingredient))
fetchIngredientWithName name = do
  getFirstJust $ map go [trustedThreshold, 1]
  where
    go threshold = do
      resp <- queryIngredientsWithThreshold name threshold
      pure $ case resp.results of
        []    -> Nothing
        (x:_) -> Just
          ( threshold
          , Ingredient {id = x.id, name = IngredientName x.name}
          )

data FetchIngredientResult
  = FetchSuccess      Ingredient
  | FetchNotSure Text Ingredient
  | FetchFailure Text
  deriving Show

data FetchRecipeResult = FetchRecipeResult
  { name        :: Text
  , sourceLink  :: Text
  , ingredients :: [FetchIngredientResult]
  }

fetchRecipe :: HashMap Text (Maybe Ingredient) -> InputRecipe -> ClientM FetchRecipeResult
fetchRecipe ingredientsMap recipe = do
  let recipeIngredientNames = map T.toLower recipe.ingredients
  ingredients <- traverse fetchIngredient recipeIngredientNames
  pure FetchRecipeResult
    { name        = recipe.name
    , sourceLink  = recipe.url
    , ingredients = ingredients
    }
  where
    fetchIngredient :: Text -> ClientM FetchIngredientResult
    fetchIngredient ingredientName = do
      liftIO (HT.lookup ingredientsMap ingredientName) >>= \case
        Just (Just ingredient) -> pure $ FetchSuccess ingredient
        Just Nothing           -> pure $ FetchFailure ingredientName
        Nothing ->
          fetchIngredientWithName ingredientName <&> \case
            Just (threshold, ingredient)
              | threshold >= trustedThreshold -> FetchSuccess                ingredient
              | otherwise                     -> FetchNotSure ingredientName ingredient
            Nothing                           -> FetchFailure ingredientName

collectRecipe :: HashMap Text (Maybe Ingredient) -> FetchRecipeResult -> IO (Either String Recipe)
collectRecipe ingredientsMap fetchRecipeResult
  = fmap (fmap toRecipe)
  $ runExceptT
  $ traverse getIngredient fetchRecipeResult.ingredients
  where
    toRecipe ingredients = Recipe
      { name        = fetchRecipeResult.name
      , sourceLink  = fetchRecipeResult.sourceLink
      , ingredients = ingredients
      }

    getIngredient :: FetchIngredientResult -> (ExceptT String IO) Ingredient
    getIngredient = \case
      FetchSuccess                ingredient -> pure ingredient
      FetchNotSure ingredientName ingredient -> do
        liftIO (HT.lookup ingredientsMap ingredientName) >>= \case
          Just (Just lookedUpIngredient) -> pure lookedUpIngredient

          Just Nothing -> throwError $ "Skipping recipe '" <> T.unpack fetchRecipeResult.name <> "'"
          Nothing -> do
            liftIO $ T.putStr $ T.unlines
              [ quote ingredientName <> " from " <> quote fetchRecipeResult.name
              , quote ingredient.name.value <> " from database"
              , "But I am not sure if it is the ingredient. Is it? y/n (default: n)"
              ]
            liftIO getLine >>= \case
              ('y':_) -> do
                liftIO $ HT.insert ingredientsMap ingredientName (Just ingredient)
                pure ingredient
              _       -> do
                liftIO $ HT.insert ingredientsMap ingredientName Nothing
                throwError $ "Skipping recipe '" <> T.unpack fetchRecipeResult.name <> "'"
      FetchFailure ingredientName ->
        throwError $ "Ingredient '" <> T.unpack ingredientName <> "' not found"

publicRecipeToDb :: RecipeId -> Recipe -> (DbRecipe, [DbRecipeIngredient], [DbIngredient])
publicRecipeToDb recipeId Recipe{name, sourceLink, ingredients} =
  ( DbRecipe{id = recipeId, name, creatorId = Nothing, isPublished = True, sourceLink = Just sourceLink}
  , DbRecipeIngredient recipeId <$> map (.id) ingredients
  , map dbIngredientFromIngredient ingredients
  )

recipesWithIdsToInsertStatements :: [(RecipeId, Recipe)] -> Text
recipesWithIdsToInsertStatements recipesWithIds
  =  insertDbIngredients
  <> "\n\n"
  <> insertDbRecipes
  <> "\n\n"
  <> insertDbRecipeIngredients
  where
    dbRecipesWithIngredients = map (uncurry publicRecipeToDb) recipesWithIds

    insertDbIngredientsValueTuples       = nub $
      concatMap (map       dbIngredientToInsertValuesTuple . \(_,_,с) -> с) dbRecipesWithIngredients
    insertDbRecipesValueTuples           =
            map (              dbRecipeToInsertValuesTuple . \(a,_,_) -> a) dbRecipesWithIngredients
    insertDbRecipeIngredientsValueTuples =
      concatMap (map dbRecipeIngredientToInsertValuesTuple . \(_,b,_) -> b) dbRecipesWithIngredients

    insertDbIngredients = dbIngredientInsertBegining
                      <> T.intercalate ",\n" insertDbIngredientsValueTuples
                      <> "\nON CONFLICT DO NOTHING;"
    insertDbRecipes = dbRecipeInsertBegining
                   <> T.intercalate ",\n" insertDbRecipesValueTuples
                   <> "\nON CONFLICT DO NOTHING;"
    insertDbRecipeIngredients = dbRecipeIngredientInsertBegining
                             <> T.intercalate ",\n" insertDbRecipeIngredientsValueTuples
                             <> "\nON CONFLICT DO NOTHING;"

recipesToInsertStatements :: [Recipe] -> IO Text
recipesToInsertStatements
  = fmap recipesWithIdsToInsertStatements
  . traverse (\recipe -> (,recipe) <$> nextRandom)

outputRecipes :: IORef [Either String Recipe] -> IO ()
outputRecipes recipesRef = do
  recipes <- mapMaybe rightToMaybe <$> readIORef recipesRef
  finalResult <- recipesToInsertStatements recipes
  T.putStrLn finalResult
  putStrLn "Writing all of that to 'init_db.sql'"
  T.writeFile "init_db.sql" finalResult

parseArgs :: IO (String, Int, String, String)
parseArgs =
  getArgs >>= \case
    [host, portStr, recipesFilename, ingredientsFilename] -> do
      port <- readIO portStr
      pure (host, port, recipesFilename, ingredientsFilename)
    _ -> fail "Expected arguments: <host> <port> <recipes-filename> <ingredients-filename>"

main :: IO ()
main = do
  recipesRef <- newIORef []
  recipesChan <- newChan

  installHandler sigINT (Catch $ outputRecipes recipesRef >> exitImmediately ExitSuccess) Nothing

  (host, port, recipesFilename, ingredientsFilename) <- parseArgs

  parsedRecipes <- decodeFileStrict recipesFilename >>= \case
    Nothing -> do
      putStrLn $ "Failed to parse '" <> recipesFilename <> "'"
      exitFailure
    Just a -> pure a
  putStrLn $ show (length parsedRecipes) <> " recipes found"

  parsedIngredients :: [Text] <- decodeFileStrict ingredientsFilename >>= \case
    Nothing -> do
      putStrLn $ "Failed to parse '" <> ingredientsFilename <> "'"
      exitFailure
    Just a -> pure a
  putStrLn $ show (length parsedIngredients) <> " ingredients found"

  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (BaseUrl Http host port "")

  let ioCreateIngredient name = createPublicIngredient name `runClientM` clientEnv >>= \case
        Left err -> fail $ show err
        Right _ -> pure ()
  mapConcurrently_ ioCreateIngredient parsedIngredients

  ingredientsMap <- HT.new

  fetchingThread <- forkThread $
    traverseAndPushInto recipesChan (fetchRecipe ingredientsMap) parsedRecipes `runClientM` clientEnv >>= \case
      Left err -> fail $ show err
      Right () -> pure ()

  collectingThread <- forkThread $ forever $ do
    res <- timeout 10_000_000 (readChan recipesChan) >>= \case -- 10 seconds
      Nothing -> outputRecipes recipesRef >> exitImmediately ExitSuccess
      Just recipe -> collectRecipe ingredientsMap recipe
    modifyIORef' recipesRef (res:)

  takeMVar fetchingThread
  takeMVar collectingThread

  outputRecipes recipesRef
