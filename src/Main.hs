{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Control.Concurrent (newChan, readChan, takeMVar)
import Control.Monad (forever)
import Data.Aeson (FromJSON, decodeFileStrict)
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client (ClientM, BaseUrl (BaseUrl), Scheme (Http), mkClientEnv, runClientM)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.Posix (exitImmediately)
import System.Posix.Signals

import BackendClient
import Ids
import SqlGeneration
import Utils

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
  }

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

fetchRecipe :: InputRecipe -> ClientM FetchRecipeResult
fetchRecipe recipe = do
  let recipeIngredientNames = map T.toLower recipe.ingredients
  ingredients <- traverse fetchIngredient recipeIngredientNames
  pure FetchRecipeResult
    { name        = recipe.name
    , sourceLink  = recipe.url
    , ingredients = ingredients
    }
  where
    fetchIngredient :: Text -> ClientM FetchIngredientResult
    fetchIngredient ingredientName =
      fetchIngredientWithName ingredientName <&> \case
        Just (threshold, ingredient)
          | threshold >= trustedThreshold -> FetchSuccess                ingredient
          | otherwise                     -> FetchNotSure ingredientName ingredient
        Nothing                           -> FetchFailure ingredientName

collectRecipe :: FetchRecipeResult -> IO (Either String Recipe)
collectRecipe fetchRecipeResult =
  fmap toRecipe . sequenceA <$> traverse getIngredient fetchRecipeResult.ingredients
  where
    toRecipe ingredients = Recipe
      { name       = fetchRecipeResult.name
      , sourceLink = fetchRecipeResult.sourceLink
      , ingredients= ingredients
      }

    getIngredient :: FetchIngredientResult -> IO (Either String Ingredient)
    getIngredient = \case
      FetchSuccess                ingredient ->
        pure $ Right ingredient
      FetchNotSure ingredientName ingredient -> do
        T.putStrLn $ "'" <> ingredientName <> "' matched with ingredient: '"
                  <> ingredient.name.value <> "', but I am not sure if it is the ingredient"
        T.putStrLn "Is it? y/n (default: n)"
        getLine >>= \case
          ('y':_) -> return $ Right ingredient
          _ -> do
            let err = "Skipping recipe '" <> T.unpack fetchRecipeResult.name <> "'"
            putStrLn err
            return $ Left err
      FetchFailure ingredientName ->
        pure $ Left $ "Ingredient '" <> T.unpack ingredientName <> "' not found"

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
                      <> ";"
    insertDbRecipes = dbRecipeInsertBegining
                   <> T.intercalate ",\n" insertDbRecipesValueTuples
                   <> ";"
    insertDbRecipeIngredients = dbRecipeIngredientInsertBegining
                             <> T.intercalate ",\n" insertDbRecipeIngredientsValueTuples
                             <> ";"

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

parseArgs :: IO (String, Int, String)
parseArgs =
  getArgs >>= \case
    (host : portStr : filename : _) -> do
      port <- readIO portStr
      pure (host, port, filename)
    _ -> fail "Expected arguments: <host> <port> <filename>"

main :: IO ()
main = do
  recipesRef <- newIORef []
  recipesChan <- newChan

  installHandler sigINT (Catch $ outputRecipes recipesRef >> exitImmediately ExitSuccess) Nothing

  (host, port, filename) <- parseArgs

  parsedRecipes <- concat <$> decodeFileStrict filename
  putStrLn $ show (length parsedRecipes) <> " recipes found"

  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (BaseUrl Http host port "")

  fetchingThread <- forkThread $
    traverseAndPushInto recipesChan fetchRecipe parsedRecipes `runClientM` clientEnv >>= \case
      Left err -> fail $ show err
      Right () -> pure ()

  collectingThread <- forkThread $ forever $ do
    res <- readChan recipesChan >>= collectRecipe
    modifyIORef' recipesRef (res:)

  takeMVar fetchingThread
  takeMVar collectingThread

  outputRecipes recipesRef
