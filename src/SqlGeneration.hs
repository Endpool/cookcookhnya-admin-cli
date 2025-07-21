{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module SqlGeneration where

import Ids
import Data.Text (Text)
import qualified Data.Text as T

tshow :: Show a => a -> Text
tshow = T.pack . show

quote :: Text -> Text
quote = T.cons '\'' . flip T.snoc '\''

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
  , name :: Text
  , isPublished :: Bool
  } deriving Eq

dbIngredientInsertBegining :: Text
dbIngredientInsertBegining =
  "INSERT INTO ingredients (id, owner_id, name, is_published) VALUES \n"

dbIngredientToInsertValuesTuple :: DbIngredient -> Text
dbIngredientToInsertValuesTuple DbIngredient{id, ownerId, name, isPublished}
  =  "("
  <>              (quote . tshow)         id <> ", "
  <> maybe "null" (quote . tshow)    ownerId <> ", "
  <>               quote                name <> ", "
  <>                       tshow isPublished
  <> ")"

