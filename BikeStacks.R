
library(tidymodels)
library(vroom)
library(rpart)
library(stacks)
library(glmnet)

bikes = vroom("BikeShare2/train.csv")
bikes_test = vroom("BikeShare2/test.csv")

bikes = bikes |> 
  select(-casual, -registered)

bike_recipe = recipe(count ~ ., bikes) |> 
  step_mutate(weather = ifelse(weather == 4, 3, weather)) |> 
  step_mutate(weather = as.factor(weather)) |> 
  step_time(datetime, features = "hour") |> 
  step_mutate(datetime_hour = as.factor(datetime_hour)) |> 
  step_mutate(season = as.factor(season)) |> 
  step_rm(datetime) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_normalize(all_numeric_predictors())

folds <- vfold_cv(bikes, v = 10, repeats = 1)

untunedModel <- control_stack_grid()
tunedModel <- control_stack_resamples()

preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) |> 
  set_engine("glmnet")

preg_wf <- workflow() |> 
  add_recipe(bike_recipe) |> 
  add_model(preg_model)

preg_tuning_grid <- grid_regular(penalty(),
                                 mixture(),
                                 levels = 5)

preg_models <- preg_wf |> 
  tune_grid(resamples = folds,
            grid = preg_tuning_grid,
            metrics = metric_set(rmse),
            control = untunedModel)

tree_model <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n=tune()) |> 
  set_engine("rpart") |> 
  set_mode("regression")

tree_wf <- workflow() |> 
  add_recipe(bike_recipe) |> 
  add_model(tree_model)

tree_tuning_grid <- grid_regular(tree_depth(),
                                 cost_complexity(),
                                 min_n(),
                                 levels = 5)

tree_models <- tree_wf |> 
  tune_grid(resamples = folds,
            grid = tree_tuning_grid,
            metrics = metric_set(rmse),
            control = untunedModel)

my_stack <- stacks() |> 
  add_candidates(preg_models) |> 
  add_candidates(tree_models)

stack_mod <- my_stack |> 
  blend_predictions() |> 
  fit_members()

stack_mod |>  predict(new_data = bikes_test) -> stack_preds

#stack_preds = max(stack_preds, 0)

kaggle_submission = stack_preds |> 
  bind_cols(bikes_test) |> 
  select(datetime, .pred) |> 
  mutate(count=pmax(0, count)) |> 
  rename(count=.pred) |> 
  mutate(datetime = as.character(format(datetime)))

vroom_write(x = kaggle_submission,
            file = "./BikeShare2/StackPreds.csv",
            delim = ",")


library(tidyverse)
library(vroom)
df = vroom("./BikeShare2/StackPreds.csv")
df = df |> mutate(count = pmax(count, 0))
vroom_write(x = df,
            file = "./BikeShare2/StackPreds2.csv",
            delim = ",")

