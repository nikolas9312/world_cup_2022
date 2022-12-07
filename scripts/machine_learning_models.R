## Models


# Load packages
library(tidyverse)
library(showtext)
library(janitor)
library(tidymodels)
library(lubridate)
library(fastDummies)
library(ggplot2)
library(zoo) # moving averages
library(readxl)
library(stringr)
library(viridis)
library(hrbrthemes)
library(gridExtra)
library(corrplot)
tidymodels_prefer()
library(summarytools)
library(vembedr)
library(ggridges)
library(ggthemes)


# Read cleaned data
results <- read_csv("data/results_cleaned.csv")


# Splitting data
#-----------------------------------------------------

# Setting seed
set.seed(1993)

# Perform initial split, stratifying by the outcome variable with a proportion
# of 80% in the training
results_split <- initial_split(results, strata = "home_result" , prop = 0.8)

# Create training sample
results_train <- training(results_split)

# Create testing sample
results_test <- testing(results_split)

# Perform 5-folds cross-validation
results_fold <- vfold_cv(results_train, v = 5 , strata = "home_result")


# Transform data to implement model
#-----------------------------------------------------

# Convert to factors categorical predictors and outcome
results <- results  %>%
  mutate(home_result=factor(home_result) ,
         neutral=factor(neutral) , 
         tournament=factor(tournament) , 
         year = factor(year)) 



# Create recipe
#-----------------------------------------------------
abalone_recipe <- recipe(age ~ . , data = abalone_train) %>% 
  step_rm(rings) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ starts_with("type"):shucked_weight +
                  longest_shell:diameter + 
                  shucked_weight:shell_weight) %>% 
  step_normalize(all_predictors())









# Linear regression




