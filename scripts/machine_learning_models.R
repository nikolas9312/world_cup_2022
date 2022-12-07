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
library(klaR) # for naive bayes
library(discrim)


# Read and transform data to implement model
#-----------------------------------------------------

# Read cleaned data
results <- read_csv("data/results_cleaned.csv")

# List of continental tournaments
continental_tournaments <- c("AFC Asian Cup" ,
                             "African Cup of Nations" ,
                             "UEFA Euro qualification" ,
                             "Gold Cup qualification" ,
                             "Copa América" ,
                             "African Cup of Nations qualification" ,
                             "Gold Cup" ,
                             "Oceania Nations Cup qualification" , 
                             "Oceania Nations Cup" , 
                             "UEFA Euro" , 
                             "Confederations Cup" ,
                             "UEFA Nations League" , 
                             "CONCACAF Nations League" ,
                             "Confederations Cup")

# Convert to factors categorical predictors and outcome
results <- results  %>%
  mutate(home_result=factor(home_result) , 
         neutral=factor(neutral) , 
         friendly = ifelse(tournament =="Friendly" , 1 , 0) , # Create dummies for friendly matches
         world_cup = ifelse(tournament =="FIFA World Cup" , 1 , 0), # Create for WC  matches
         continental_tr = ifelse(tournament %in% continental_tournaments, 1 , 0) , # Create Tournaments
         qualifier = ifelse(tournament =="FIFA World Cup qualification" , 1 , 0), # Dummy for qualifier matches
         year = factor(year)) %>% # Dummy code years
select(-tournament) %>% # Remove the tournament variable
  filter(!is.na(m30_sh_win_dha)) # Filter missing values for countries that are recent adds to FIFA 

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

# Create recipe
#-----------------------------------------------------

# Include all predictors , no interactions
recipe_basic <- recipe(home_result ~ . , data = results_train) %>% 
  step_rm(score_dha , date , home_team , away_team) %>% # Remove score difference as it is colinear with the result
  step_dummy(all_nominal_predictors()) %>% # Transform to dummy all categorical predictors
  step_normalize(all_nominal_predictors()) %>% # Center and scale nominal predictors
  step_impute_linear(overall_dha, impute_with = imp_vars(ranking_fifa_dha , starts_with("year")))

# Fit with elastic net
#-----------------------------------------------------

# Specify a logistic regression model for classification
elastic_net_spec <- multinom_reg(penalty = tune(), mixture = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet")

# create a workflow
en_workflow <- workflow() %>% 
  add_model(elastic_net_spec) %>% 
  add_recipe(recipe_basic)

# Set grids for penalty mixture
en_grid <- grid_regular(penalty(range = c(-5, 5)), 
                        mixture(range = c(0, 1)), levels = 10)
# Tune the model
tune_res <- tune_grid(
  en_workflow,
  resamples =results_fold, 
  grid = en_grid
)

# Save results as an R object to not run it again when knitting
saveRDS(tune_res, file="model_results/tune_elastic_net_basic.RData")

# Plot
autoplot(tune_res)

# Select best model
best_model <- select_best(tune_res, metric = "roc_auc")

# Select best model
en_final <- finalize_workflow(en_workflow, best_model)

en_final_fit <- fit(en_final, data = results_train)

predicted_data <- augment(en_final_fit, new_data = results_test) %>% 
  select(home_result, starts_with(".pred"))

# Overall ROC
predicted_data %>% roc_auc(home_result, .pred_draw:.pred_win)

# Plotted curves
predicted_data %>% roc_curve(home_result, .pred_draw:.pred_win) %>% 
  autoplot()

# Confusion matrix (this cannot predict draws)
predicted_data %>% 
  conf_mat(truth = home_result, estimate = .pred_class) %>%
  autoplot(type = "heatmap")
































# Fit the model
fit_log_basic <- fit(log_wkflow, results_train)

# Save results as an R object to not run it again when knitting
saveRDS(fit_log_basic, file="model_results/fit_log_basic.RData")

# Fit with LDA
#-----------------------------------------------------

# Specify a LDA model for classification
lda <- discrim_linear() %>% 
  set_engine("MASS") %>% 
  set_mode("classification")

# create a workflow
lda_wkflow <- workflow() %>% 
  add_model(lda) %>% 
  add_recipe(recipe_basic)

# Fit the model
fit_lda_basic <- fit(lda_wkflow, results_train)

# Save results as an R object to not run it again when knitting
saveRDS(fit_lda_basic, file="model_results/fit_lda_basic.RData")

# Fit with QDA
#-----------------------------------------------------

# Specify a QDA model for classification
qda <- discrim_quad() %>% 
  set_engine("MASS") %>% 
  set_mode("classification")

# create a workflow
qda_wkflow <- workflow() %>% 
  add_model(qda) %>% 
  add_recipe(recipe_basic)

# Fit the model
fit_qda_basic <- fit(qda_wkflow, results_train)

# Save results as an R object to not run it again when knitting
saveRDS(fit_qda_basic, file="model_results/fit_qda_basic.RData")

# Fit with Naive Bayes
#-----------------------------------------------------
nb_mod <- naive_Bayes() %>% 
  set_mode("classification") %>% 
  set_engine("klaR") %>% 
  set_args(usekernel = FALSE) 

nb_wkflow <- workflow() %>% 
  add_model(nb_mod) %>% 
  add_recipe(recipe_basic)

fit_nb_basic <- fit(nb_wkflow, results_train)

# Save results as an R object to not run it again when knitting
saveRDS(fit_nb_basic, file="model_results/fit_nb_basic.RData")



# Comparing models

acc_log_basic <- predict(fit_log_basic, new_data = results_train, type = "class") %>% 
  bind_cols(results_train %>% select(home_result)) 


%>% 
  accuracy(truth = results_train, estimate = .pred_class)


lda_acc <- predict(lda_fit, new_data = titanic_train, type = "class") %>% 
  bind_cols(titanic_train %>% select(survived)) %>% 
  accuracy(truth = survived, estimate = .pred_class)
qda_acc <- predict(qda_fit, new_data = titanic_train, type = "class") %>% 
  bind_cols(titanic_train %>% select(survived)) %>% 
  accuracy(truth = survived, estimate = .pred_class)
nb_acc <- predict(nb_fit, new_data = titanic_train, type = "class") %>% 
  bind_cols(titanic_train %>% select(survived)) %>% 
  accuracy(truth = survived, estimate = .pred_class)

results <- bind_rows(log_acc, lda_acc, qda_acc, nb_acc) %>% 
  tibble() %>% mutate(model = c("Logistic", "LDA", "QDA", "NB")) %>% 
  select(model, .estimate) %>% 
  arrange(.estimate)

results


inter_interact(terms = ~ starts_with("type"):shucked_weight +
                longest_shell:diameter + 
                shucked_weight:shell_weight) %>% 




?starts_with


# Linear regression




