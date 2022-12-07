# Exploratory data analysis


# Settings
#----------------------------------------------------------------------

# Load packages
library(tidyverse)
library(worldfootballR)
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
tidymodels_prefer()

# Open cleaned data
games <- read_csv("data/results_cleaned.csv")
  

# Creating a bar chart of the distribution of results
games %>% 
  filter(neutral == "TRUE") %>% 
  ggplot(aes(y=forcats::fct_infreq(away_result) , fill=away_result)) +
  geom_bar() +
  labs(title="Home team match results", y="Result" , x="Number") +
  theme_minimal() + theme(legend.position="none") +
  scale_fill_viridis_d() 


# Open cleaned data
games <- read_csv("data/results_cleaned.csv") %>% 
  mutate( goal_diff_ha = home_score - away_score ,
          rf_diff_ha = ranking_fifa_home - ranking_fifa_away , 
          ov_diff_ha = overall_home - overall_away)

games %>%
  ggplot(aes(rf_diff_ha , goal_diff_ha)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method='lm', formula= y ~ splines::bs(x, 3) , color = "blue")

games %>%
  ggplot(aes(goal_diff_ha, ov_diff_ha)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method='lm', formula= splines::bs(x, 3) , color = "blue")

?geom_smooth

  geom_line(aes(y = .pred), data = regression_lines, color = "blue") +
  geom_line(aes(y = .pred_lower), data = regression_lines, 
            linetype = "dashed", color = "blue") +
  geom_line(aes(y = .pred_upper), data = regression_lines, 
            linetype = "dashed", color = "blue")

  
  ```{r}
  results %>% 
    group_by(neutral , home_result) %>% 
    summarize(win = mean(home_result_win) , draw = mean(home_result_draw) , lose = mean(home_result_loss)) %>%
    ggplot(aes(y=forcats::fct_infreq(home_result) , fill=neutral)) +
    geom_bar(position="dodge") +
    labs(title="Home team match results", y="Result" , x="Number") +
    theme_minimal() + theme(legend.position="none") +
    scale_fill_viridis_d() 
  ```
  
  create a dataset specie \<- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) ) condition \<- rep(c("normal" , "stress" , "Nitrogen") , 4) value \<- abs(rnorm(12 , 0 , 15)) data \<- data.frame(specie,condition,value)
  
  Grouped ggplot(data, aes(fill=condition, y=value, x=specie)) + geom_bar(position="dodge", stat="identity")
  
  results %\>% ggplot(aes(x=attack, group=neutral, fill=type_1)) + geom_density(adjust=1.5, alpha=.4) + theme_ipsum() + labs(title="Distribution of attack by type", fill= "Type")
  
  #rf_diff_ha = ranking_fifa_home - ranking_fifa_away , \# ov_diff_ha = overall_home - overall_away
  
  # Creating a bar chart of the distribution of results
  
