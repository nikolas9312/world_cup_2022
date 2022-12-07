#----------------------------------------------------------------------
# Data exploration
# World cup winner prediction : ML project
# Nicolas Herrera
#----------------------------------------------------------------------


#Ideas: i need to predict win tie loss based on the stats of both teams. 
# Predict goal difference for match to classify to the second round

# Predict next rounds and penalties
# Match the real players to the fifa data

# variables to add : won world cup already, 
# number of world cups, number of times pass round, number of classifications

# Database per country to match to games

# Need to create shares of winnings and goals per quality of team
# Need to include difference in rankings
# Try one specification weighted by ranking of the opponent


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



# Add custom font
font_add_google("Cairo", "cairo")
showtext_auto()


# Fifa game datasets
#----------------------------------------------------------------------

# Function to read and summarize scores in fifa game by country
fifa_game <- function(y) {
  path_data <- paste0("data/players_",y,".csv")
  game_scores <- read_csv(path_data) %>% 
    clean_names() %>% 
    mutate(overall=(overall+potential)/2) %>% 
    arrange(nationality_name , desc(overall)) %>% 
    group_by(nationality_name) %>% 
    mutate(ranking_player = seq_along(nationality_name)) %>% 
    ungroup() %>% 
    filter(ranking_player < 27) %>% 
    mutate(year= y + 2000 - 1) %>% 
    group_by(nationality_name , year) %>% 
    summarize(overall = mean(overall) , npg=max(ranking_player)) %>%
    ungroup() %>%
    mutate(year=ifelse(nationality_name=="Bhutan",2019,year) , 
           across('nationality_name', str_replace , "Cape Verde Islands" ,"Cape Verde") ,
           across('nationality_name', str_replace , "Curacao" ,"Curaçao") ,
           across('nationality_name', str_replace , "Congo DR" ,"DR Congo") ,
           across('nationality_name', str_replace , "Guinea Bissau" ,"Guinea-Bissau") ,
           across('nationality_name', str_replace , "Guinea Bissau" ,"Guinea-Bissau") ,
           across('nationality_name', str_replace , "Côte d'Ivoire" ,"Ivory Coast") ,
           across('nationality_name', str_replace , "Korea DPR" ,"North Korea") ,
           across('nationality_name', str_replace , "Korea Republic" ,"South Korea"))
  return(game_scores) 
}

# Create a list of datasets for each game
year_fifa <- c(15:23)
fifa_15_23 <- lapply(year_fifa, fifa_game)

# append them
fifa_game_scores <- bind_rows(fifa_15_23[1], fifa_15_23[2], fifa_15_23[3], fifa_15_23[4],
                              fifa_15_23[5], fifa_15_23[6], fifa_15_23[7], fifa_15_23[8],
                              fifa_15_23[9]) %>% 
  group_by(year) %>% 
  mutate(overall_max = max(overall) , overall_min = min(overall) ,
         overall= (overall - overall_min)*100/(overall_max - overall_min)) %>% 
  select(-overall_min , -overall_max) %>% 
  ungroup()

# Remove list of datasets
rm(fifa_15_23)

# Fifa ranking
#-----------------------------------------------------------------------------------
fifa_ranking <- read_csv("data/fifa_ranking.csv") %>% 
  clean_names() %>% 
  mutate(date = as.Date(rank_date, "%Y-%m-%d") , year=year(date) ,
         across('country_full', str_replace , "Brunei Darussalam" ,"Brunei") , 
         across('country_full', str_replace , "Cabo Verde" ,"Cape Verde") , 
         across('country_full', str_replace , "Cape Verde Islands" ,"Cape Verde") ,
         across('country_full', str_replace , "Congo DR" ,"DR Congo") ,
         across('country_full', str_replace , "IR Iran" ,"Iran"), 
         across('country_full', str_replace , "Côte d'Ivoire" ,"Ivory Coast"),
         across('country_full', str_replace , "Kyrgyz Republic" ,"Kyrgyzstan"),
         across('country_full', str_replace , "Korea DPR" ,"North Korea"),         
         across('country_full', str_replace , "St. Kitts and Nevis" ,"Saint Kitts and Nevis"),   
         across('country_full', str_replace , "St. Vincent / Grenadines" ,"Saint Vincent and the Grenadines"), 
         across('country_full', str_replace , "St. Vincent and the Grenadines" ,"Saint Vincent and the Grenadines"), 
         across('country_full', str_replace , "Korea Republic" ,"South Korea"), 
         across('country_full', str_replace , "St. Vincent and the Grenadines" ,"Saint Vincent and the Grenadines"), 
         across('country_full', str_replace , "USA" ,"United States"), 
         across('country_full', str_replace , "US Virgin Islands" ,"United States Virgin Islands")) %>% 
  select(year , country_full, total_points) %>% 
  group_by(country_full , year) %>% 
  summarize(ranking_fifa =mean(total_points)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate( max = max(ranking_fifa) , min = min(ranking_fifa) ,
          ranking_fifa= (ranking_fifa - min)*100/(max - min)) %>% 
  select(-max , -min) %>% 
  ungroup()


# Historical matches dataset
#----------------------------------------------------------------------

# Database of the matches of the first round for the 2022 world cup
world_cup <- read_csv("data/results.csv") %>% 
  clean_names() %>% 
  filter(tournament == "FIFA World Cup" & country == "Qatar") %>% 
  mutate(year=year(date)) 

# Countries playing current world cup
world_cup_countries <- as_tibble(world_cup$home_team) %>% 
  bind_rows(as_tibble(world_cup$away_team)) %>% 
  unique() %>% 
  mutate(playing_wc = 1) %>% 
  rename(team = value)

# Results for the entire history of the cups
results <- read_csv("data/results.csv") %>% 
  clean_names() %>% 
  mutate(date = as.Date(date, "%Y-%m-%d")) %>% 
  mutate(year=year(date) , month=month(date)) %>% 
  filter(!is.na(home_score) | !is.na(away_score)) %>% 
  mutate(home_result = factor(ifelse(home_score > away_score , "win" , 
                                     ifelse(home_score == away_score , 
                                            "draw" , "loss")))  ,
         away_result = factor(ifelse(home_score < away_score , "win" , 
                                     ifelse(home_score == away_score , 
                                            "draw" , "loss") )) , 
         across('home_team', str_replace , "Brunei Darussalam" ,"Brunei") ,
         across('away_team', str_replace , "Brunei Darussalam" ,"Brunei")) %>% 
  dummy_cols(select_columns = c('home_result' , 'away_result')) 

# Include in the main database the FIFA game score
results <- results %>% 
  left_join(fifa_game_scores , c("home_team" = "nationality_name" , "year")) %>% 
  rename_at(vars(overall , npg),function(x) paste0(x,"_home")) %>% 
  left_join(fifa_game_scores , c("away_team" = "nationality_name" , "year")) %>% 
  rename_at(vars(overall , npg),function(x) paste0(x,"_away"))

# Include in the main database the FIFA ranking
results <- results %>% 
  left_join(fifa_ranking , c("home_team" = "country_full" , "year")) %>% 
  rename_at(vars(ranking_fifa),function(x) paste0(x,"_home")) %>% 
  left_join(fifa_ranking , c("away_team" = "country_full" , "year")) %>% 
  rename_at(vars(ranking_fifa),function(x) paste0(x,"_away"))

# Include in the main database and indicator of a match played by countries 
# in the current WC

results <- results %>% 
  left_join(world_cup_countries , c("home_team" = "team")) %>% 
  left_join(world_cup_countries , c("away_team" = "team")) %>% 
  mutate(playing_wc = if_else((playing_wc.x == 1 | playing_wc.y == 1), 
                               1 , 0, missing = 0)) %>% 
  select(-playing_wc.x  , -playing_wc.y)

# Results as list
#----------------------------------------------------------------------

# Create a dataset with information about home teams
results_home <- results %>% 
  rename( team = home_team , goals_scored = home_score ,
          goals_received = away_score ,
          win = home_result_win , 
          draw = home_result_draw ,
          loss = home_result_loss ,
          ranking_fifa = ranking_fifa_home, 
          ranking_fifa_opp = ranking_fifa_away ,
          overall = overall_home  , 
          overall_opp = overall_away ) %>%  
  mutate (ranking_fifa_inv = 1/ranking_fifa, 
          ranking_fifa_opp_inv = 1/ranking_fifa_opp ,
          overall_inv = 1/overall  , 
          overall_opp_inv = 1/overall_opp) %>% 
  select(year , team , goals_scored , goals_received , 
         win, draw , loss , tournament , date , starts_with("ranking_fifa") ,
         starts_with("overall"))  %>% 
  mutate(home = 1)

# Create a dataset with information about visiting teams
results_away <- results %>% 
  rename( team = away_team , goals_scored = away_score ,
          goals_received = home_score ,
          win = away_result_win , 
          draw = away_result_draw ,
          loss = away_result_loss ,
          ranking_fifa = ranking_fifa_away, 
          ranking_fifa_opp = ranking_fifa_home ,
          overall = overall_away  , 
          overall_opp = overall_home) %>% 
  mutate (ranking_fifa_inv = 1/ranking_fifa, 
          ranking_fifa_opp_inv = 1/ranking_fifa_opp ,
          overall_inv = 1/overall  , 
          overall_opp_inv = 1/overall_opp) %>% 
  select(year , team , goals_scored , goals_received , 
           win, draw , loss , tournament , date , starts_with("ranking_fifa") ,
           starts_with("overall"))  %>% 
  mutate(home = 0)

# Create a list of the countries that played the qualification phase to the WC 
country_wc_qualifier <- results_home %>%  
  bind_rows(results_away) %>% 
  filter(year >2017 & tournament == "FIFA World Cup qualification") %>% 
  select(team) %>% 
  unique()

# Update the results to matches where both countries played the qualification phase of 
# the current World Cup

results <- results %>% 
  right_join(country_wc_qualifier , c("home_team" = "team"))  %>% 
  right_join(country_wc_qualifier , c("away_team" = "team"))   
  
# Create a list of results of countries that played the current phase of the WC
results_list <- results_home %>%  # home results
  bind_rows(results_away) %>% # append away results
  right_join(country_wc_qualifier) %>% 
  mutate(points = win*3 + draw) %>% 
  unique()
  rm(results_home , results_away) # remove from memory duplicate data

# List of tournaments
tournaments <- results_list %>%
  filter(year >2014) %>% 
  select(tournament) %>% 
  unique() 

# Relation game score and fifa ranking
#----------------------------------------------------------------------
results_list %>% 
  ggplot(aes(y=ranking_fifa , x=overall)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x + x^2)

# Tournaments
#----------------------------------------------------------------------

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
                             "FIFA World Cup" ,
                             "Confederations Cup")

# Historical results in a rolling window
#--------------------------------------------------------------------------

# Entire history
results_hist <- results_list %>%  
  mutate( n = 1) %>% 
  arrange(team , date) %>% 
  group_by(team) %>% 
  mutate(n_matches = cumsum(n), 
         sh_win = cummean(win) , 
         sh_loss = cummean(loss) , 
         av_goal_sc = cummean(goals_scored) ,
         av_goal_rec = cummean(goals_received)) %>% 
  select(team, date , starts_with("sh_") , 
         starts_with("av_") , n_matches)

# Since FIFA Ranking exists and there are 32 teams in the WC (Ranking fifa available)
results_hist_rf <- results_list %>%  
  filter(year>=1994 & !is.na(ranking_fifa_opp)) %>% 
  mutate( n = 1) %>% 
  arrange(team , date) %>% 
  group_by(team) %>% 
  mutate(n_matches = cumsum(n), 
         sh_win = cummean(win) , 
         sh_loss = cummean(loss) , 
         av_goal_sc = cummean(goals_scored) ,
         av_goal_rec = cummean(goals_received) ,
         av_rf_opp = cummean(ranking_fifa_opp)) %>% 
  select(team, date , starts_with("sh_") , 
         starts_with("av_") , n_matches) 

# Since fifa exists for countries with information in their opponents score
results_hist_go <- results_list %>%  
  filter(year>=2013 & !is.na(overall_opp)) %>% 
  mutate( n = 1) %>% 
  arrange(team , date) %>% 
  group_by(team) %>% 
  mutate(n_matches = cumsum(n), 
         sh_win = cummean(win) , 
         sh_loss = cummean(loss) , 
         av_goal_sc = cummean(goals_scored) ,
         av_goal_rec = cummean(goals_received) ,
         av_go_opp = cummean(overall_opp)) %>% 
  select(team, date , starts_with("sh_") , 
         starts_with("av_") , n_matches)  %>% 
  arrange(team , date) 

# Rolling basis results
#----------------------------------------------------------------------

# All last matches
results_rolling <- results_list %>% 
  arrange(team , date) %>% 
  group_by(team) %>% 
  mutate(match_number = seq_along(team)) %>% 
  ungroup() %>% 
  mutate(m5_goals_scored = ifelse(match_number > 5 , rollmean(goals_scored, k = 5, fill = NA , align="right") , NA) ,
         m15_goals_scored = ifelse(match_number > 15 , rollmean(goals_scored, k = 15, fill = NA , align="right") , NA) ,
         m30_goals_scored = ifelse(match_number > 30 , rollmean(goals_scored, k = 30, fill = NA , align="right") , NA) ,
         m5_goals_received = ifelse(match_number > 5 , rollmean(goals_received, k = 5, fill = NA , align="right") , NA) ,
         m15_goals_received = ifelse(match_number > 15 , rollmean(goals_received, k = 15, fill = NA , align="right") , NA) ,
         m30_goals_received = ifelse(match_number > 30 , rollmean(goals_received, k = 30, fill = NA , align="right") , NA) ,
         m5_sh_win = ifelse(match_number > 5 , rollmean(win, k = 5, fill = NA , align="right") , NA) ,
         m15_sh_win = ifelse(match_number > 15 , rollmean(win, k = 15, fill = NA , align="right") , NA) ,
         m30_sh_win = ifelse(match_number > 30 , rollmean(win, k = 30, fill = NA , align="right") , NA) ,
         m5_sh_loss = ifelse(match_number > 5 , rollmean(loss, k = 5, fill = NA , align="right") , NA) ,
         m15_sh_loss= ifelse(match_number > 15 , rollmean(loss, k = 15, fill = NA , align="right") , NA) ,
         m30_sh_loss = ifelse(match_number > 30 , rollmean(loss, k = 30, fill = NA , align="right") , NA) ,
         m5_rf_opp = ifelse(match_number > 5 , rollmean(ranking_fifa_opp, k = 5, fill = NA , align="right") , NA) ,
         m15_rf_opp = ifelse(match_number > 15 , rollmean(ranking_fifa_opp, k = 15, fill = NA , align="right") , NA) ,
         m30_rf_opp = ifelse(match_number > 30 , rollmean(ranking_fifa_opp, k = 30, fill = NA , align="right") , NA) ,
         m5_go_opp = ifelse(match_number > 5 , rollmean(overall_opp, k = 5, fill = NA , align="right") , NA) ,
         m15_go_opp = ifelse(match_number > 15 , rollmean(overall_opp, k = 15, fill = NA , align="right") , NA) ,
         m30_go_opp = ifelse(match_number > 30 , rollmean(overall_opp, k = 30, fill = NA , align="right") , NA)) %>% 
  select(team, date , starts_with("m5") , starts_with("m15") , starts_with("m30")) 
         
# Latest results for world cup qualification
results_rolling_wcq <-results_list %>% 
  filter(tournament == "FIFA World Cup qualification") %>% 
  arrange(team , date) %>% 
  group_by(team) %>% 
  mutate(match_number = seq_along(team)) %>% 
  ungroup() %>% 
  mutate(m10_goals_sc_qual = ifelse(match_number > 10 , rollmean(goals_scored, k = 10, fill = NA , align="right") , NA) ,
         m10_goals_rec_qual = ifelse(match_number > 10 , rollmean(goals_received, k = 10, fill = NA , align="right") , NA) ,
         m10_sh_win_trn = ifelse(match_number > 10 , rollmean(win, k = 10, fill = NA , align="right") , NA) ,
         m10_sh_loss_trn = ifelse(match_number > 10 , rollmean(loss, k = 10, fill = NA , align="right") , NA) ,
         m10_rf_opp_qual = ifelse(match_number > 10 , rollmean(ranking_fifa_opp, k = 10, fill = NA , align="right") , NA) ,
         m10_go_opp_qual = ifelse(match_number > 10 , rollmean(overall_opp, k = 10, fill = NA , align="right") , NA)) %>% 
  select(team, date , starts_with("m10")) 

# Latest results for tournaments including world cups
results_rolling_trn <-results_list %>% 
  filter(tournament %in% continental_tournaments) %>% 
  arrange(team , date) %>% 
  group_by(team) %>% 
  mutate(match_number = seq_along(team)) %>% 
  ungroup() %>% 
  mutate(m10_goals_sc_trn = ifelse(match_number > 10 , rollmean(goals_scored, k = 10, fill = NA , align="right") , NA) ,
         m10_goals_rec_trn = ifelse(match_number > 10 , rollmean(goals_received, k = 10, fill = NA , align="right") , NA) ,
         m10_sh_win_trn = ifelse(match_number > 10 , rollmean(win, k = 10, fill = NA , align="right") , NA) ,
         m10_sh_loss_trn = ifelse(match_number > 10 , rollmean(loss, k = 10, fill = NA , align="right") , NA) ,
         m10_rf_opp_trn = ifelse(match_number > 10 , rollmean(ranking_fifa_opp, k = 10, fill = NA , align="right") , NA) ,
         m10_go_opp_trn = ifelse(match_number > 10 , rollmean(overall_opp, k = 10, fill = NA , align="right") , NA)) %>% 
  select(team, date , starts_with("m10")) 

# Results in world cups
#----------------------------------------------------------------------

# Years of world cup
years_wc <-  results_list %>% 
  filter(tournament == "FIFA World Cup") %>% 
  select (year) %>% 
  unique() %>% 
  pull(year)

# Entire history
results_wc_hist <- results_list %>%  
  filter(tournament == "FIFA World Cup") %>% 
  mutate( n = 1) %>% 
  group_by(team , year) %>%
  summarize( matches = sum(n) , win=sum(win) , loss=sum(loss) ,
             goals_scored = sum(goals_scored) , goals_received = sum(goals_received) , 
             ranking_fifa_opp = sum(ranking_fifa_opp , na.rm=TRUE) ,
             overall_opp = sum(overall_opp , na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(team) %>%  complete(year = seq(1930,2018 )) %>%
  filter(year %in% years_wc ) %>% 
  mutate(across(matches:overall_opp, ~replace_na(.,0))) %>% 
  mutate(n_matches_wch = cumsum(matches), 
         sh_win_wch = cumsum(win) / n_matches_wch ,
         sh_loss_wch = cumsum(loss) / n_matches_wch , 
         av_goals_sc_wch = cummean(goals_scored) ,
         av_goals_rec_wch  = cummean(goals_received))


# Since ranking fifa exists
results_wc_hist_rf <- results_list %>%  
  filter(tournament == "FIFA World Cup") %>%
  filter(year>=1994) %>% 
  mutate( n = 1) %>% 
  group_by(team , year) %>%
  summarize( matches = sum(n) , win=sum(win) , loss=sum(loss) ,
             goals_scored = sum(goals_scored) , goals_received = sum(goals_received) , 
             ranking_fifa_opp = sum(ranking_fifa_opp , na.rm=TRUE) ,
             overall_opp = sum(overall_opp , na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(team) %>%  complete(year = seq(1994,2018 )) %>%
  filter(year %in% years_wc ) %>% 
  mutate(across(matches:overall_opp, ~replace_na(.,0))) %>% 
  mutate(n_matches_wch = cumsum(matches), 
         sh_win_wch = cumsum(win) / n_matches_wch ,
         sh_loss_wch = cumsum(loss) / n_matches_wch , 
         av_goals_sc_wch = cummean(goals_scored) ,
         av_goals_rec_wch  = cummean(goals_received) ,
         av_rf_opp_wch = cumsum(ranking_fifa_opp) /n_matches_wch)

# Since fifa game exists
results_wc_hist_go <- results_list %>%  
  filter(tournament == "FIFA World Cup") %>%
  filter(year>=2014) %>% 
  mutate( n = 1) %>% 
  group_by(team , year) %>%
  summarize( matches = sum(n) , win=sum(win) , loss=sum(loss) ,
             goals_scored = sum(goals_scored) , goals_received = sum(goals_received) , 
             ranking_fifa_opp = sum(ranking_fifa_opp , na.rm=TRUE) ,
             overall_opp = sum(overall_opp , na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(team) %>%  complete(year = seq(2014,2018 )) %>%
  filter(year %in% years_wc ) %>% 
  mutate(across(matches:overall_opp, ~replace_na(.,0))) %>% 
  mutate(n_matches_wch = cumsum(matches), 
         sh_win_wch = cumsum(win) / n_matches_wch ,
         sh_loss_wch = cumsum(loss) / n_matches_wch , 
         av_goals_sc_wch = cummean(goals_scored) ,
         av_goals_rec_wch  = cummean(goals_received) ,
         av_rf_opp_wch = cumsum(ranking_fifa_opp) /n_matches_wch ,
         av_go_opp_wch = cumsum(overall_opp)/n_matches_wch)


# Join with results
#----------------------------------------------------------------------

# Keep just information of the matches played since 2014
results <- results %>% 
  filter(year >=2014) %>% 
  filter(home_team !=away_team) %>%
  unique()


results_try <- results

# Join historical results
results_try <- results_try %>% 
  left_join(results_hist ,by =c("home_team" = "team" , "date")) %>% 
  rename_at(vars(sh_win:n_matches),function(x) paste0(x,"_hist_home")) %>% 
  left_join(results_hist , by =c("away_team" = "team" , "date")) %>% 
  rename_at(vars(sh_win:n_matches),function(x) paste0(x,"_hist_away")) %>% 
  unique()

# Join historical results since Ranking FIFA exists
results_try <- results_try %>% 
  left_join(results_hist_rf ,by =c("home_team" = "team" , "date")) %>% 
  rename_at(vars(sh_win:n_matches),function(x) paste0(x,"_hist_rf_home")) %>% 
  left_join(results_hist_rf , by =c("away_team" = "team" , "date")) %>% 
  rename_at(vars(sh_win:n_matches),function(x) paste0(x,"_hist_rf_away")) 

# Join historical results since FIFA scores exists
results_try <- results_try %>% 
  left_join(results_hist_go ,by =c("home_team" = "team" , "date")) %>% 
  rename_at(vars(sh_win:n_matches),function(x) paste0(x,"_hist_go_home")) %>% 
  left_join(results_hist_go , by =c("away_team" = "team" , "date")) %>% 
  rename_at(vars(sh_win:n_matches),function(x) paste0(x,"_hist_go_away")) 

# Join rolling basis world cup qualifiers
results_try <- results_try %>% 
  left_join(results_rolling_wcq , by =c("home_team" = "team" , "date")) %>% 
  rename_at(vars(ends_with("_qual")),function(x) paste0(x,"_home")) %>%  
  left_join(results_rolling_wcq , by =c("away_team" = "team" , "date")) %>% 
  rename_at(vars(ends_with("_qual")),function(x) paste0(x,"_away"))


# Join rolling basis international tournaments
results_try <- results_try %>% 
  left_join(results_rolling_trn , by =c("home_team" = "team" , "date")) %>% 
  rename_at(vars(ends_with("_qual")),function(x) paste0(x,"_home")) %>%  
  left_join(results_rolling_trn , by =c("away_team" = "team" , "date")) %>% 
  rename_at(vars(ends_with("_qual")),function(x) paste0(x,"_away"))


# Export cleaned dataset
write_csv(results_try, "data/results_cleaned.csv")


# Model
# ---------------------------------

# Initial split
set.seed(1993)

# Creating a bar chart of the distribution of results
results_try %>% 
  filter(neutral == "TRUE") %>% 
  ggplot(aes(y=forcats::fct_infreq(home_result) , fill=home_result)) +
  geom_bar() +
  labs(title="Home team match results", y="Result" , x="Number") +
  theme_minimal() + theme(legend.position="none") +
  scale_fill_viridis_d() 

# Perform initial split, stratifying by the outcome variable with a proportion
# of 80% in the training
results_split <- initial_split(results_try, strata = "home_result" , prop = 0.8)

# Create training sample
results_train <- training(results_split)

# Create testing sample
results_test <- testing(results_split)

# Perform 5-folds cross-validation
results_fold <- vfold_cv(results_train, v = 5 , strata = "home_result")

# Convert to factors
results_try <- results_try  %>%
  mutate(home_result=factor(home_result) ,
         neutral=factor(neutral) , 
         tournament=factor(tournament)) 

# Create the recipe tournament + neutral
recipe <- recipe(formula = home_result ~  + overall_home + overall_away + ranking_fifa_home + ranking_fifa_away + 
                  + ranking_fifa_away +          sh_win_hist_home +        
                   sh_loss_hist_home +        av_goal_sc_hist_home +     av_goal_rec_hist_home +   
                   n_matches_hist_home +      sh_win_hist_away +         sh_loss_hist_away +       
                   av_goal_sc_hist_away +     av_goal_rec_hist_away +    n_matches_hist_away +     
                   sh_win_hist_rf_home +      sh_loss_hist_rf_home +     av_goal_sc_hist_rf_home + 
                   av_goal_rec_hist_rf_home + av_rf_opp_hist_rf_home +   n_matches_hist_rf_home +  
                   sh_win_hist_rf_away +      sh_loss_hist_rf_away +     av_goal_sc_hist_rf_away + 
                   av_goal_rec_hist_rf_away + av_rf_opp_hist_rf_away +   n_matches_hist_rf_away ,
                 data = results_try) %>% 
  #step_dummy(playing_wc , neutral) %>% 
  step_interact( ~ sh_win_hist_rf_home:av_rf_opp_hist_rf_away) %>% 
  step_normalize(all_predictors())

colnames(results_try)

# Set up the model specification
specification <- 
  multinom_reg(penalty = tune(), mixture = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet") 

# Set up the workflow
workflow <- workflow() %>% 
  add_recipe(recipe) %>% 
  add_model(specification)

# Set up the penalty and mixture grid
en_grid <- grid_regular(penalty(range = c(-5, 5)), 
                        mixture(range=c(0,1)) , 
                        levels = 10)

# Tunning
tune_results <- tune_grid(
  workflow,
  resamples = results_fold, 
  grid = en_grid
)

# Autoplot
autoplot(tune_results)


# select the best value of penalty and the moxture
best_penalty_mix <- select_best(tune_results, metric = "roc_auc")

# Include in the workflow the optimal penalty and mixture
model_opt <- finalize_workflow(workflow, best_penalty_mix)

# Refit the using the whole training data set:
final_fit <- fit(model_opt, data = results_train)

# predict on the testing set:
predicted_test <- augment(final_fit, new_data = results_test) %>%
  select(home_result, starts_with(".pred"))

# Assess accuracy
predicted_test %>%
  accuracy(truth = home_result, estimate = .pred_class)


# ROC curves by type
predicted_test %>% roc_curve(home_result , .pred_draw:.pred_win) %>% 
  autoplot()


# Create a heatmap of the confussion matrix
predicted_test %>%
  conf_mat(truth = home_result, estimate = .pred_class) %>%
  autoplot(type = "heatmap") 


# Test joins
#----------------------------------------------------------------------

# Fifa game scores
fifa_scores <- results_list %>%
  select(year , team) %>% 
  left_join(fifa_game_scores , by =c("team" = "nationality_name", "year")) %>% 
  filter(year > 2013) %>% 
  unique() %>% 
  arrange(team , year) %>% 
  group_by(team) %>%
  fill(overall, .direction = "updown") %>%
  ungroup() %>% 
  filter(is.na(overall)) %>% 
  view()

fifa_game_country <- fifa_game_scores %>% 
  select(nationality_name , year, overall) %>% 
  unique()  %>% 
  view()

# Fifa ranking
fifa_rank_match <- results %>%
  select(year , away_team) %>% 
  left_join(fifa_ranking , c("away_team" = "country_full" , "year")) %>% 
  filter(year > 2013) %>% 
  unique() %>% 
  arrange(away_team , year) %>% 
  group_by(away_team) %>%
  fill(ranking_fifa, .direction = "updown") %>%
  ungroup() %>% 
  filter(is.na(ranking_fifa)) %>% 
  select(away_team) %>% 
  unique()

fifa_rank_country <- fifa_ranking %>% 
  select(country_full) %>% 
  unique()





{r}
# Open cleaned data
results <- read_csv("data/results_cleaned.csv")

results %>% 
  mutate(rf_diff_ha = ranking_fifa_home - ranking_fifa_away) %>% 
  ggplot(aes(x=rf_diff_ha, group=home_result, fill=home_result)) +
  geom_density(adjust=1, alpha=.4) +
  theme_minimal() + labs(title="Distribution of Differences in the Ranking FIFA \n by Outcome of the Home Team", x="Home Ranking FIFA - Away Ranking FIFA" , fill= "Type") + theme(plot.title = element_text(hjust = 0.5))

{r}
# Open cleaned data
results <- read_csv("data/results_cleaned.csv")

results %>% 
  mutate(rf_diff_ha = ranking_fifa_home - ranking_fifa_away ,
         ov_diff_ha = overall_home - overall_away) %>% 
  ggplot(aes(x=rf_diff_ha, group=home_result, fill=home_result)) +
  geom_density(adjust=1, alpha=.4) +
  theme_minimal() + labs(title="Distribution of Differences in the Ranking FIFA \n by Outcome of the Home Team", x="Home Ranking FIFA - Away Ranking FIFA" , fill= "Type") + theme(plot.title = element_text(hjust = 0.5))

# Relation videogame scores and difference in goals scored
results %>% 
  filter(year== 2014 | year==2018 | year ==2022) %>% 
  mutate(year=factor(year) ,
         ov_diff_ha = overall_home - overall_away , goal_diff_ha = home_score - away_score) %>% 
  ggplot(aes(y=goal_diff_ha , x=ov_diff_ha, color=year , shape = year)) +
  geom_point(alpha=0.5, aes(color=year)) + 
  scale_color_manual(values = colors_3) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = F) +
  labs(y="Difference in Goals Scored", x= "Difference videogame score",
       title="Yearly comparison between the Videogame and FIFA indexes") +
  theme_minimal()




# FIFA dataset analysis
#----------------------------------------------------------------------


# Country level database
fifa_23_2 <- read_csv("data/Fifa 23 Players Data.csv") %>% 
  clean_names()

# Fifa choose
qatar_ecuador <- fifa_23_2 %>% 
  filter(nationality =="Ecuador" | nationality == "Qatar") %>% 
  group_by(nationality) %>% 
  slice_max(overall , n =25) %>% 
  summarize(mean_overall = mean(overall) , med_overall =median(overall) , sd_ov =sd(overall))

%>% 
  clean_names()
?summarize


# Fifa data
fifa_23 <- read_csv("data/FIFA23_official_data.csv") %>% 
  clean_names()

# Fifa data
fifa_22 <- read_csv("data/FIFA22_official_data.csv") %>% 
  clean_names()

fifa_23_2 <- read_csv("data/Fifa 23 Players Data.csv") %>% 
  clean_names()


national <- unique(fifa_23_2$national_team_name)
national


wc_2018_urls <- fb_match_urls(country = "", gender = "M", 
                              season_end_year = 2018, tier = "", 
                              non_dom_league_url = "https://fbref.com/en/comps/1/history/World-Cup-Seasons")

# for international friendlies:
international_results_friendly <- fb_match_results(country = "", gender = "M", 
                                          season_end_year = 2021, tier = "",
                                          non_dom_league_url = "https://fbref.com/en/comps/218/history/Friendlies-M-Seasons")

# for international friendlies:
international_results_friendly <- fb_match_results(country = "", gender = "M", 
                                                   season_end_year = 2021, tier = "",
                                                   non_dom_league_url = "https://fbref.com/en/comps/218/history/Friendlies-M-Seasons")


# Qualifications
qualif_conmebol <- fb_match_results(country = "", gender = "M", 
                                                   season_end_year = 2022, tier = "",
                                                   non_dom_league_url = "https://fbref.com/en/comps/4/history/WCQ----CONMEBOL-M-Seasons")


#
conmebol_urls <- fb_match_urls(country = "", gender = "M", 
                              season_end_year = 2022, tier = "", 
                              non_dom_league_url = "https://fbref.com/en/comps/4/history/WCQ----CONMEBOL-M-Seasons")

conmebol_lineups <- fb_match_lineups(match_url = conmebol_urls)


print("ready")

# References
# https://www.kaggle.com/code/evangower/using-the-worldfootballr-package
# https://www.maths.ox.ac.uk/node/61756
# https://www.kaggle.com/datasets/shilongzhuang/soccer-world-cup-challenge?select=data_dictionary.csv