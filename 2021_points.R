library(nflreadr)
library(tidyverse)
library(nflfastR)

stats <- nflreadr::load_player_stats(seasons = 2021) %>%
  filter(season_type == "REG")

players <- load_rosters(seasons = 2021) %>%
  mutate(player_id = gsis_id, recent_team = team) %>%
  select(player_id, full_name, position, recent_team)

pbp <- load_pbp(seasons = 2021) %>%
  filter(interception==1, touchdown==1)

pick_six <- pbp %>%
  group_by(passer_player_id, passer_player_name, week) %>%
  summarize(pick_six_count = n()) %>%
  mutate(player_id = passer_player_id, player_name = passer_player_name) %>%
  ungroup() %>%
  select(player_id, player_name, week, pick_six_count)


fantasy_points <- stats %>%
  left_join(pick_six, by=c("player_id", "week", "player_name")) %>%
  mutate(pick_six_count = ifelse(is.na(pick_six_count), 0, pick_six_count)) %>%
  mutate(fantasy_half_points = (fantasy_points_ppr - receptions) + (receptions/2) + 
           2*(rushing_fumbles_lost + receiving_fumbles_lost + sack_fumbles_lost) - 
           (rushing_fumbles_lost + receiving_fumbles_lost + sack_fumbles_lost + 
              rushing_fumbles + receiving_fumbles + sack_fumbles) - 
           4*pick_six_count) %>%
  group_by(player_id, player_name, recent_team) %>% 
  summarize(across(where(is.numeric), sum)) %>%
  ungroup()

final <- fantasy_points %>%
  left_join(players, by = c("player_id", "recent_team"))
  


 

