##### hoopR timeouts #####

library(hoopR)
library(tidyverse)

pbp <- load_mbb_pbp()
unique(pbp$type_text)

to <- pbp %>%
  filter(type_text == "RegularTimeOut")



pbp_cleaned <- pbp %>%
  mutate(clock_minutes = as.numeric(clock_minutes),
         clock_seconds = as.numeric(clock_seconds)) %>%
  mutate(second_half_or_overtime = ifelse(period > 1, 1, 0)) %>%
  group_by(game_id, period) %>%
  arrange(game_id, period, desc(clock_minutes), desc(clock_seconds)) %>%
  mutate(starting_full_timeouts = 1,
         starting_short_timeouts = 3) %>%
  group_by(game_id, second_half_or_overtime, team_id) %>% # can't do period since bonus doesn't reset for OT
  mutate(foul_called = ifelse(type_text == "PersonalFoul", 1, 0),
         foul_count = cumsum(foul_called),
         shooting_bonus = case_when(foul_count < 6 ~ "none",
                                    foul_count >= 6 & foul_count < 9 ~ "bonus",
                                    foul_count >= 9 ~ "double_bonus")) %>%
  group_by(game_id, team_id) %>%
  mutate(called_full_timeout = ifelse(type_text == "RegularTimeOut",
                                      1,
                                      0),
         called_short_timeout = ifelse(type_text == "ShortTimeOut",
                                       1,
                                       0),
         used_full_timeouts = cumsum(called_full_timeout),
         used_short_timeouts = cumsum(called_short_timeout),
         full_timeouts_remaining = starting_full_timeouts - used_full_timeouts,
         short_timeouts_remaining = starting_short_timeouts - used_short_timeouts,
         overtime_period = max(period - 2, 0),
         short_timeouts_remaining = short_timeouts_remaining + overtime_period) %>%
  ungroup() %>%
  separate(clock_display_value,
           into = c("time_minutes", "time_seconds"),
           sep = ":") %>%
  mutate(across(starts_with("time_"),
                .fns = ~ as.numeric(.x)),
         seconds_remaining_in_game = (time_minutes * 60) + time_seconds +
                                     ifelse(period == 1, 1200, 0),
         score_diff = home_score - away_score,
         event_team = ifelse(team_id == home_team_id,
                             home_team_name,
                             away_team_name),
         point_diff_time_ratio = score_diff * # 2400 might need to change for OT periods?
                                 exp(4 * ((2400 - seconds_remaining_in_game) / 2400)))


