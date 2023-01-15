##### hoopR timeouts #####

library(hoopR)
library(tidyverse)

pbp <- load_mbb_pbp()
unique(pbp$type_text)

to <- pbp %>%
  filter(type_text == "RegularTimeOut")

pbp <- pbp %>%
  mutate(clock_minutes = as.numeric(clock_minutes),
         clock_seconds = as.numeric(clock_seconds)) %>%
  group_by(game_id, period) %>%
  arrange(game_id, period, desc(clock_minutes), desc(clock_seconds)) %>%
  mutate(starting_full_timeouts = 1,
         starting_short_timeouts = 3) %>%
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
         short_timeouts_remaining = short_timeouts_remaining + overtime_period)


