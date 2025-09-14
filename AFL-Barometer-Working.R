# Who is Your Club's Barometer? 
# I.e., who, when they play better than average, is correlated with your 
# team winning
# Joel Fernando, 2025

# Thoughts: 
# Should try add some more control variables this time e.g. Finals vs Regular Season, 
# Home or Away, Venue, Opponent (?), Season (?)

#-----Loading Packages
library(fitzRoy)
library(tidyverse)
library(purrr)
library(here)

#-----What Year is it?
current_year <- lubridate::year(Sys.Date())

#------Fetching Match Level Data
# Using the fitzRoy package
all_matches <- purrr::map(2010:current_year, 
                          ~ fitzRoy::fetch_results_afltables(season = .x)) |> 
  bind_rows() |> 
  janitor::clean_names()

#-------Data Cleaning Match Level Data
match_data <- all_matches |>
  mutate(
    winner = case_when(
      home_points > away_points ~ home_team, # Codifying the winner of each game
      home_points < away_points ~ away_team,
      home_points == away_points ~ "Draw"
    ), 
  )

#--------Fetching Player Level Data 
# AFL Data (which is where we get the rating points from) only starts in 2016
all_player_stats <- purrr::map(2016:current_year,
                               ~ fitzRoy::fetch_player_stats_afl(season = .x)) |>
  bind_rows() |>
  janitor::clean_names()

#--------Data Cleaning the Player Level Data
player_data <- all_player_stats |> 
  select(utc_start_time, round_number = round_round_number, # Selecting relevant columns 
         first_name = player_player_player_given_name, 
         surname = player_player_player_surname,
         team_name, 
         player_position = player_player_position, 
         captain = player_player_player_captain,
         games_played, rating_points, 
         home_team = home_team_name, 
         away_team = away_team_name) |> 
  mutate(utc_start_time = str_replace(utc_start_time, "T.*", "")) |> # Convering the utc time to a date to extract the season so I can join with the player datafram
  mutate(date = as.Date(utc_start_time, format = "%Y-%m-%d")) |> 
  mutate(season = lubridate::year(date)) |> 
  relocate(c(date, season)) |> 
  select(-utc_start_time)

# Function to ensure all team names are the same
# Can left join to convert
team_names <- data.frame(
  player_data_names = unique(player_data$away_team) |> sort(), 
  match_data_names = c("Adelaide","Brisbane Lions", "Carlton", "Collingwood", 
                       "Essendon", "Fremantle", "Geelong", "Gold Coast", 
                       "GWS", "Hawthorn", "Melbourne", "North Melbourne",
                       "Port Adelaide", "Richmond", "St Kilda", "Sydney", 
                       "West Coast", "Footscray"))

# Ensuring the same naming across dataframes - this will help join the match
# and player dataframes together
match_data_join <- left_join(match_data, team_names, 
                        by = c("winner" = "match_data_names"))  |>
  mutate(across(c(home_team, away_team, winner), 
                ~ case_match(.x,
                  "Adelaide" ~ "Adelaide Crows",
                  "Geelong" ~ "Geelong Cats",
                  "Gold Coast" ~ "Gold Coast SUNS",
                  "GWS" ~ "GWS GIANTS",
                  "Sydney" ~ "Sydney Swans",
                  "West Coast" ~ "West Coast Eagles",
                  "Footscray" ~ "Western Bulldogs",
                  .default = .x
                ))) # Converting match data team names to player data team names

# Merge player and match dataframes
player_match <-
  left_join(player_data,
            match_data_join,
            by = c("date",
                   "home_team",
                   "away_team",
                   "season")) |> 
  filter(!is.na(rating_points)) |> 
  mutate(name = paste(first_name, surname)) |> 
  relocate(name, .after = surname)

# Distribution of player ratings
ggplot(player_match, aes(x = rating_points)) + 
  geom_density()

# Data check
# colSums(is.na(player_match |> select(where(is.numeric))))
# Some NAs for round number, b/c of opening round in 2024, last round is different across dataframes -> no big issue
# Some NAs for rating points - mostly random games and players in 2016 and some emergencies in 2014 games -> can remove these

# player rating summaries 
player_rating_summary <- player_match |> 
  mutate(name = stringr::str_to_title(name)) |> # To fix duplicate names like de Goey and De Goey
  mutate(name = ifelse(name == "Cam Ellis-Yolmen", "Cameron Ellis-Yolmen", name)) |> # One naming irregularity, see test_names to check other 
  summarise(mean_rating = mean(rating_points),
            median_rating = median(rating_points),
            max_rating = max(rating_points),
            min_rating = min(rating_points),
            games_played = n(),
            .by = name) |> 
  arrange(desc(mean_rating))

# Testing cases where there may be duplicates e.g. players with 3 words in their name (e.g. Tom De Koning) or a hyphen
test_names <- player_rating_summary |> 
  filter(stringr::str_count(name, " ") == 2 | stringr::str_detect(name, "-")) |> 
  arrange(name)

# Some odd naming irregularities e.g. Jordan de Goey vs Jordan De Goey, 
# Cam Ellis-Yolmen, Cameron Ellis Yolmen - might need to remove duplicates
# by indexing by player number and using just one format

#-------------------------------------------------------------------------------
#-----------------------------Summary Statistics--------------------------------
#-------------------------------------------------------------------------------

# Games played vs player ratings
ggplot(
  player_rating_summary |> filter(games_played >= 50)
  ,
  aes(x = games_played, y = mean_rating)
) +
  geom_point(colour = "blue") +
  ggrepel::geom_text_repel(aes(label = ifelse(mean_rating > 15, name, "")), vjust = -1) +
  ggrepel::geom_text_repel(aes(label = ifelse(mean_rating < 7 &
                                                games_played > 175, name, "")), vjust = -1) +
  labs(title = "Games Played vs Average Player Rating",
       subtitle = "Min. 50 games played, since 2016",
       caption = "Source: AFL") + 
  theme_minimal()

ggsave(here("graphs","games_played-vs-rating.png"))

# Distribution of player ratings - bit of a long left tail
mean_line <- mean(player_rating_summary$mean_rating)

ggplot(
  player_rating_summary,
  aes(x = mean_rating)
) + 
  geom_density() +
  geom_vline(xintercept = mean_line, linetype = "dashed", colour = "blue") + 
  labs(title = "Distribution of Average Player Ratings",
       subtitle = "Min. 50 games played, since 2016, blue line is average",
       caption = "Source: AFL") + 
  theme_minimal()

ggsave(here("graphs", "distribution-average-player-rating.png"))

#--------------------------------------
# Regression of player rating on games played
summary(lm(mean_rating ~ games_played, data = player_rating_summary))

#-------------------------------------------------------------------------------
# Calculating the barometer of each team
# - could add games played as a control variable
# - position listed as control variable
# - is there a way to control for opposition strength? ladder position?

# Dataframe to calculate how many games each player has played (since 2016)
games_played <- player_match |> 
  select(date, name) |> 
  arrange(date) |> 
  mutate(games_played = row_number(), .by = name)
  
# position categories
forward <- c("HFFL", "FF", "CHF", "FPL", "FPR", "HFFR")
midfield <- c("WR", "WL", "C", "R", "RR")
defender <- c("FB", "HBFR", "BPR", "CHB", "HBFL", "BPL")
ruck <- c("RK")
sub <- c("SUB")
bench <- c("INT")

# Which position does a player primarily play - based off the team sheet
player_primary_position <- player_match |> 
  select(date, name, player_position) |> 
  mutate(position = case_when(
    player_position %in% forward ~ "forward",
    player_position %in% midfield ~ "midfielder", # Reducing the categories down 
    player_position %in% defender ~ "defender", 
    player_position %in% ruck ~ "ruck", 
    player_position %in% sub ~ "sub",
    player_position %in% bench ~ "bench",
    .default = NA
  )) |> 
  mutate(position_count = n(), .by = c(name, position)) |> 
  slice_max(order_by = position_count, by = name, with_ties = FALSE) |> # Keep the player in the position they have been most listed as
  select(name, position)
  
# Start with the dataframe - select the appropriate columns from player_match
# Need to create the appropriate dummy variables and controls
df_player_barometer <- player_match |> 
  select(date, season, round_number = round_number.x, name, rating_points,
         player_team_name = team_name, player_position, 
         round_type, winner) |> 
  mutate(
    team_won = ifelse(player_team_name == winner, 1, 0), # Dumm¬y variable for if a players team was the winner
    final = ifelse(round_type == "Finals", 1, 0) # Dummy variable for if the game is a final
         ) |> 
  replace_na(list(final = 0)) |> # Somes of the latest round is yet to have updated data 
  left_join(games_played, by = c("date", "name")) |>  # Joining cumulative games played total 
  left_join(player_primary_position, by = "name") |> 
  select(date, season, round_number, name, rating_points, player_team_name, 
         team_won, final, games_played, position) |> 
  mutate(dummy = 1) |> 
  pivot_wider(names_from = position, 
              values_from = dummy, # Adding dummy variables for position
              values_fill = 0) |> 
  group_by(name, player_team_name) |> 
  filter(n() >= 30) |> # Only want players who have played at least 30 games for their club 
  ungroup() |> 
  mutate(std_rating_points = scale(rating_points), .by = c(name, player_team_name)) # Standardising player rating points (per club) to test what a standard deviation increase in their performance has on their team's probability of winning 


# Testing regression specifications 
testing <- df_player_barometer |> 
  filter(name == "Callum Wilkie")

# Simple regression
reg1 <- lm(team_won ~ rating_points, testing)
summary(reg1)

# Adding some control variables
reg2 <- lm(team_won ~ rating_points + final + games_played, testing)
summary(reg2)

# Standard deviation   

#-------------------------------------------------------------------------------
# Testing
set.seed(42)
library(dplyr)
library(tidyr)

# Parameters
n_matches <- 1000
n_players <- 200
seasons <- 2015:2023
opponents <- paste0("Team", LETTERS[1:18])  # 18 teams

# 1. Simulate match-level metadata
match_meta <- tibble(
  match_id = 1:n_matches,
  season = sample(seasons, n_matches, replace = TRUE),
  round = sample(1:23, n_matches, replace = TRUE),
  opponent = sample(opponents, n_matches, replace = TRUE),
  home_game = rbinom(n_matches, 1, 0.5),
  win = rbinom(n_matches, 1, 0.5)  # team won or lost
)

# 2. Simulate player performance data
# Each player appears in ~40% of matches randomly
player_performance <- expand.grid(
  match_id = 1:n_matches,
  player_id = paste0("player_", 1:n_players)
) %>%
  filter(runif(n()) < 0.4) %>%  # ~40% participation rate
  mutate(performance = rnorm(n(), mean = 50, sd = 10))  # mock stat like SuperCoach

# 3. Z-score standardization by player
player_performance <- player_performance %>%
  group_by(player_id) %>%
  mutate(perf_z = (performance - mean(performance, na.rm = TRUE)) / sd(performance, na.rm = TRUE)) %>%
  ungroup()

# 4. Pivot to wide format: 1 row per match, 1 column per player
perf_wide <- player_performance %>%
  select(match_id, player_id, perf_z) %>%
  pivot_wider(names_from = player_id, values_from = perf_z, values_fill = 0)

# 5. Merge in match-level metadata
model_data <- match_meta %>%
  left_join(perf_wide, by = "match_id") %>%
  mutate(
    season = as.factor(season),
    round = as.factor(round),
    opponent = as.factor(opponent)
  )
