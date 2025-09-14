# Preferred specifications 
# Run AFL-Barometer-Working.R first, at least until the construction of 
# df_player_barometer is done. 

# Trying to answer 2 questions: 
# 1. Barometer (compare players across teams fairly)
# 2. Most important player (absolute contribution within team)

#------------------------------------------------------------------------------
# Preparing data for last 2 seasons 
# restrict to recent window (last 2 seasons)
recent_df <- df_player_barometer %>%
  filter(season >= max(season) - 1)

# keep only players with enough games
player_counts <- recent_df %>%
  group_by(name) %>%
  summarise(n_games = n())

recent_df <- recent_df %>%
  left_join(player_counts, by = "name") %>%
  filter(n_games >= 15)

# Compute mean rating per player (between-player effect)
player_means <- recent_df %>%
  group_by(name) %>%
  summarise(rp_mean = mean(rating_points, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(rp_mean_z = as.numeric(scale(rp_mean)))  # standardise across all players


# compute centred & z-scored rating points
recent_df <- recent_df %>%
  left_join(player_means %>% select(name, rp_mean_z), by = "name") %>%
  group_by(name) %>%
  mutate(rp_c = rating_points - mean(rating_points, na.rm = TRUE),
         rp_c_z = as.numeric(scale(rp_c))) %>%  # within-player standardized deviation
  ungroup()

#------------------------------------------------------------------------------
# Barometer model, slopes relative to team
m_barometer <- glmer(
  team_won ~ rp_c_z + rp_mean_z + factor(player_team_name) +
    (0 + rp_c_z || name),
  data = recent_df,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Terms: 
# rp_c_z: The within player effect, game-to-game deviation. This fixed effect is the 
#         average effect across all player of playing above/below your own mean
# rp_mean_z: The between-player effect, fixed effect: on average, the mean effect
#            of player performance on winning (across all players)
# factor(player_team_name): team fixed-effect to control for player strength. We
#                           do not estimate player-specific intercepts as this 
#                           team effect would be highly correlated
# (0 + rp_c_z || name): Random slope for each player: estimates how senstiive 
#                       the team's win probability is to this specific player's
# performance i.e. 'barometer'

# Note: no random intercept per player because of collinearity with team effect. 
# We are not asking who is the best player in this regression but rather whose
# ups and downs matter most to the team's success

# The '||' in (0 + rp_c_z || name) basically just means that we estimate the 
# random slopes without estimating correlations with other effects (i.e. the 
# fixed effects). This just makes the model a bit more stable and less prone 
# to singular fits, in practice it doesn't matter too much 

# extract player-specific slopes
fixed_slope <- fixef(m_barometer)["rp_c_z"]

player_barometer <- ranef(m_barometer)$name %>%
  rownames_to_column("name") %>%
  mutate(total_slope = rp_c_z + fixed_slope) %>%
  arrange(desc(total_slope))

# Get player team names to get the biggest barometer for each team 
player_team <- recent_df |> 
  slice(1, .by = c(name, player_team_name)) |> 
  select(name, player_team_name)

team_barometers <- player_barometer |> 
  left_join(player_team, by = "name") |> 
  slice_max(order_by = total_slope, by = player_team_name, n = 3) |> 
  arrange(player_team_name, desc(total_slope))

head(player_barometer |> 
       left_join(player_team, by = "name") |> 
       filter(player_team_name == "St Kilda"), 10)   # top 10 barometer players

# Testing to see player's ratings in wins and losses: 
recent_df |> 
  filter(name == "Scott Pendlebury") |> 
  summarise(rating_points = mean(rating_points),
            .by = team_won)

#------------------------------------------------------------------------------
# Bootstrapping standard errors for the barometer regressions
# # function to extract total slopes vector for all players from a fitted model object
# get_total_slopes <- function(fit) {
#   fix_est <- fixef(fit)["rp_c_z"]
#   re_name <- ranef(fit)$name
#   # as vector of total slopes in same order as ranef
#   total <- as.numeric(re_name[,"rp_c_z"]) + fix_est
#   names(total) <- rownames(re_name)
#   total
# }
# 
# # run a parametric bootstrap: B replications
# B <- 1000   # 200-1000 typical; increase for more precise tails
# set.seed(123)
# 
# boot_res <- bootMer(
#   m_barometer,
#   FUN = function(fit) get_total_slopes(fit),
#   nsim = B,
#   type = "parametric",
#   use.u = FALSE,       # simulate from the model including random effects variance
#   parallel = "multicore", ncpus = 4   # adjust for your machine
# )
# 
# # boot_res$t is a B x G matrix (G = number of players kept in model)
# # compute mean (should be ~original total slopes) and SE and CIs
# boot_mat <- boot_res$t
# player_names <- colnames(boot_mat)   # ensure order
# boot_summary <- tibble(
#   player = player_names,
#   mean_boot = apply(boot_mat, 2, mean),
#   se_boot = apply(boot_mat, 2, sd),
#   ci_lower = apply(boot_mat, 2, quantile, probs = 0.025),
#   ci_upper = apply(boot_mat, 2, quantile, probs = 0.975)
# )
# 
# # join to original total slopes for comparison
# original_totals <- get_total_slopes(m_barometer) %>% enframe(name = "player", value = "total_slope")
# boot_summary <- left_join(boot_summary, original_totals, by = "player") %>%
#   arrange(desc(total_slope))


#------------------------------------------------------------------------------
# MVP Model (absolute impact)
# Gives us the within-team importance (baseline + slope)
m_mvp <- glmer(
  team_won ~ rp_c_z + rp_mean_z + (1 + rp_c_z || name),
  data = recent_df,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

fixefs <- fixef(m_mvp)

player_abs <- ranef(m_mvp)$name %>%
  rownames_to_column("player") %>%
  mutate(
    total_slope = rp_c_z + fixefs["rp_c_z"],
    total_intercept = `(Intercept)` + fixefs["(Intercept)"],
    total_effect_abs = total_intercept + total_slope
  ) %>%
  left_join(recent_df %>% select(name, player_team_name) %>% distinct(),
            by = c("player" = "name"))

# top player per team
team_mvp <- player_abs %>%
  group_by(player_team_name) %>%
  arrange(desc(total_effect_abs)) %>%
  slice_head(n = 3) |> 
  ungroup()

#------------------------------------------------------------------------------
# Diagnostics 
isSingular(m_barometer)   # should be FALSE
isSingular(m_mvp)

summary(m_barometer)
summary(m_mvp)

VarCorr(m_barometer)
VarCorr(m_mvp)

hist(player_barometer$total_slope, main="Distribution of Barometer Slopes")

