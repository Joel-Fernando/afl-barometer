# PLotting based off preferred-regressions 

#------------------------------------------------------------------------------
# Plot: All player ratings then highlighing top 3 barometers from each club
# Prepare data
player_plot_df <- recent_df %>%
  select(name, player_team_name, rp_mean_z) %>%
  distinct() %>%
  left_join(player_barometer %>% select(name, total_slope), by = "name")

# Identify top 3 barometer players per team
top3_barometers <- player_plot_df %>%
  group_by(player_team_name) %>%
  arrange(desc(total_slope)) %>%
  slice_head(n = 3) %>%
  ungroup() %>%
  mutate(top3 = TRUE)

# Flag top3 vs others
player_plot_df <- player_plot_df %>%
  left_join(top3_barometers %>% select(name, top3), by = "name") %>%
  mutate(top3 = ifelse(is.na(top3), FALSE, TRUE))

# Plot
ggplot(player_plot_df, aes(x = rp_mean_z, y = player_team_name, color = player_team_name)) +
  geom_point(alpha = 0.7, size = 3) +
  # Add labels for top 3 barometer players
  geom_text_repel(
    data = subset(player_plot_df, top3),
    aes(label = name),
    size = 3,
    nudge_x = 0.1,
    segment.color = "grey50",
    max.overlaps = 20
  ) +
  labs(
    x = "Player mean rating (standardized)",
    y = "Team",
    color = "Team"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "right"
  ) +
  ggtitle("Top 3 Barometer Players vs Mean Player Ratings by Team")

ggsave(here("graphs", "top_3_barometer_team.jpg"))

#------------------------------------------------------------------------------
# Player ratings vs barometer effect (top 3 from each club)
# Assign colors: top 3 barometers get team colour, others grey
plot_df <- player_plot_df %>%
  mutate(
    color_plot = ifelse(top3, player_team_name, "Other")
  ) |> 
  mutate(player_team_name = fct_rev(player_team_name))

# Custom palette
teams <- unique(plot_df$player_team_name)
palette <- c(setNames(rainbow(length(teams)), teams), Other = "grey50")

# ------------------------------
# Plot with team background density
# ------------------------------
ggplot() +
  # 1. Violin: distribution of all players' mean ratings per team
  geom_violin(
    data = plot_df,
    aes(x = rp_mean_z, y = player_team_name),
    fill = "grey90",
  #  color = NA,
    alpha = 0.3
  ) +
  # 2. Overlay points: only top 3 barometer players per team
  geom_point(
    data = subset(plot_df, top3),
    aes(x = rp_mean_z, y = player_team_name, color = player_team_name, size = total_slope),
    alpha = 0.8
  ) +
  # 3. Label top 3 barometer players
  geom_text_repel(
    data = subset(plot_df, top3),
    aes(x = rp_mean_z, y = player_team_name, label = name),
    size = 3,
    nudge_x = 0.1,
    segment.color = "grey50",
    max.overlaps = 20
  ) +
  scale_color_manual(values = setNames(rainbow(length(unique(plot_df$player_team_name))),
                                       unique(plot_df$player_team_name))) +
  scale_size_continuous(name = "Total slope (barometer)", range = c(3,6)) +
  labs(
    x = "Player mean rating (standardized across all players)",
    y = "Team",
    color = "Team"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    legend.position = "right"
  ) +
  ggtitle("Top 3 Barometer Players vs Team Player Rating Distribution")

ggsave(here("graphs", "top_3_barometer_team_distribution.jpg"))
