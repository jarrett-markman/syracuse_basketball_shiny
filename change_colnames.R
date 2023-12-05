# Change column names

# Change roster into a display
roster_display <- roster %>%
  # Split up height into feet and inches via -
  separate(Height, c("feet", "inch"), sep = "-") %>%
  # Calculate height in inches
  mutate(Height = as.numeric(feet) * 12 + as.numeric(inch)) %>%
  select( # Select columns
    Player,
    Number = X.,
    Class,
    Position = Pos,
    Height,
    Weight,
    Hometown
  )
# Set class as factor levels
roster_display$Class <- factor(roster_display$Class, levels = c("SR", "JR", "SO", "FR"))

# Fix recruit table
recruit_table <- recruits %>%
  mutate( # Change player names
    PLAYER = ifelse(PLAYER == "Joe Girard", "Joseph Girard", PLAYER),
    PLAYER = ifelse(PLAYER == "John Bol-Ajak", "John Bol Ajak", PLAYER),
    PLAYER = ifelse(PLAYER == "Chris Bunch", "Chris Bell", PLAYER)
  ) %>% # Join w/ roster on player
  right_join(roster, by = c("PLAYER" = "Player")) %>%
  filter(!is.na(POS)) %>% # Remove players not in recruiting page
  # Players that have no recruitment data
  separate(HT, c("feet", "inch"), sep = "'") %>%
  # Separate height and calculate height in inches
  mutate(Height = as.numeric(feet) * 12 + as.numeric(inch)) %>%
  # Select columns
  select(Player = PLAYER,
         Position = POS,
         Height,
         Weight = WT,
         "Grade (ESPN)" = GRADE,
         "Recruitment Year" = YR,
         Class,
         "High School" = High.School)
# Set class as factor level
recruit_table$Class <- factor(recruit_table$Class, level = c("SR", "JR", "SO", "FR"))

# Create advanced stats table
adv_stats_table <- adv_stats %>%
  select( # Select column names
  # Change column names
   Player = Player...1, PER, ORtg, DRtg, FTr, "USG%" = USG., OWS, DWS, WS, OBPM, DBPM, BPM 
  ) %>%
  # Change column values for shiny table
  mutate(FTr = ifelse(FTr == "", NA, FTr),
         PER = as.numeric(PER)) %>%
  arrange(desc(PER)) # Arrange by PER


# Change cuse_games into a display
games_display <- cuse_games %>%
  rename( # Change column names
    date = DATE,
    record = `W-L (CONF)`,
    points = `Hi Points`,
    rebs = `Hi Rebounds`,
    ast = `Hi Assists`
  ) %>%
  mutate( # Change variables for opponent and game result
    opp = ifelse(grepl("vs", OPPONENT), gsub("vs", "", OPPONENT), gsub("@", "", OPPONENT)),
    opp =  gsub("\\*", "", opp),
    result = ifelse(grepl("W", RESULT), gsub("W", "W ", RESULT), gsub("L", "L ", RESULT))
  ) %>%
  select( # Select columns and change column names
    "Date" = date,
    "Opponent" = opp,
    "Result" = result,
    "Record (Conference)" = record,
    "Points Leader" = points,
    "Rebounds Leader" = rebs,
    "Assists Leader" = ast
  )

# Combine cuse/opp stats
combined_cuse_opp_stats <- bind_rows(combined_team_stats, combined_opp_stats) %>%
  summarise( # Change column names and calculate percentage stats
    "Home/Away" = status,
    "PPG" = ppg,
    "OREB/G" = oreb_pg,
    "DREB/G" = dreb_pg,
    "RPG" = rpg,
    "APG" = apg,
    "SPG" = spg,
    "BPG" = bpg,
    "TO/G" = tog,
    "PF/G" = pfg,
    "FG%" = fg_pct * 100,
    "3PT%" = fg3_pct * 100,
    "FT%" = ft_pct * 100,
    "TS%" = ts_pct * 100,
    "EFG%" = efg_pct * 100,
    "Syracuse/Opponent" = team,
    "Conference" = conf,
    Games = games
  )

# Combine season performance data
team_games_season <- team_games_season %>%
  mutate(team = "Syracuse") # Add column value of "Syracuse"
opp_games_season <- opp_games_season %>%
  mutate(team = "Opponent") # Add column value of "Opponent"
season_performance <- bind_rows(team_games_season, opp_games_season) %>%
  summarise( # Change column names and calculate percentage stats
    "Game Number" = gm_no,
    "Points Per Game" = ppg,
    "Offensive Rebounds Per Game" = oreb_pg,
    "Defensive Rebounds Per Game" = dreb_pg,
    "Rebounds Per Game" = rpg,
    "Assists Per Game" = apg,
    "Steals Per Game" = spg,
    "Blocks Per Game" = bpg,
    "Turnovers Per Game" = tog,
    "Fouls Per Game" = pfg,
    "Field Goal Percentage" = fg_pct * 100,
    "Three Point Field Goal Percentage" = fg3_pct * 100,
    "Free Throw Percentage" = ft_pct * 100,
    "True Shooting Percentage" = ts_pct * 100,
    "Effective Field Goal Percentage" = efg_pct * 100,
    "Syracuse/Opponent" = team
  )

# Combine syracuse player stats
combined_player_stats <- combined_player_stats %>%
  summarise( # Change column names and calculate percentage stats
    "Player" = player,
    "MPG" = mpg,
    "PPG" = ppg,
    "OREB/G" = oreb_pg,
    "DREB/G" = dreb_pg,
    "RPG" = rpg,
    "APG" = apg,
    "SPG" = spg,
    "BPG" = bpg,
    "TO/G" = tog,
    "PF/G" = pfg,
    "FG%" = fg_pct * 100,
    "3PT%" = fg3_pct * 100,
    "FT%" = ft_pct * 100,
    "TS%" = ts_pct * 100,
    "EFG%" = efg_pct * 100,
    "Home/Away" = status,
    "Conference" = conf,
    Games = games
  ) %>%
  mutate_if(is.numeric, round, digits = 2) # If the value is numeric, round 2 digits

# Game season stats
games_season_stats <- games_season_stats %>%
  summarise( # Change column names and calculate percentage stats
    "Player" = player,
    "Minutes Per Game" = mpg,
    "Points Per Game" = ppg,
    "Offensive Rebounds Per Game" = oreb_pg,
    "Defensive Rebounds Per Game" = dreb_pg,
    "Rebounds Per Game" = rpg,
    "Assists Per Game" = apg,
    "Steals Per Game" = spg,
    "Blocks Per Game" = bpg,
    "Turnovers Per Game" = tog,
    "Fouls Per Game" = pfg,
    "Field Goal Percentage" = fg_pct * 100,
    "Three Point Field Goal Percentage" = fg3_pct * 100,
    "Free Throw Percentage" = ft_pct * 100,
    "True Shooting Percentage" = ts_pct * 100,
    "Effective Field Goal Percentage" = efg_pct * 100,
    "Game Number" = gm_no
  ) %>%
  mutate_if(is.numeric, round, digits = 2) # If the value is numeric, round 2 digits