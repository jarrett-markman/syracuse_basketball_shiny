# Aggregate team data

# Aggregate game-by-game team data

team_games_season <- combined_syracuse_box %>%
  filter(starters == "team") %>% # Filter team stats
  mutate(gm_no = 1:32) %>% # Create game number columns
  select(-c(starters, MIN)) %>% 
  # Remove starters and minutes columns
  # Separate columns for shot attempts
  separate(FG, c("fgm", "fga"), sep = "-") %>%
  separate(`3PT`, c ("fg3_m", "fg3_a"), sep ="-") %>%
  separate(FT, c("ftm", "fta"), sep = "-") %>%
  rename( # Rename columns
    oreb = OREB,
    dreb = DREB,
    reb = REB,
    ast = AST,
    stl = STL,
    blk = BLK,
    to = TO,
    pf = PF,
    pts = PTS
  ) %>%
  summarise( # Calculate season stats
    pts = cumsum(pts),
    oreb = cumsum(oreb),
    dreb = cumsum(dreb),
    reb = cumsum(reb),
    ast = cumsum(ast),
    to = cumsum(to),
    stl = cumsum(stl),
    blk = cumsum(blk),
    pf = cumsum(pf),
    fgm = cumsum(fgm),
    fga = cumsum(fga),
    fg3_m = cumsum(fg3_m),
    fg3_a = cumsum(fg3_a),
    ftm = cumsum(ftm),
    fta = cumsum(fta),
    fg_pct = cumsum(as.numeric(fgm))/cumsum(as.numeric(fga)),
    fg3_pct = cumsum(as.numeric(fg3_m))/cumsum(as.numeric(fg3_a)),
    ft_pct = cumsum(as.numeric(ftm))/cumsum(as.numeric(fta)),
    # Formulas according to basketball reference
    ts_pct = cumsum(as.numeric(pts)) / (2 * (cumsum(as.numeric(fga)) + .44 * cumsum(as.numeric(fta)))),
    efg_pct = (cumsum(fgm) + .5 * cumsum(fg3_m)) / cumsum(fga),
    gm_no
  ) %>%
  group_by(gm_no) %>% # Group by game number
  mutate( # Calculate rate stats
    ppg = pts/gm_no,
    oreb_pg = oreb/gm_no,
    dreb_pg = dreb/gm_no,
    rpg = reb/gm_no,
    apg = ast/gm_no,
    spg = stl/gm_no,
    bpg = blk/gm_no,
    tog = to/gm_no,
    pfg = pf/gm_no,
    gm_no
  ) %>%
  ungroup() # Remove constraints

# Create a total_team data frame
total_team <- combined_syracuse_box %>%
  filter(starters == "team") %>% # Filter team stats
  select(-c(starters, MIN)) %>% # Remove columns
  mutate(gm_no = 1:32) %>% # Create game number column
  # Join box on games to get opponent data
  left_join(cuse_games, by = c("gm_no" = "game_no")) %>%
  # Use grepl to find if "@" is in the opponent column
  mutate(status = ifelse(grepl("@", OPPONENT), "Away", "Home")) %>%
  # Split up the W/L column into record and conference
  separate(`W-L (CONF)`, c("record", "conference"), sep = " ", remove = F) %>%
  # Calculate if conference record changes to find conference/non-conference games
  mutate(conf = ifelse(conference != lag(conference), "Conference", "Non-Conference"),
         conf = ifelse(is.na(conf), "Non-Conference", conf)) %>%
  select(-c(gm_no, DATE, OPPONENT, RESULT, `W-L (CONF)`, record, conference, `Hi Points`, `Hi Rebounds`, `Hi Assists`)) %>%
  # Remove columns
  # Separate shot columns
  separate(FG, c("fgm", "fga"), sep = "-") %>%
  separate(`3PT`, c ("fg3_m", "fg3_a"), sep ="-") %>%
  separate(FT, c("ftm", "fta"), sep = "-") %>%
  rename( # Change column names
    oreb = OREB,
    dreb = DREB,
    reb = REB,
    ast = AST,
    stl = STL,
    blk = BLK,
    to = TO,
    pf = PF,
    pts = PTS
  )

# Find home/away, conf/non splits

splits_team <- total_team %>%
  group_by(status, conf) %>% # Group by status and conference
  summarise( # Calculate team splits stats
    pts = sum(as.numeric(pts)),
    oreb = sum(as.numeric(oreb)),
    dreb = sum(as.numeric(dreb)),
    reb = sum(as.numeric(reb)),
    ast = sum(as.numeric(ast)),
    stl = sum(as.numeric(stl)),
    blk = sum(as.numeric(blk)),
    to = sum(as.numeric(to)),
    pf = sum(as.numeric(pf)),
    fg_pct = sum(as.numeric(fgm))/sum(as.numeric(fga)),
    fg3_pct = sum(as.numeric(fg3_m))/sum(as.numeric(fg3_a)),
    ft_pct = sum(as.numeric(ftm))/sum(as.numeric(fta)),
    # Formulas according to basketball reference
    ts_pct = sum(as.numeric(pts)) / (2 * (sum(as.numeric(fga)) + .44 * sum(as.numeric(fta)))),
    efg_pct = (sum(as.numeric(fgm)) + .5 * sum(as.numeric(fg3_m)))/sum(as.numeric(fga)),
    games = n()
  ) %>%
  ungroup() # Remove constraints

# Calculate only status stats

combined_status_stats <- total_team %>%
  group_by(status) %>% # Group by team status
  summarise( # Calculate status stats
    pts = sum(as.numeric(pts)),
    oreb = sum(as.numeric(oreb)),
    dreb = sum(as.numeric(dreb)),
    reb = sum(as.numeric(reb)),
    ast = sum(as.numeric(ast)),
    stl = sum(as.numeric(stl)),
    blk = sum(as.numeric(blk)),
    to = sum(as.numeric(to)),
    pf = sum(as.numeric(pf)),
    fg_pct = sum(as.numeric(fgm))/sum(as.numeric(fga)),
    fg3_pct = sum(as.numeric(fg3_m))/sum(as.numeric(fg3_a)),
    ft_pct = sum(as.numeric(ftm))/sum(as.numeric(fta)),
    # Formulas according to basketball reference
    ts_pct = sum(as.numeric(pts)) / (2 * (sum(as.numeric(fga)) + .44 * sum(as.numeric(fta)))),
    efg_pct = (sum(as.numeric(fgm)) + .5 * sum(as.numeric(fg3_m)))/sum(as.numeric(fga)),
    games = n()
  ) %>%
  ungroup() %>% # Remove constraints
  mutate(conf = "Both") # Set conference equal to both

# Calculate only conference stats

combined_conf_stat <- total_team %>%
  group_by(conf) %>% # Group by conference
  summarise( # Calculate conference stats
    pts = sum(as.numeric(pts)),
    oreb = sum(as.numeric(oreb)),
    dreb = sum(as.numeric(dreb)),
    reb = sum(as.numeric(reb)),
    ast = sum(as.numeric(ast)),
    stl = sum(as.numeric(stl)),
    blk = sum(as.numeric(blk)),
    to = sum(as.numeric(to)),
    pf = sum(as.numeric(pf)),
    fg_pct = sum(as.numeric(fgm))/sum(as.numeric(fga)),
    fg3_pct = sum(as.numeric(fg3_m))/sum(as.numeric(fg3_a)),
    ft_pct = sum(as.numeric(ftm))/sum(as.numeric(fta)),
    # Formulas according to basketball reference
    ts_pct = sum(as.numeric(pts)) / (2 * (sum(as.numeric(fga)) + .44 * sum(as.numeric(fta)))),
    efg_pct = (sum(as.numeric(fgm)) + .5 * sum(as.numeric(fg3_m)))/sum(as.numeric(fga)),
    games = n()
  ) %>%
  ungroup() %>% # Remove constraints
  mutate(status = "Both") # Set status equal to both

# Find season results

season_team <- total_team %>%
  summarise( # Calculate overall season stats
    pts = sum(as.numeric(pts)),
    oreb = sum(as.numeric(oreb)),
    dreb = sum(as.numeric(dreb)),
    reb = sum(as.numeric(reb)),
    ast = sum(as.numeric(ast)),
    stl = sum(as.numeric(stl)),
    blk = sum(as.numeric(blk)),
    to = sum(as.numeric(to)),
    pf = sum(as.numeric(pf)),
    fg_pct = sum(as.numeric(fgm))/sum(as.numeric(fga)),
    fg3_pct = sum(as.numeric(fg3_m))/sum(as.numeric(fg3_a)),
    ft_pct = sum(as.numeric(ftm))/sum(as.numeric(fta)),
    # Formulas according to basketball reference
    ts_pct = sum(as.numeric(pts)) / (2 * (sum(as.numeric(fga)) + .44 * sum(as.numeric(fta)))),
    efg_pct = (sum(as.numeric(fgm)) + .5 * sum(as.numeric(fg3_m)))/sum(as.numeric(fga)),
    games = n()
  ) %>%
  mutate(status = "Both",
         conf = "Both")
  # Set status and conference equal to both

# Combine all statistics data frames into one
combined_team_stats <- bind_rows(splits_team, combined_conf_stat, combined_status_stats, season_team) %>%
  summarise( # Calculate stats
    status,
    conf,
    ppg = pts/games,
    oreb_pg = oreb/games,
    dreb_pg = dreb/games,
    rpg = reb/games,
    apg = ast/games,
    spg = stl/games,
    bpg = blk/games,
    tog = to/games,
    pfg = pf/games,
    fg_pct, fg3_pct, ft_pct, ts_pct, efg_pct,
    games
  ) %>% 
  mutate(
    team = "Syracuse" # Set the team = "Syracuse"
  )