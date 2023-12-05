# Create an ACC Teams vector
# Written without spaces to enable conference games
acc <- c("NotreDame", "Virginia", "VirginiaTech", "Miami", "GeorgiaTech", 
         "NorthCarolina", "BostonCollege", "FloridaState", "NCState", "Duke", 
         "Clemson", "WakeForest", "Pittsburgh", "Louisville")

# Aggregate player data

total_box <- combined_syracuse_box %>%
  # Remove non-player data
  filter(starters != "bench" & starters != "team" & starters != "") %>%
  mutate(starters = gsub(" III ", " ", starters)) %>%
  # Change Joe Girard's name
  separate(starters, c("first", "last", "pos"), " ") %>%
  # Split up starters variable into first and last name, and position
  left_join(player_names, by = "last") %>%
  # Join on player_names to change first initial to first name
  # Change column names
  mutate(first = first.y,
         player = paste(first, last), # Combine first and last name
         player = ifelse(player == "NA Ajak", "John Bol-Ajak", player)) %>%
          # Fix John Bol-Ajak
  select(-c(first.x, first.y, first, last)) %>% # Remove columns
  # Split up shooting stats
  separate(FG, c("fgm", "fga"), sep = "-") %>%
  separate(`3PT`, c ("fg3_m", "fg3_a"), sep ="-") %>%
  separate(FT, c("ftm", "fta"), sep = "-") %>%
  rename( # Rename columns
    mins = MIN,
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
  mutate(game = 1) %>% # Create a game variable
  group_by(player) %>% 
  # Group by player and calculate each game number
  mutate(gm_no = cumsum(game)) %>%
  ungroup() %>% # Ungroup to remove constraints
  left_join(cuse_games, by = c("gm_no" = "game_no")) %>%
  # Join game number on cuse_games to get opponent data
  # Create status and conference columns
  # Use grepl to find if the game is home/away
  mutate(status = ifelse(grepl("@", OPPONENT), "Away", "Home")) %>%
  # Use gsub and ifelse to calculate if the game is a conference game
  mutate(conf = ifelse(gsub("vs|@|[0-9]+| *", "", OPPONENT) %in% acc,
                       "Conference", "Non-Conference")) %>%
  select(-c(game, gm_no, DATE, OPPONENT, RESULT, `W-L (CONF)`, `Hi Points`, `Hi Rebounds`, `Hi Assists`))
  # Remove columns

# Create a cumulative game stats data frame

games_season_stats <- total_box %>%
  mutate(gp = 1) %>% # Create a value for game playerd
  group_by(player) %>% # Group by player
  summarise( # Choose the columns
    # Use "cumsum" to get cumulative sums as the season progresses
    player,
    mins = cumsum(mins),
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
    gm_no = cumsum(gp)
  ) %>%
  ungroup() %>% # Remove constraints
  summarise( # Create a new set of columns with rate stats
    player,
    mpg = mins/gm_no,
    ppg = pts/gm_no,
    oreb_pg = oreb/gm_no,
    dreb_pg = dreb/gm_no,
    rpg = reb/gm_no,
    apg = ast/gm_no,
    spg = stl/gm_no,
    bpg = blk/gm_no,
    tog = to/gm_no,
    pfg = pf/gm_no,
    gm_no,
    fg_pct, fg3_pct, ft_pct, ts_pct, efg_pct
  )

# Calculate season stats via splits

splits_stats <- total_box %>%
  group_by(player, status, conf) %>% # Group by player, status and conference
  summarise( # Create a set of columns
    mins = sum(as.numeric(mins)),
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
  mutate( # Create rate stats columns
    player,
    mpg = mins/games,
    ppg = pts/games,
    oreb_pg = oreb/games,
    dreb_pg = dreb/games,
    rpg = reb/games,
    apg = ast/games,
    spg = stl/games,
    bpg = blk/games,
    tog = to/games,
    pfg = pf/games,
    games,
    status,
    conf
  ) %>%
  ungroup() # Ungroup to remove constraints

# Calculate only status splits

status_stats <- total_box %>%
  group_by(player, status) %>% # Group by player and status
  summarise( # Create a set of columns
    mins = sum(as.numeric(mins)),
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
  mutate( # Calculate rate stats
    player,
    mpg = mins/games,
    ppg = pts/games,
    oreb_pg = oreb/games,
    dreb_pg = dreb/games,
    rpg = reb/games,
    apg = ast/games,
    spg = stl/games,
    bpg = blk/games,
    tog = to/games,
    pfg = pf/games,
    games,
    status,
    conf = "Both" # Set conference to both home/away
  ) %>%
  ungroup() # Remove constraints

# Calculate only conf splits

conf_stats <- total_box %>%
  group_by(player, conf) %>% # Group by player and conference
  summarise( # Create a set of columns
    mins = sum(as.numeric(mins)),
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
  mutate( # Calculate rate stats
    player,
    mpg = mins/games,
    ppg = pts/games,
    oreb_pg = oreb/games,
    dreb_pg = dreb/games,
    rpg = reb/games,
    apg = ast/games,
    spg = stl/games,
    bpg = blk/games,
    tog = to/games,
    pfg = pf/games,
    games,
    status =  "Both", # Set the status to both home/away
    conf
  ) %>%
  ungroup()

# Calculate overall season stats 

season_stats <- total_box %>%
  group_by(player) %>% # Calculate all season stats
  summarise( # Create a set of columns
    mins = sum(as.numeric(mins)),
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
  mutate( # Calculate rate stats
    player,
    mpg = mins/games,
    ppg = pts/games,
    oreb_pg = oreb/games,
    dreb_pg = dreb/games,
    rpg = reb/games,
    apg = ast/games,
    spg = stl/games,
    bpg = blk/games,
    tog = to/games,
    pfg = pf/games,
    games,
    status = "Both",
    conf = "Both")
  # Set status and conference equal to both

# Combine results into one data frame
combined_player_stats <- bind_rows(splits_stats, status_stats, conf_stats, season_stats)