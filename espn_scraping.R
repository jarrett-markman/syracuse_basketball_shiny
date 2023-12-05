# Get espn game schedules

# Get the url
url <- str_c("https://www.espn.com/mens-college-basketball/team/schedule/_/id/183/season/2023")
response <- GET(url) # Get the response
html_content <- content(response, "text") # Get the content from the html
html <- read_html(html_content) # Read in the hmtl
schedules <- html %>% # Follow the path
  html_nodes(".Wrapper") %>%
  html_nodes(".pt0") %>%
  html_nodes(".ResponsiveTable") %>%
  html_nodes(".flex") %>%
  html_table(header = TRUE) # Get the header for the data
sch_df <- as.data.frame(schedules[1]) # Get the [1] listed item and store it as a data frame
colnames(sch_df) <- head(sch_df, 1) 
# Get the first observation (of the columns) and store them as the column names of the data frame
cuse_games <- sch_df %>%
  tail(32) %>% # Get the last 32 observations
  mutate(game_no = 1:32) %>% # Create a new column of the game number
  select(-`NA`) # Remove `NA` from the data set

# Get game ids

url <- str_c("https://www.espn.com/mens-college-basketball/team/schedule/_/id/183/season/2023")
# Store the url in "url"
game_ids <- read_html(url) %>% # Use read_html(url) to read in the URL
  html_nodes("a") %>% # Selects the anchors from the url
  html_attr("href") %>% # Extract the urls from the anchors
  str_subset("game/_/gameId") %>% # Subset the strings for "game/_/gameId" in the url 
  str_extract("\\d+$") # Take the values at the end of the url string

# Get recruit data

get_recruits <- function(season) {
  # Store the url given season
  url <- str_c(paste0("https://www.espn.com/college-sports/basketball/recruiting/school/_/id/183/class/", season, "/syracuse-orange"))
  # Use paste0 to combine the season url
  response <- GET(url) # Get the response
  html_content <- content(response, "text") # Get the content from the html
  html <- read_html(html_content) # Read in the hmtl
  html %>% # Follow the path
    html_nodes(".bg-opaque") %>%
    html_nodes(".span-6") %>%
    html_nodes(".mod-tabs") %>%
    html_table(header = TRUE) %>% # Set a header for the table
    as.data.frame() %>% # Store the table as a data frame
    mutate(PLAYER = str_remove(PLAYER, "Video \\| Scouts Report"),
           HT = str_remove(HT, "''"),
           YR = season,
           GRADE = as.numeric(GRADE)) %>%
    # Change column names and variable names
    select(-c(SCHOOL, LOCATION)) # Remove school and location
}
seasons <- 2019:2022 # Get the season values as a vector
recruitment_data <- map(seasons, get_recruits) # Map the values for seasons on the function
recruits <- do.call(bind_rows, recruitment_data)
# Combine all recruitment data

# Get box scores

get_box_score <- function(game_id) { # Create a function with a game id input
  game_url <- paste0("https://www.espn.com/mens-college-basketball/boxscore/_/gameId/",
                     game_id)
  # Use paste0 to concatenate the url string with game_id and store that in game_url
  response <- GET(game_url) # Make a GET request for the game_url
  if (response$status_code == 200) { # If the status code is ok, scrape the website
    game_content <- content(response, "text") # Get the content from the response
    game_html <- read_html(game_content) # Read the html code and path
    box_score <- game_html %>% # Pipe game_html into "box_score"
      # Find the path via inspect element to pull player and stat data
      html_nodes(".Wrapper") %>% 
      html_nodes(".Boxscore.flex.flex-column") %>%
      html_nodes(".ResponsiveTable") %>%
      html_nodes(".flex") %>%
      html_nodes(".Table") %>%
      html_table(header = TRUE)
    # The first and second elements in "box_score" are the results for the away team
    away_players <- box_score[1]
    away_stats <- box_score[2]
    away_box <- bind_cols(away_players, away_stats) 
    # Bind the player and stat columns together
    # The third and fourth elements in "box_score" are the results for the home team
    home_players <- box_score[3]
    home_stats <- box_score[4]
    home_box <- bind_cols(home_players, home_stats) 
    # Bind the player and stat columns together
    # Find the path for the name of the "away" team name
    away_team_name <- game_html %>%
      html_node(".Wrapper") %>%
      html_node(".Boxscore") %>%
      html_node(".Boxscore__Title") %>%
      html_node(".BoxscoreItem__TeamName") %>%
      html_text()
    # If the away team name is "Syracuse Orange"
    if (away_team_name == "Syracuse Orange") {
      # Store the box score values for the away team into "syracuse_box"
      syracuse_box = away_box 
      # Store the box score values for the home team into "opponent_box"
      opponent_box = home_box
    } else { 
      # If the away_team_name is not "Syracuse Orange" - Syracuse must be the home team
      syracuse_box = home_box
      # Store the box score values for the home team into "syracuse_box"
      opponent_box = away_box
      # Store the box score values for the away team into "opponent_box"
    }
    full_box_score <- list(syracuse_box, opponent_box) 
    # Store the data frames for "syracuse_box" and "opponent_box" into a list
    # So that only one element is returned
    return(full_box_score)
  } else { 
    print("INVALID URL") # If the status code is not okay, print "INVALID URL"
  }
}
# Pull all the game box scores
box_scores <- map(game_ids, get_box_score) # Map the vector of game_ids on the function
# Get only syracuse box scores
get_syracuse <- function(game) {
  game[[1]] # Take the first element from a game box score
}
# Get only opponent box scores
get_opponent <- function(game) {
  game[[2]]
}
# Get only the syracuse games
syracuse_box <- map(box_scores, get_syracuse) 
# Map the list values from box_scores through the get_syracuse function
# Combine all games and box score results into one data frame
combined_syracuse_box <- do.call(bind_rows, syracuse_box)

# Get only the opponent games
opp_box <- map(box_scores, get_opponent) 
# Map the list values from box_scores through the get_opponent function
# Combine all games and box score results into one data frame
combined_opp_box <- do.call(bind_rows, opp_box)