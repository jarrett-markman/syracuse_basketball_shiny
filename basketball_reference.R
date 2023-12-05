# Get basketball reference data

# Store the url
url <- str_c("https://www.sports-reference.com/cbb/schools/syracuse/men/2023.html")
response <- GET(url) # Get the response
html_content <- content(response, "text") # Get the content from the html
html <- read_html(html_content) # Read in the hmtl
reference <- html %>%
  html_nodes(".table_wrapper") %>% # Follow the path
  html_table(header = TRUE) # Get a table header

# Get roster data
roster <- as.data.frame(reference[1]) # Get the [1] listed item and store it as a data frame
# Get player full names
player_names <- roster %>%
  separate(Player, c("first", "last")," ") %>% # Split player into first and last names
  select(first, last) # Select columns
# Get O and D ratings
ratings <- as.data.frame(reference[7]) %>% # Get the [7] listed item and store it as a data frame
  select(Player, ORtg, DRtg) %>% # Select columns
  head(17) # Select the first 17 observations based on the bref output
# Get player efficiency stats
adv_stats <- as.data.frame(reference[8]) %>% # Get the [8] listed item and store it as a data frame
  select(Player, PER, TS., eFG., FTr, USG., OWS, DWS, WS, OBPM, DBPM, BPM) %>%
  # Select columns
  head(17) # Select first 17 observations
adv_stats <-bind_cols(ratings, adv_stats) # Combine the columns