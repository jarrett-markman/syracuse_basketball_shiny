# Create shiny

# Create ui

ui <- navbarPage(
  # Create page title
  "Syracuse Basketball Analytics",
  # Set page id = mainTab
  id = "mainTab",
  # Home Page
  tabPanel(
    # Title the tab
    "Home Page",
    # Create headers
    h4("Jarrett Markman, Ben Jennings, Andrew Jameison, Carter Strauss, Xiangning Wang", align = "center"),
    h4("SAL 413 Final Project", align = "center"),
    h4("Fall 2023", align = "center"),
    # Use HTML to get an image and center it
    HTML('<div style="display: flex; justify-content: center; align-items: center; height: 100vh;"><img src="https://upload.wikimedia.org/wikipedia/commons/4/49/Syracuse_Orange_logo.svg" style="max-width: 100%; max-height: 100%;"></div>')
  ),
  # Create tab 1
  tabPanel(
    "Team Overview",
    sidebarPanel(
      # Create a select input for the display
      selectInput("display", "Select Display: ", choices = c("Roster", "Schedule", "Recruiting Profile")),
      actionButton("submit_home", "Submit"),
      # Have a submit button to press
    ),
    mainPanel(
      dataTableOutput("overview_output") # Create a data table output
    )
  ),
  # Create tab 2
  tabPanel(
    "Team Stats",
    sidebarPanel(
      # Select team/split/conf inputs
      selectInput("team", "Select Team: ", choices = unique(combined_cuse_opp_stats$`Syracuse/Opponent`)),
      selectInput("split", "Select Home/Away: ", choices = unique(combined_cuse_opp_stats$`Home/Away`)),
      selectInput("conf", "Select Conference:", choices = unique(combined_cuse_opp_stats$Conference)),
      actionButton("submit_team", "Submit")
    ),
    mainPanel(
      dataTableOutput("team_output") # Create a data table output
    )
  ),
  # Create tab 3
  tabPanel(
    "Team Progression",
    sidebarPanel(
      # Select team/stat inputs
      selectInput("team_1", "Select Team: ", choices = unique(season_performance$`Syracuse/Opponent`)),
      selectInput("stat_1", "Select Stat: ", choices = colnames(season_performance[2:15])),
      actionButton("submit_team_pro", "Submit") 
    ),
    mainPanel(
      plotOutput("team_plot") # Create a plot output
    )
  ),
  # Create tab 4
  tabPanel(
    "Player Stats",
    sidebarPanel(
      # Select split/conf inputs
      selectInput("split_1", "Select Home/Away: ", choices = unique(combined_player_stats$`Home/Away`)),
      selectInput("conf_1", "Select Conference: ", choices = unique(combined_player_stats$Conference)),
      actionButton("submit_player", "Submit")
    ),
    mainPanel(
      dataTableOutput("player_output") # Create a data table output
    )
  ),
  # Create tab 5
  tabPanel(
    "Player Progression",
    sidebarPanel(
      # Select player/stat inputs
      selectInput("player_1", "Player: ", choices = unique(games_season_stats$Player)),
      selectInput("stat_2", "Select Stat: ", choices = colnames(games_season_stats[2:16])),
      actionButton("submit_player_pro", "Submit") 
    ),
    mainPanel(
      plotOutput("player_plot") # Create a plot input
    )
  ),
  # Create tab 6
  tabPanel(
    "Advanced Stats",
    mainPanel(dataTableOutput("adv_stats")) # Create a data table output
  )
)

# Create server

server <- function(input, output, session) {
  
  combined_cuse_opp_stats <- reactiveVal(combined_cuse_opp_stats)
  season_performance <- reactiveVal(season_performance)
  combined_player_stats <- reactiveVal(combined_player_stats)
  games_season_stats <- reactiveVal(games_season_stats)
  
  
  # Use options = list(searching = FALSE) to remove text box at bottom of data table output
  
  # Observe the submit button for the first tab
  # Use if and else if to set up to render each event display
  observeEvent(input$submit_home, {
    if (input$display == "Roster") {
      output$overview_output <- renderDataTable(
        roster_display, options = list(searching = FALSE)
      )
    } else if (input$display == "Schedule") {
      output$overview_output <- renderDataTable(
        games_display, options = list(searching = FALSE)
      )
    } else if (input$display == "Recruiting Profile") {
      output$overview_output <- renderDataTable(
        recruit_table, options = list(searching = FALSE)
      )
    }
  })
  
  # Observe the submit button for the second tab
  observeEvent(input$submit_team, {
    table <- combined_cuse_opp_stats() %>%
      # Add in inputs as filters
      filter(`Syracuse/Opponent` == input$team &
               `Home/Away` == input$split &
               Conference == input$conf) %>%
      select(-c(`Syracuse/Opponent`, `Home/Away`, Conference)) %>%
      # Remove filter inputs from the data set
      mutate_if(is.numeric, round, digits = 2) # If the variable is numeric round it 2 digits
    
    output$team_output <- renderDataTable(
      table, 
      options = list(searching = FALSE)
      # Render the data table
    )
  })
  
  # Observe the submit button for tab three
  observeEvent(input$submit_team_pro, {
    plot <- season_performance() %>%
      filter(`Syracuse/Opponent` == input$team_1) %>% # Filter for team input
      ggplot(aes(x = `Game Number`, y = .data[[input$stat_1]])) +
      # Set the y variable to the stat input
      geom_line(color = "#F76900", lwd = 2) + 
      # Change the line color to the hex code for syracuse orange
      labs(title = glue("{input$team_1} {input$stat_1} Over the Course of the Season")) +
      # Set the title = "Team Statistic Over the Course of the Season"
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5), # Bold text and center the title
        panel.background = element_rect(fill = "#000E54") 
        # Change the background color to syracuse blue
      )
    
    output$team_plot <- renderPlot({
      plot # Render the plot
    })
  })
  
  # Observe the submit button for tab four
  observeEvent(input$submit_player, {
    
    table <- combined_player_stats() %>%
      # Add in inputs as filters
      filter(`Home/Away` == input$split_1 & 
               Conference == input$conf_1) %>%
      select(-c(`Home/Away`, Conference)) %>% # Remove columns from the output
      arrange(desc(PPG)) # Arrange columns in decreasing order by ppg
    
    output$player_output <- renderDataTable(
      table, options = list(searching = FALSE)
      # Render the data table
    )
  })
  
  # Observe the submit button for tab five
  observeEvent(input$submit_player_pro, {
    
    plot <- games_season_stats() %>%
      # Filter for the player input
      filter(Player == input$player_1) %>%
      ggplot(aes(x = `Game Number`, y = .data[[input$stat_2]])) +
      # Set the y variable to the stat input
      geom_line(color = "#F76900", lwd = 2) +
      # Change the line color to the hex code for syracuse orange
      labs(title = glue("{input$player_1} {input$stat_2} Over the Course of the Season")) +
      # Set the title = "Player Statistic Over the Course of the Season"
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5), # Bold text and center the title
        panel.background = element_rect(fill = "#000E54") 
        # Set the background equal to syracuse blue
      )
    
    output$player_plot <- renderPlot({
      plot # Render plot
    })
  })
  
  output$adv_stats <- renderDataTable(
    adv_stats_table, options = list(searching = FALSE)
    # Render data table for the last tab
  )
}

# Create shiny

shinyApp(ui, server)
