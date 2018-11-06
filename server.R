library(shiny)
library(dplyr)
library(rvest)
library(tidyr)
library(ggplot2)
library(DT)
library(ggthemes)
library(stringr)

### Helper functions for the server ###
# Define function to generate graphs for each player
GraphStat <- function(df, n_Games){
   # Determine number of games to plot using input$n_Games
   df <- df[1:n_Games, ]
   # Place data in chronological order 
   df <- df[dim(df)[1]:1, ]
   # Change df into long format 
   df_long <- gather(data = df, key = stat_type, value = stat_value, 4:6)
   # Make stat_value numeric
   df_long$stat_value <- as.integer(df_long$stat_value)
   # Remove NAs
   df_long <- df_long %>% filter(!is.na(stat_value))
   # Create graph object
   ggplot(df_long, aes(Date, stat_value, group = stat_type, colour = stat_type)) + 
      geom_point() +  geom_line() + ylab('Value') + 
      ggtitle('Basic Stats Per Game') + 
      theme_hc(bgcolor = 'darkunica') + scale_colour_hc(palette = 'darkunica') + 
      scale_x_discrete('Opponent', labels = df_long$Opp) + 
      scale_y_continuous(breaks = seq(0, max(df_long$stat_value, na.rm=T) + 5, 5)) + 
      expand_limits(y = 0) + 
      scale_color_discrete('', breaks = c('PTS','TRB','AST'), 
                           labels = c('Points', 'Rebounds', 'Assists')) + 
      theme(plot.title = element_text(hjust = .5), panel.grid = element_line(linetype = 3)) 
}

# Define function to get box scores 
# [[1]], [[2]] are tables, [[3]] is outcome, [[4]] is date 
LatestBox <- function(team_abb, whichgame){
   # Generate url of team gamelog page 
   url <- paste('https://www.basketball-reference.com/teams/', 
                team_abb, '/2019/gamelog/', sep = '')
   # Instantiate html_session 
   session <- html_session(url)
   # Find table that contains game dates 
   game_table <- (session %>% html_nodes('#tgl_basic') %>% html_table())[[1]]
   # Remove first and second row, make second row column names
   colnames(game_table) <- game_table[1,]
   game_table <- game_table[-1, ]
   
   # Update whichgame to work for subsetting
   whichgame <- whichgame-1
   # Find opponent team abbreviation
   opp_team_abb <- game_table$Opp[nrow(game_table)-whichgame]
   # Use Date to find appropriate follow_link
   follow_link_date <- game_table$Date[nrow(game_table) - whichgame]
   
   # Using follow_link_date, navigate to team_abb box score page using session
   team_box <- (session %>% follow_link(follow_link_date) %>% 
                   html_nodes(paste('#box_', tolower(team_abb), '_basic', sep = '')) %>% 
                   html_table())[[1]]
   colnames(team_box) <- team_box[1,]
   team_box <- team_box[-1, ]
   # Remove column with reserves as name 
   team_box <- team_box %>% filter(Starters != 'Reserves')
   # Change first column name
   colnames(team_box)[1] <- 'Player'
   
   # Opp team box 
   opp_team_box <- (session %>% follow_link(follow_link_date) %>% 
                       html_nodes(paste('#box_', tolower(opp_team_abb), '_basic', sep = '')) %>% 
                       html_table())[[1]]
   colnames(opp_team_box) <- opp_team_box[1,]
   opp_team_box <- opp_team_box[-1, ]
   # Remove column with reserves as name 
   opp_team_box <- opp_team_box %>% filter(Starters != 'Reserves')
   # Change first column name
   colnames(opp_team_box)[1] <- 'Player'
   
   # Generate string of final score 
   final_score <- paste(team_abb, '-', team_box[nrow(team_box), 'PTS'], 
                        '          ', opp_team_box[nrow(opp_team_box), 'PTS'], '-', opp_team_abb)
   
   # Place them into a list to return
   returnlist <- list(team_box, opp_team_box, final_score, follow_link_date)
   names(returnlist) <- c(team_abb, opp_team_abb, 'Outcome', 'Date')
   return(returnlist)
}

# Server
server <- function(input, output){
   
   # Collect Data - Giannis
   page <- read_html('https://www.basketball-reference.com/players/a/antetgi01/gamelog/2019')
   giannis <- (page %>% html_nodes('#pgl_basic') %>% html_table(fill=TRUE))[[1]]
   giannis <- giannis[,c(3,7,10,28,22,23,13,16)]
   giannis <- giannis[nrow(giannis):1, ]
   
   # LeBron
   page <- read_html('https://www.basketball-reference.com/players/j/jamesle01/gamelog/2019/')
   lebron <- (page %>% html_nodes('#pgl_basic') %>% html_table(fill=TRUE))[[1]]
   lebron <- lebron[,c(3,7,10,28,22,23,13,16)]
   lebron <- lebron[nrow(lebron):1, ]
   
   # Tatum
   page <- read_html('https://www.basketball-reference.com/players/t/tatumja01/gamelog/2019/')
   tatum <- (page %>% html_nodes('#pgl_basic') %>% html_table(fill=TRUE))[[1]]
   tatum <- tatum[,c(3,7,10,28,22,23,13,16)]
   tatum <- tatum[nrow(tatum):1, ]
   
   # Kawhi
   page <- read_html('https://www.basketball-reference.com/players/l/leonaka01/gamelog/2019/')
   kawhi <- (page %>% html_nodes('#pgl_basic') %>% html_table(fill=TRUE))[[1]]
   kawhi <- kawhi[,c(3,7,10,28,22,23,13,16)]
   kawhi <- kawhi[nrow(kawhi):1, ]
   
   # DeRozan
   page <- read_html('https://www.basketball-reference.com/players/d/derozde01/gamelog/2019/')
   derozan <- (page %>% html_nodes('#pgl_basic') %>% html_table(fill=TRUE))[[1]]
   derozan <- derozan[,c(3,7,10,28,22,23,13,16)]
   derozan <- derozan[nrow(derozan):1, ]
   
   #######################
   ## BASIC STATS: TABLE # 
   #######################
   # Return the selected dataset 
   datasetInput <- reactive({
      switch(input$Player, 
             'Giannis Antetokounmpo' = giannis, 
             'LeBron James' = lebron, 
             'Jaysen Tatum' = tatum, 
             'Kawhi Leonard' = kawhi,
             'DeMar DeRozan' = derozan)
   })
   
   # Show n observations
   output$view <- renderTable({
      head(datasetInput(), n = input$Games)
   })
   
   #######################
   ## BASIC STATS: PLOTS # 
   #######################
   # Return the selected dataset
   dfInput <- reactive({
      switch(input$PlayerGraph,
             'Giannis Antetokounmpo' = giannis, 
             'LeBron James' = lebron, 
             'Jaysen Tatum' = tatum, 
             'Kawhi Leonard' = kawhi,
             'DeMar DeRozan' = derozan)
   })
   
   output$statPlot <- renderPlot({
      GraphStat(dfInput(), n_Games = input$n_Games)
   }
   )
   
   ################
   ## BOX SCORES ##
   ################
   # Transform input into abbreviation LatestBox can use 
   fixed_abb <- reactive({
      switch(input$Team, 
             'Hawks' = 'ATL', 'Nets' = 'BRK', 'Celtics' = 'BOS', 'Hornets' = 'CHO', 
             'Bulls' = 'CHI', 'Cavaliers' = 'CLE', 'Mavericks' = 'DAL', 'Nuggets' = 'DEN', 
             'Pistons' = 'DET', 'Warriors' = 'GSW', 'Rockets' = 'HOU', 'Pacers' = 'IND', 
             'Clippers' = 'LAC', 'Lakers' = 'LAL', 'Grizzlies' = 'MEM', 'Heat' = 'MIA', 
             'Bucks' = 'MIL', 'Timberwolves' = 'MIN', 'Pelicans' = 'NOP', 'Knicks' = 'NYK', 
             'Thunder' = 'OKC', 'Magic' = 'ORL', '76rs' = 'PHI', 'Suns' = 'PHO', 
             'Trailblazers' = 'POR', 'Kings' = 'SAC', 'Spurs' = 'SAS', 'Raptors' = 'TOR',
             'Jazz' = 'UTA', 'Wizards' = 'WAS'
      )
   })
   
   # Generate box scores for team user inputs 
   boxscores <- reactive({
      LatestBox(team_abb = fixed_abb(), whichgame = input$nth_game_ago)
   })
   
   # Output the date
   output$date <- renderUI(h4(
      paste0(month(boxscores()[[4]]), '/', day(boxscores()[[4]]), '/', 
             year(boxscores()[[4]])), align = 'center'))
   
   # Output the outcome
   output$outcome <- renderUI(h1(HTML(paste0("<pre>", boxscores()[[3]])), align = 'center'))
   
   # Output both tables
   output$hometeam <- DT::renderDataTable(
      datatable(boxscores()[[1]], options = list(scrollX = TRUE, scrollY = TRUE, dom = 't', 
                                                 pageLength = 30))
   )
   
   output$awayteam <- DT::renderDataTable(
      datatable(boxscores()[[2]], options = list(scrollX = TRUE, scrollY = TRUE, dom = 't', 
                                                 pageLength = 30))
   )
   
   
   ###############
   ## STANDINGS ##
   ###############
   # Load in page
   page <- read_html('https://www.basketball-reference.com/leagues/NBA_2019.html')
   # Extract eastern conference 
   eastern_conf <- (page %>% html_nodes('div') %>% 
                       html_nodes('#confs_standings_E') %>% html_table())[[1]]
   # Change column name to team
   colnames(eastern_conf)[c(1,2,3,4)] <- c('Team', 'Wins', 'Losses', 'Win/Loss Perc')
   # Remove rank from team name 
   eastern_conf$Team <- str_replace_all(eastern_conf$Team, pattern = '\\(.*', '') %>% 
      str_trim()
   # Create rank row
   eastern_conf$Rank <- 1:15
   # Select only relevant columns
   eastern_conf <- eastern_conf[,c(9, 1:4)]
   
   
   # Extract western conference
   western_conf <- (page %>% html_nodes('div') %>% 
                       html_nodes('#confs_standings_W') %>% html_table())[[1]]
   # Change column name to team
   colnames(western_conf)[c(1,2,3,4)] <- c('Team', 'Wins', 'Losses', 'Win/Loss Perc')
   # Remove rank from team name 
   western_conf$Team <- str_replace_all(western_conf$Team, pattern = '\\(.*', '') %>% 
      str_trim()
   # Create rank row
   western_conf$Rank <- 1:15
   # Select only relevant columns
   western_conf <- western_conf[,c(9, 1:4)]
   
   # Output tables 
   output$east <- renderTable(eastern_conf)
   output$west <- renderTable(western_conf)
   
}