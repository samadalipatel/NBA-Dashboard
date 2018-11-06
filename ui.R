library(shiny)
ui <- fluidPage(
   
   # App Title
   titlePanel('NBA Team and Player Updates'),
   
   navbarPage('Pages',
              navbarMenu("Basic Statlines",
                         tabPanel("Tables",
                                  # Sidebar layout with input and output definitions 
                                  sidebarLayout(
                                     # Panel with inputs 
                                     sidebarPanel(
                                        # Input: Select Player 
                                        selectInput(inputId = 'Player',
                                                    label = 'Select a Player:',
                                                    choices = c('Giannis Antetokounmpo', 
                                                                'LeBron James', 
                                                                'Jaysen Tatum', 
                                                                'Kawhi Leonard',
                                                                'DeMar DeRozan')
                                        ),
                                        
                                        # Input: Select number of games for to view
                                        numericInput(inputId = 'Games',
                                                     label = 'Number of Games to View:',
                                                     value = 3, min = 1)
                                     ),
                                     # Main Panel for outputs 
                                     mainPanel(
                                        # Output; HTML table with requested number of observations 
                                        tableOutput('view')
                                     )
                                  )
                         ),
                         
                         tabPanel("Graphs",
                                  sidebarLayout(
                                     # Define Inputs
                                     sidebarPanel(
                                        # Input: Select player
                                        selectInput(inputId = 'PlayerGraph', 
                                                    label = 'Select a Player:', 
                                                    choices = c('Giannis Antetokounmpo', 
                                                                'LeBron James', 
                                                                'Jaysen Tatum', 
                                                                'Kawhi Leonard',
                                                                'DeMar DeRozan')
                                        ),
                                        
                                        # Input: Select games (slider)
                                        sliderInput(inputId = 'n_Games',
                                                    label = 'Number of Games to View:', 
                                                    min = 3,
                                                    max = 10, 
                                                    value = 5)
                                     ),
                                     # Define output
                                     mainPanel(plotOutput(outputId = 'statPlot'))
                                  )
                         )
              ),
              
              tabPanel('Box Scores', 
                       sidebarLayout(
                          # Define inputs
                          sidebarPanel(
                             helpText('View the box score of the nth most recent game. 1 = most recent, 
                                      2 = 2nd most recent, etc.', align = 'center'), 
                             # Input: Select team you want box score for 
                             selectInput(inputId = 'Team',
                                         label = 'Select a Team:', 
                                         choices = c('Hawks', 'Nets', 'Celtics', 'Hornets', 'Bulls', 
                                                     'Cavaliers', 'Mavericks', 'Nuggets', 'Pistons', 'Warriors', 
                                                     'Rockets', 'Pacers', 'Clippers', 'Lakers', 'Grizzlies', 
                                                     'Heat', 'Bucks', 'Timberwolves', 'Pelicans', 'Knicks', 
                                                     'Thunder', 'Magic', '76rs', 'Suns', 'Trailblazers', 
                                                     'Kings', 'Spurs', 'Raptors', 'Jazz', 'Wizards')
                             ), 
                             
                             # Input: Select game you want to view 
                             numericInput(inputId = 'nth_game_ago',
                                          label = 'Select last game you want to view:', 
                                          value = 1, min = 1), 
                             helpText('First five players are starters.', align = 'center')
                             ), 
                          
                          # Define output
                          mainPanel(uiOutput(outputId = 'date'), 
                                    uiOutput(outputId = 'outcome'), 
                                    DT::dataTableOutput(outputId = 'hometeam'), 
                                    DT::dataTableOutput(outputId = 'awayteam'))
                       )
   ),
   
   tabPanel('Standings', 
            fluidRow(
               # One half of the page should be eastern conference 
               column(5, h3('Eastern Conference'), 
                      tableOutput('east')),
               # Other half should be western conference - offset by 1 to have space
               column(5, offset = 1, h3('Western Conference'), 
                      tableOutput('west'))
            )
   )
   
   )
   
)
