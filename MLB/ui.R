navbarPage("We Talk Fantasy Sports",
                      tabPanel("Today's Projections",
                               sidebarLayout(
                                 sidebarPanel(
                                   helpText("Projections last updated at ", textOutput("mtime1", inline = T),
                                            " EST"),
                                   br(),
                                   radioButtons("site1", "Please select a site:",
                                                c("FanDuel" = "FD", "DraftKings" = "DK"), inline = T),
                                   br(),
                                   radioButtons("position1", "Please select a position:",
                                                c("P", "C", "1B", "2B", "3B", "SS", "OF", "Batters"),
                                                inline = T)
                                 ),
                                 mainPanel(
                                   plotOutput("projPlot", height = "600px"),
                                   br(),
                                   br(),
                                   dataTableOutput("projTable")
                                 )
                               )
                      ),
                      tabPanel("Yesterday's Results",
                               sidebarLayout(
                                 sidebarPanel(
                                   helpText("Results last updated at ", textOutput("mtime2", inline = T), " EST"),
                                   br(),
                                   radioButtons("site2", "Please select a site:",
                                                c("FanDuel" = "FD", "DraftKings" = "DK"), inline = T),
                                   br(),
                                   radioButtons("position2", "Please select a position:",
                                                c("P", "C", "1B", "2B", "3B", "SS", "OF", "Batters"),
                                                inline = T)
                                 ),
                                 mainPanel(
                                   plotOutput("resultsPlot", height = "600px"),
                                   br(),
                                   br(),
                                   dataTableOutput("resultsTable")
                                 )
                               )
                      )
)