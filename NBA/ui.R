navbarPage("We Talk Fantasy Sports",
           tabPanel("Home"),
           navbarMenu("NBA",
                      tabPanel("Today's Projections",
                               sidebarLayout(
                                 sidebarPanel(
                                   helpText("Projections last updated at ", textOutput("mtime1", inline = T), " EST"),
                                   br(),
                                   radioButtons("site1", "Please select a site:",
                                                c("FanDuel" = "FD", "DraftKings" = "DK"), inline = T),
                                   br(),
                                   radioButtons("position1", "Please select a position:",
                                                c("PG", "SG", "SF", "PF", "C", "All"), inline = T)
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
                                                c("PG", "SG", "SF", "PF", "C", "All"), inline = T)
                                 ),
                                 mainPanel(
                                   plotOutput("resultsPlot", height = "600px"),
                                   br(),
                                   br(),
                                   dataTableOutput("resultsTable")
                                 )
                               ))
           ),
           tabPanel("MLB",
                    p("Season begins April 2, 2017")
           ),
           tabPanel("NFL",
                    p("Season begins September 7, 2017")
           ),
           tabPanel("About Us",
                    p(" We have one goal...to help you win your league. It does not matter if you are a 
                      fantasy rookie or a veteran. Everyone has questions, and we have opinions. 
                      Lots of them!"),
                    br(),
                    p("Throughout each fantasy season we will be here to provide quality information to 
                      help you make decisions that will win your leagues.  We will stay up to date with 
                      the latest news injuries and trades and comment on what each means to the fantasy 
                      sports world."),
                    br(),
                    p("Please make sure to comment, start discussions and chime in on other owners' 
                      questions!"),
                    br(),
                    p("If you like what you see and would like to help or just want to comment on the site, ",
                      a("email us", href = "mailto:keith@wetalkfantasysports.com"), ".")
                    ),
           tabPanel("Privacy Policy")
)