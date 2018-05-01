#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- pageWithSidebar(
    
    headerPanel("Fantasy Basketball "),
    
    sidebarPanel(
        
        checkboxGroupInput("position",
                           h4("Position"), 
                           list("Point Guard" = "PG",
                                "Point / Shooting Guard" = c("SG-PG"),
                                "Shooting Guard" = "SG",
                                "Forward" = c("F", "SF-PF", "PF-SF"),
                                "Guard / Forward" = c("G-F", "SG-SF"),
                                "Small Forward" = "SF",
                                "Power Forward" = "C-F",
                                "Center" = "C")
        ),
        
        checkboxGroupInput("category_reg", 
                           h4("Regular Categories*"), 
                           list("3PTM" ,
                                "PTS" ,
                                "REB" ,
                                "AST" ,
                                "ST" ,
                                "BLK",
                                "TOV"
                           )
        ),
        
        checkboxGroupInput("category_pct", 
                           h4("Percentage Categories"), 
                           list("FG" ,
                                "FT" 
                           )
        ),
        selectizeInput("Team",
                       h4("Team"),
                       list("ATL",
                            "BOS",
                            "BRK",
                            "CHI",
                            "CHO",
                            "CLE",
                            "DAL",
                            "DEN",
                            "DET",
                            "GSW",
                            "HOU",
                            "IND",
                            "LAC",
                            "LAL",
                            "MEM",
                            "MIA",
                            "MIL",
                            "MIN",
                            "NOP",
                            "NYK",
                            "OKC",
                            "ORL",
                            "PHI",
                            "PHO",
                            "POR",
                            "SAC",
                            "SAS",
                            "TOR",
                            "UTA",
                            "WAS"))
        
        
    ),
    
    mainPanel(
        tabsetPanel(
            tabPanel("Plot", plotOutput("graph")),
            tabPanel("Table", dataTableOutput("table"))
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output){
    
}
# Run the application 
shinyApp(ui = ui, server = server)

