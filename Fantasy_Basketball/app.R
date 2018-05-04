#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)
pg_data = read.csv("pg_data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Fantasy Basketball Draft"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("position",
                        label = h5("Position"),
                        choices = list("Point Guard",
                        "Point / Shooting Guard",
                        "Shooting Guard",
                        "Forward",
                        "Guard / Forward",
                        "Small Forward",
                        "Power Forward",
                        "Center",
                        "All Positions")),
            numericInput("fg_weight",
                         label = h5("Weight of FG"),
                         value = 1),
            numericInput("3pt_weight",
                         label = h5("Weight of 3PT"),
                         value = 1), 
            numericInput("2pt_weight",
                         label = h5("Weight of 2PT"),
                         value = 1),
            numericInput("ft_weight",
                         label = h5("Weight of Free Throws"),
                         value = 1),
            numericInput("ofr_weight",
                         label = h5("Weight of Offensive Rebounds"),
                         value = 1),
            numericInput("dfr_weight",
                         label = h5("Weight of Defensive Rebounds"),
                         value = 1),
            numericInput("as_weight",
                         label = h5("Weight of Assists"),
                         value = 1),
            numericInput("st_weight",
                         label = h5("Weight of Steals"),
                         value = 1),
            numericInput("bl_weight",
                         label = h5("Weight of Blocks"),
                         value = 1),
            numericInput("tov_weight",
                         label = h5("Weight of Turnovers"),
                         value = 1),
            numericInput("fo_weight",
                         label = h5("Weight of Fouls"),
                         value = 1),
            numericInput("obs",
                         label = h6("Number of Players Shown in Table"),
                         value = 5),
            submitButton("Load Best Fantasy Picks")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            h3("Top Fantasy Picks"),
            tableOutput("table")
            )
    )
)



server <- function(input, output) {
    active_dataset = reactive({
        if(input$position == "Point Guard"){
            pg_data
        }
        else{
            
        }
    })
    finalized_data = reactive({
        total =  3pt_weight*pg_data$pred_Bhind_Arc + 2pt_weight*
    })
    
    output$table = renderTable({
        head(active_dataset(), n = input$obs)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

