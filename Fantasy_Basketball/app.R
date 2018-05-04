#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(shiny)
library(magrittr)
all_data = read.csv("all_data.csv")
pg_data = read.csv("pg_data.csv")
c_data = read.csv("c_data.csv")
f_data = read.csv("f_data.csv")
gf_data = read.csv("gf_data.csv")
pf_data = read.csv("pf_data.csv")
psg_data = read.csv("psg_data.csv")
sf_data = read.csv("sf_data.csv")
sg_data = read.csv("sg_data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Fantasy Basketball Draft"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("position",
                        label = h5("Position"),
                        choices = c("Point Guard",
                        "Point / Shooting Guard",
                        "Shooting Guard",
                        "Forward",
                        "Guard / Forward",
                        "Small Forward",
                        "Power Forward",
                        "Center",
                        "All Positions")),
            numericInput("three_weight",
                         label = h5("Weight of 3PT"),
                         value = 1), 
            numericInput("two_weight",
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
    total_data = reactive({
        if(input$position == "Point Guard"){
            total = input$three_weight * pg_data$pred_Bhind_Arc + 
                input$two_weight * pg_data$pred_In_Arc + input$ft_weight * pg_data$pred_FreeT +
                input$ofr_weight * pg_data$pred_OfReb + input$dfr_weight * pg_data$pred_DefReb + 
                input$as_weight * pg_data$pred_Assist + input$st_weight * pg_data$pred_Steal +
                input$bl_weight * pg_data$pred_Block + input$tov_weight * pg_data$pred_TOV +
                input$fo_weight * pg_data$pred_Fouls
        }
        else if(input$position == "Point / Shooting Guard"){
            total = input$three_weight * psg_data$pred_Bhind_Arc + 
                input$two_weight * psg_data$pred_In_Arc + input$ft_weight * psg_data$pred_FreeT +
                input$ofr_weight * psg_data$pred_OfReb + input$dfr_weight * psg_data$pred_DefReb + 
                input$as_weight * psg_data$pred_Assist + input$st_weight * psg_data$pred_Steal + 
                input$bl_weight * psg_data$pred_Block + input$tov_weight * psg_data$pred_TOV +
                input$fo_weight * psg_data$pred_Fouls   
        }
        else if(input$position == "Shooting Guard"){
            total = input$three_weight * sg_data$pred_Bhind_Arc + 
                input$two_weight * sg_data$pred_In_Arc + input$ft_weight * sg_data$pred_FreeT +
                input$ofr_weight * sg_data$pred_OfReb + input$dfr_weight * sg_data$pred_DefReb + 
                input$as_weight * sg_data$pred_Assist + input$st_weight * sg_data$pred_Steal + 
                input$bl_weight * sg_data$pred_Block + input$tov_weight * sg_data$pred_TOV +
                input$fo_weight * sg_data$pred_Fouls  
            
        }
        else if(input$position == "Forward"){
            total = input$three_weight * f_data$pred_Bhind_Arc + 
                input$two_weight * f_data$pred_In_Arc + input$ft_weight * f_data$pred_FreeT +
                input$ofr_weight * f_data$pred_OfReb + input$dfr_weight * f_data$pred_DefReb + 
                input$as_weight * f_data$pred_Assist + input$st_weight * f_data$pred_Steal + 
                input$bl_weight * f_data$pred_Block + input$tov_weight * f_data$pred_TOV +
                input$fo_weight * f_data$pred_Fouls  
        }
        else if(input$position == "Guard / Forward"){
            total = input$three_weight * gf_data$pred_Bhind_Arc + 
                input$two_weight * gf_data$pred_In_Arc + input$ft_weight * gf_data$pred_FreeT +
                input$ofr_weight * gf_data$pred_OfReb + input$dfr_weight * gf_data$pred_DefReb + 
                input$as_weight * gf_data$pred_Assist + input$st_weight * gf_data$pred_Steal + 
                input$bl_weight * gf_data$pred_Block + input$tov_weight * gf_data$pred_TOV +
                input$fo_weight * gf_data$pred_Fouls  
        }
        else if(input$position == "Small Forward"){
            total = input$three_weight * sf_data$pred_Bhind_Arc + 
                input$two_weight * sf_data$pred_In_Arc + input$ft_weight * sf_data$pred_FreeT +
                input$ofr_weight * sf_data$pred_OfReb + input$dfr_weight * sf_data$pred_DefReb + 
                input$as_weight * sf_data$pred_Assist + input$st_weight * sf_data$pred_Steal + 
                input$bl_weight * sf_data$pred_Block + input$tov_weight * sf_data$pred_TOV +
                input$fo_weight * sf_data$pred_Fouls  
        }
        else if(input$position == "Power Forward"){
            total = input$three_weight * pf_data$pred_Bhind_Arc + 
                input$two_weight * pf_data$pred_In_Arc + input$ft_weight * pf_data$pred_FreeT +
                input$ofr_weight * pf_data$pred_OfReb + input$dfr_weight * pf_data$pred_DefReb + 
                input$as_weight * pf_data$pred_Assist + input$st_weight * pf_data$pred_Steal + 
                input$bl_weight * pf_data$pred_Block + input$tov_weight * pf_data$pred_TOV +
                input$fo_weight * pf_data$pred_Fouls 
        }
        else if(input$position == "Center"){
            total = input$three_weight * c_data$pred_Bhind_Arc + 
                input$two_weight * c_data$pred_In_Arc + input$ft_weight * c_data$pred_FreeT +
                input$ofr_weight * c_data$pred_OfReb + input$dfr_weight * c_data$pred_DefReb + 
                input$as_weight * c_data$pred_Assist + input$st_weight * c_data$pred_Steal + 
                input$bl_weight * c_data$pred_Block + input$tov_weight * c_data$pred_TOV +
                input$fo_weight * c_data$pred_Fouls
        }
        else if(input$position == "All Positions"){
            total = input$three_weight * all_data$pred_Bhind_Arc + 
                input$two_weight * all_data$pred_In_Arc + input$ft_weight * all_data$pred_FreeT +
                input$ofr_weight * all_data$pred_OfReb + input$dfr_weight * all_data$pred_DefReb + 
                input$as_weight * all_data$pred_Assist + input$st_weight * all_data$pred_Steal + 
                input$bl_weight * all_data$pred_Block + input$tov_weight * all_data$pred_TOV +
                input$fo_weight * all_data$pred_Fouls
        }
    })
    active_dataset = reactive({
        if(input$position == "Point Guard"){
            data.frame("Player" = pg_data$Name,"Predicted_Total_Points" = total_data())
        }
        else if(input$position == "Point / Shooting Guard"){
            data.frame("Player" = psg_data$Name,"Predicted_Total_Points" = total_data())
        }
        else if(input$position == "Shooting Guard"){
            data.frame("Player" = sg_data$Name,"Predicted_Total_Points" = total_data())
        }
        else if(input$position == "Forward"){
            data.frame("Player" = f_data$Name,"Predicted_Total_Points" = total_data())
        }
        else if(input$position == "Guard / Forward"){
            data.frame("Player" = gf_data$Name,"Predicted_Total_Points" = total_data())
        }
        else if(input$position == "Small Forward"){
            data.frame("Player" = sf_data$Name,"Predicted_Total_Points" = total_data())
        }
        else if(input$position == "Power Forward"){
            data.frame("Player" = pf_data$Name,"Predicted_Total_Points" = total_data())
        }
        else if(input$position == "Center"){
            data.frame("Player" = c_data$Name,"Predicted_Total_Points" = total_data())
        }
        else if(input$position == "All Positions"){
            data.frame("Player" = all_data$Name,"Predicted_Total_Points" = total_data())
        }
    })
    
    more_dataset = reactive({
        top_n(active_dataset(), input$obs, Predicted_Total_Points)
    })
    
    output$table = renderTable({
        more_dataset()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

