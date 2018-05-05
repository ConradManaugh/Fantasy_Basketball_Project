#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("ggplot2")
library("ggplot2")
library("tidyr")
library(dplyr)
library(shiny)
library(magrittr)
graph_data = read.csv("DSP.csv")
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
            textInput("graph_player1",
                      label = h6("Search Player Statistics"),
                      value = "Enter Player Name..."),
            textInput("graph_player2",
                      label = h6("Player You Would Like to Compare to..."),
                      value = "Enter Second Player Name Here..."),
            selectInput("stat",
                        label = h6("Player Stat to Study"),
                        choices = c("3 Point Shots",
                                  "2 Point Shots",
                                  "Free Throws", 
                                  "Offensive Rebounds",
                                  "Defensive Rebounds",
                                  "Assists",
                                  "Steals",
                                  "Blocks",
                                  "Turnovers",
                                  "Fouls")),
            submitButton("Load Best Fantasy Picks")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Introduction", textOutput("intro")),
                tabPanel("Top Fantasy Picks", tableOutput("table")),
                tabPanel("Player Actual Stats and Prediction Stats Graph", plotOutput("player_graph")),
                tabPanel("Player Fantasy Point Contributions", plotOutput("ft_graph")),
                tabPanel("Player Comparison Graphs", plotOutput("comp_graph"))
            )
        )
    )
)




server <- function(input, output) {
    point_3P = reactive(input$three_weight * all_data$pred_Bhind_Arc)#Make a vector of points from each catagory
    point_2P = reactive(input$two_weight * all_data$pred_In_Arc)
    point_FreeT = reactive(input$ft_weight * all_data$pred_FreeT)
    point_OfR = reactive(input$ofr_weight * all_data$pred_OfReb)
    point_DfR = reactive(input$dfr_weight * all_data$pred_DefReb)
    point_As = reactive(input$as_weight * all_data$pred_Assist)
    point_St = reactive(input$st_weight * all_data$pred_Steal)
    point_Bl = reactive(input$bl_weight * all_data$pred_Block)
    point_Tov = reactive(input$tov_weight * all_data$pred_TOV)
    point_Fo = reactive(input$fo_weight * all_data$pred_Fouls)
    
    
    
    point_data=reactive({
        new_frame=all_data
        
        colnames(new_frame)=c("X1","Name","Position",
                              "Age","Year","Points_from_3","Points_from_2","Points_from_1",
                              "Points_from_Ofr","Points_from_Defr","Points_from_Assist","Points_from_Steal",
                              "Points_from_Block","Points_from_Tov","Points_from_Fouls")
        
        new_frame[,6]=point_3P()
        new_frame[,7]=point_2P()
        new_frame[,8]=point_FreeT()
        new_frame[,9]=point_OfR()
        new_frame[,10]=point_DfR()
        new_frame[,11]=point_As()
        new_frame[,12]=point_St()
        new_frame[,13]=point_Bl()
        new_frame[,14]=point_Tov()
        new_frame[,15]=point_Fo()
        new_frame["Total"]=point_3P()+point_2P()+point_FreeT()+point_OfR()+point_DfR()+point_As()+point_St()+point_Bl()+point_Tov()+point_Fo()
        new_frame
        
    })
    
    
    
    
    
    
    total_data = reactive({
        if(input$position == "Point Guard"){
    
            
            
            
            
            point_3P = reactive(input$three_weight * pg_data$pred_Bhind_Arc)#Make a vector of points from each catagory
            point_2P = reactive(input$two_weight * pg_data$pred_In_Arc)
            point_FreeT = reactive(input$ft_weight * pg_data$pred_FreeT)
            point_OfR = reactive(input$ofr_weight * pg_data$pred_OfReb)
            point_DfR = reactive(input$dfr_weight * pg_data$pred_DefReb)
            point_As = reactive(input$as_weight * pg_data$pred_Assist)
            point_St = reactive(input$st_weight * pg_data$pred_Steal)
            point_Bl = reactive(input$bl_weight * pg_data$pred_Block)
            point_Tov = reactive(input$tov_weight * pg_data$pred_TOV)
            point_Fo = reactive(input$fo_weight * pg_data$pred_Fouls)
            
            total = point_3P()+point_2P()+point_FreeT()+point_OfR()+point_DfR()+point_As()+point_St()+point_Bl()+point_Tov()+point_Fo()#Sum of all is total points
            
        }
        else if(input$position == "Point / Shooting Guard"){
         
            
            point_3P = reactive(input$three_weight * psg_data$pred_Bhind_Arc)#Make a vector of points from each catagory
            point_2P = reactive(input$two_weight * psg_data$pred_In_Arc)
            point_FreeT = reactive(input$ft_weight * psg_data$pred_FreeT)
            point_OfR = reactive(input$ofr_weight * psg_data$pred_OfReb)
            point_DfR = reactive(input$dfr_weight * psg_data$pred_DefReb)
            point_As = reactive(input$as_weight * psg_data$pred_Assist)
            point_St = reactive(input$st_weight * psg_data$pred_Steal)
            point_Bl = reactive(input$bl_weight * psg_data$pred_Block)
            point_Tov = reactive(input$tov_weight * psg_data$pred_TOV)
            point_Fo = reactive(input$fo_weight * psg_data$pred_Fouls)
            
            total = point_3P()+point_2P()+point_FreeT()+point_OfR()+point_DfR()+point_As()+point_St()+point_Bl()+point_Tov()+point_Fo()#Sum of all is total points
            
            
            
        }
        else if(input$position == "Shooting Guard"){
         
            
            point_3P = reactive(input$three_weight * sg_data$pred_Bhind_Arc)#Make a vector of points from each catagory
            point_2P = reactive(input$two_weight * sg_data$pred_In_Arc)
            point_FreeT = reactive(input$ft_weight * sg_data$pred_FreeT)
            point_OfR = reactive(input$ofr_weight * sg_data$pred_OfReb)
            point_DfR = reactive(input$dfr_weight * sg_data$pred_DefReb)
            point_As = reactive(input$as_weight * sg_data$pred_Assist)
            point_St = reactive(input$st_weight * sg_data$pred_Steal)
            point_Bl = reactive(input$bl_weight * sg_data$pred_Block)
            point_Tov = reactive(input$tov_weight * sg_data$pred_TOV)
            point_Fo = reactive(input$fo_weight * sg_data$pred_Fouls)
            
            total = point_3P()+point_2P()+point_FreeT()+point_OfR()+point_DfR()+point_As()+point_St()+point_Bl()+point_Tov()+point_Fo()#Sum of all is total points
            
            
            
        }
        else if(input$position == "Forward"){
           
            
            point_3P = reactive(input$three_weight * f_data$pred_Bhind_Arc)#Make a vector of points from each catagory
            point_2P = reactive(input$two_weight * f_data$pred_In_Arc)
            point_FreeT = reactive(input$ft_weight * f_data$pred_FreeT)
            point_OfR = reactive(input$ofr_weight * f_data$pred_OfReb)
            point_DfR = reactive(input$dfr_weight * f_data$pred_DefReb)
            point_As = reactive(input$as_weight * f_data$pred_Assist)
            point_St = reactive(input$st_weight * f_data$pred_Steal)
            point_Bl = reactive(input$bl_weight * f_data$pred_Block)
            point_Tov = reactive(input$tov_weight * f_data$pred_TOV)
            point_Fo = reactive(input$fo_weight * f_data$pred_Fouls)
            
            total = point_3P()+point_2P()+point_FreeT()+point_OfR()+point_DfR()+point_As()+point_St()+point_Bl()+point_Tov()+point_Fo()#Sum of all is total points
            
            
            
            
            
            
        }
        else if(input$position == "Guard / Forward"){
          
            
            point_3P = reactive(input$three_weight * gf_data$pred_Bhind_Arc)#Make a vector of points from each catagory
            point_2P = reactive(input$two_weight * gf_data$pred_In_Arc)
            point_FreeT = reactive(input$ft_weight * gf_data$pred_FreeT)
            point_OfR = reactive(input$ofr_weight * gf_data$pred_OfReb)
            point_DfR = reactive(input$dfr_weight * gf_data$pred_DefReb)
            point_As = reactive(input$as_weight * gf_data$pred_Assist)
            point_St = reactive(input$st_weight * gf_data$pred_Steal)
            point_Bl = reactive(input$bl_weight * gf_data$pred_Block)
            point_Tov = reactive(input$tov_weight * gf_data$pred_TOV)
            point_Fo = reactive(input$fo_weight * gf_data$pred_Fouls)
            
            total = point_3P()+point_2P()+point_FreeT()+point_OfR()+point_DfR()+point_As()+point_St()+point_Bl()+point_Tov()+point_Fo()#Sum of all is total points
            
            
            
            
            
        }
        else if(input$position == "Small Forward"){
            
            
            
            point_3P = reactive(input$three_weight * sf_data$pred_Bhind_Arc)#Make a vector of points from each catagory
            point_2P = reactive(input$two_weight * sf_data$pred_In_Arc)
            point_FreeT = reactive(input$ft_weight * sf_data$pred_FreeT)
            point_OfR = reactive(input$ofr_weight * sf_data$pred_OfReb)
            point_DfR = reactive(input$dfr_weight * sf_data$pred_DefReb)
            point_As = reactive(input$as_weight * sf_data$pred_Assist)
            point_St = reactive(input$st_weight * sf_data$pred_Steal)
            point_Bl = reactive(input$bl_weight * sf_data$pred_Block)
            point_Tov = reactive(input$tov_weight * sf_data$pred_TOV)
            point_Fo = reactive(input$fo_weight * sf_data$pred_Fouls)
            
            total = point_3P()+point_2P()+point_FreeT()+point_OfR()+point_DfR()+point_As()+point_St()+point_Bl()+point_Tov()+point_Fo()#Sum of all is total points
            
        }
        else if(input$position == "Power Forward"){

            
            
            point_3P = reactive(input$three_weight * pf_data$pred_Bhind_Arc)#Make a vector of points from each catagory
            point_2P = reactive(input$two_weight * pf_data$pred_In_Arc)
            point_FreeT = reactive(input$ft_weight * pf_data$pred_FreeT)
            point_OfR = reactive(input$ofr_weight * pf_data$pred_OfReb)
            point_DfR = reactive(input$dfr_weight * pf_data$pred_DefReb)
            point_As = reactive(input$as_weight * pf_data$pred_Assist)
            point_St = reactive(input$st_weight * pf_data$pred_Steal)
            point_Bl = reactive(input$bl_weight * pf_data$pred_Block)
            point_Tov = reactive(input$tov_weight * pf_data$pred_TOV)
            point_Fo = reactive(input$fo_weight * pf_data$pred_Fouls)
            
            total = point_3P()+point_2P()+point_FreeT()+point_OfR()+point_DfR()+point_As()+point_St()+point_Bl()+point_Tov()+point_Fo()#Sum of all is total points
        }
        else if(input$position == "Center"){
        
            point_3P = reactive(input$three_weight * c_data$pred_Bhind_Arc)#Make a vector of points from each catagory
            point_2P = reactive(input$two_weight * c_data$pred_In_Arc)
            point_FreeT = reactive(input$ft_weight * c_data$pred_FreeT)
            point_OfR = reactive(input$ofr_weight * c_data$pred_OfReb)
            point_DfR = reactive(input$dfr_weight * c_data$pred_DefReb)
            point_As = reactive(input$as_weight * c_data$pred_Assist)
            point_St = reactive(input$st_weight * c_data$pred_Steal)
            point_Bl = reactive(input$bl_weight * c_data$pred_Block)
            point_Tov = reactive(input$tov_weight * c_data$pred_TOV)
            point_Fo = reactive(input$fo_weight * c_data$pred_Fouls)
            
            
            total = point_3P()+point_2P()+point_FreeT()+point_OfR()+point_DfR()+point_As()+point_St()+point_Bl()+point_Tov()+point_Fo()#Sum of all is total points
        }
        else if(input$position == "All Positions"){
            point_3P = reactive(input$three_weight * all_data$pred_Bhind_Arc)#Make a vector of points from each catagory
            point_2P = reactive(input$two_weight * all_data$pred_In_Arc)
            point_FreeT = reactive(input$ft_weight * all_data$pred_FreeT)
            point_OfR = reactive(input$ofr_weight * all_data$pred_OfReb)
            point_DfR = reactive(input$dfr_weight * all_data$pred_DefReb)
            point_As = reactive(input$as_weight * all_data$pred_Assist)
            point_St = reactive(input$st_weight * all_data$pred_Steal)
            point_Bl = reactive(input$bl_weight * all_data$pred_Block)
            point_Tov = reactive(input$tov_weight * all_data$pred_TOV)
            point_Fo = reactive(input$fo_weight * all_data$pred_Fouls)
            total = point_3P()+point_2P()+point_FreeT()+point_OfR()+point_DfR()+point_As()+point_St()+point_Bl()+point_Tov()+point_Fo()#Sum of all is total points
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
    
    graph2_dataset = reactive({
        point_data() %>%
            subset(., Name == input$graph_player1)
    })
    
    reduced_graph2_dataset = reactive({
        graph2_dataset()[-c(1:5)]
    })
    
    tidy_graph2_dataset = reactive({
        gather(reduced_graph2_dataset(), key = "stat", value = "Player")
    })
    
    output$table = renderTable({
        more_dataset()[order(-more_dataset()$Predicted_Total_Points),]
    })
    
    output$intro = renderText({
        "This is a tool to use during an NBA Fantasy League Draft."
    })
    

    output$ft_graph = renderPlot({
        ggplot(tidy_graph2_dataset(),
               mapping = aes(stat, Player,fill="Identity")) +
            geom_bar(stat = "Identity")
            
            
            
        
    })
    output$player_graph=renderPlot({
        
        if(input$stat=="3 Point Shots"){
           
            ggplot(graph_data[(which(graph_data$Name==input$graph_player1)),],mapping=aes(x=Age, y=Bhind_Arc,color="Actual"))+
                geom_line(mapping=aes(x=Age,y=pred_Bhind_Arc,color="Predicted Curve"))+
                geom_point(mapping=aes(x=Age,y=pred_Bhind_Arc,color="Predicted"))+
                geom_point()+
                labs(title = paste0("Predicted 3 pointers made of ",input$graph_player1),
                     subtitle = "Based on data from 2014 to 2018",
                     x = "Age",
                     y ="3 Pointers Made",
                     color = "Actual or Predicted")}
        else if(input$stat=="2 Point Shots"){ 
            ggplot(graph_data[(which(graph_data$Name==input$graph_player1)),],mapping=aes(x=Age, y=In_Arc,color="Actual"))+
                geom_line(mapping=aes(x=Age,y=pred_In_Arc,color="Predicted Curve"))+
                geom_point(mapping=aes(x=Age,y=pred_In_Arc,color="Predicted"))+
                geom_point()+
                labs(title = paste0("Predicted 2 pointers made of ",input$graph_player1),
                     subtitle = "Based on data from 2014 to 2018",
                     x = "Age",
                     y ="2 Pointers Made",
                     color = "Actual or Predicted")}
        else if(input$stat=="Free Throws"){ 
            ggplot(graph_data[(which(graph_data$Name==input$graph_player1)),],mapping=aes(x=Age, y=FreeT,color="Actual"))+
                geom_line(mapping=aes(x=Age,y=pred_FreeT,color="Predicted Curve"))+
                geom_point(mapping=aes(x=Age,y=pred_FreeT,color="Predicted"))+
                geom_point()+
                labs(title = paste0("Predicted free throws made of ",input$graph_player1),
                     subtitle = "Based on data from 2014 to 2018",
                     x = "Age",
                     y ="Free Throws Made",
                     color = "Actual or Predicted")}
        else if(input$stat=="Offensive Rebounds"){ 
            ggplot(graph_data[(which(graph_data$Name==input$graph_player1)),],mapping=aes(x=Age, y=OfReb,color="Actual"))+
                geom_line(mapping=aes(x=Age,y=pred_OfReb,color="Predicted Curve"))+
                geom_point(mapping=aes(x=Age,y=pred_OfReb,color="Predicted"))+
                geom_point()+
                labs(title = paste0("Predicted Offensive Rebounds made of ",input$graph_player1),
                     subtitle = "Based on data from 2014 to 2018",
                     x = "Age",
                     y ="Offensive Rebounds",
                     color = "Actual or Predicted")}
        else if(input$stat=="Defensive Rebounds"){ 
            ggplot(graph_data[(which(graph_data$Name==input$graph_player1)),],mapping=aes(x=Age, y=DefReb,color="Actual"))+
                geom_line(mapping=aes(x=Age,y=pred_DefReb,color="Predicted Curve"))+
                geom_point(mapping=aes(x=Age,y=pred_DefReb,color="Predicted"))+
                geom_point()+
                labs(title = paste0("Predicted Defensive Rebounds made of ",input$graph_player1),
                     subtitle = "Based on data from 2014 to 2018",
                     x = "Age",
                     y =" Defensive Rebounds",
                     color = "Actual or Predicted")}
        else if(input$stat=="Assists"){ 
            ggplot(graph_data[(which(graph_data$Name==input$graph_player1)),],mapping=aes(x=Age, y=Assist,color="Actual"))+
                geom_line(mapping=aes(x=Age,y=pred_Assists,color="Predicted Curve"))+
                geom_point(mapping=aes(x=Age,y=pred_Assists,color="Predicted"))+
                geom_point()+
                labs(title = paste0("Predicted Assists made of ",input$graph_player1),
                     subtitle = "Based on data from 2014 to 2018",
                     x = "Age",
                     y =" Assists",
                     color = "Actual or Predicted")}
        else if(input$stat=="Steals"){ 
            ggplot(graph_data[(which(graph_data$Name==input$graph_player1)),],mapping=aes(x=Age, y=Steal,color="Actual"))+
                geom_line(mapping=aes(x=Age,y=pred_Steal,color="Predicted Curve"))+
                geom_point(mapping=aes(x=Age,y=pred_Steal,color="Predicted"))+
                geom_point()+
                labs(title = paste0("Predicted Steals made of ",input$graph_player1),
                     subtitle = "Based on data from 2014 to 2018",
                     x = "Age",
                     y =" Steals",
                     color = "Actual or Predicted")}
        else if(input$stat=="Blocks"){ 
            ggplot(graph_data[(which(graph_data$Name==input$graph_player1)),],mapping=aes(x=Age, y=Block,color="Actual"))+
                geom_line(mapping=aes(x=Age,y=pred_Block,color="Predicted Curve"))+
                geom_point(mapping=aes(x=Age,y=pred_Block,color="Predicted"))+
                geom_point()+
                labs(title = paste0("Predicted Blocks made of ",input$graph_player1),
                     subtitle = "Based on data from 2014 to 2018",
                     x = "Age",
                     y =" Blocks",
                     color = "Actual or Predicted")}
        
        else if(input$stat=="Fouls"){ 
            ggplot(graph_data[(which(graph_data$Name==input$graph_player1)),],mapping=aes(x=Age, y=Fouls,color="Actual"))+
                geom_line(mapping=aes(x=Age,y=pred_Fouls,color="Predicted Curve"))+
                geom_point(mapping=aes(x=Age,y=pred_Fouls,color="Predicted"))+
                geom_point()+
                labs(title = paste0("Predicted Fouls made of ",input$graph_player1),
                     subtitle = "Based on data from 2014 to 2018",
                     x = "Age",
                     y =" Fouls",
                     color = "Actual or Predicted")}
        else if(input$stat=="Turnovers"){ 
            ggplot(graph_data[(which(graph_data$Name==input$graph_player1)),],mapping=aes(x=Age, y=TOV,color="Actual"))+
                geom_line(mapping=aes(x=Age,y=pred_TOV,color="Predicted Curve"))+
                geom_point(mapping=aes(x=Age,y=pred_TOV,color="Predicted"))+
                geom_point()+
                labs(title = paste0("Predicted Turnovers made of ",input$graph_player1),
                     subtitle = "Based on data from 2014 to 2018",
                     x = "Age",
                     y =" Turnovers",
                     color = "Actual or Predicted")}
            

    
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

