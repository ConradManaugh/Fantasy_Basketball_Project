#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
library("ggplot2")
library("tidyr")
library("dplyr")
library("shiny")
library("magrittr")
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
                        choices = c("All Positions","Point Guard",
                        "Point / Shooting Guard",
                        "Shooting Guard",
                        "Forward",
                        "Guard / Forward",
                        "Small Forward",
                        "Power Forward",
                        "Center")),
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
                tabPanel("Introduction", verbatimTextOutput("intro")),
                tabPanel("Top Fantasy Picks", tableOutput("table")),
                tabPanel("Player Actual Stats and Prediction Stats Graph", plotOutput("player_graph")),
                tabPanel("Player Fantasy Point Contributions", plotOutput("ft_graph")),
                tabPanel("Player Comparison Graphs of Prediction Stats", plotOutput("comp_graph"))
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
                              "Age","Year","Three_Points","Two_Points","Free_Throws",
                              "Offensive_Rebounds","Defensive_Rebounds","Assists","Steals",
                              "Blocks","Turnovers","Fouls")
        
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
    
    active_dataset = reactive({ # Creating a dataset for each position containing players total predicted fantasy point for the next season
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
    
    table_dataset = reactive({ # Making a dataset with the top players based on their predicted fantasy score for the table output
        top_n(active_dataset(), input$obs, Predicted_Total_Points)
    })
    
    graph2_dataset = reactive({ # Subsetting the point_data() based on the input data and tidying the dataset 
        point_data() %>%
            subset(., Name == input$graph_player1) %>%
            .[-c(1:5)] %>%
            gather(., key = "stat", value = "player")
    })
    
    graph3_dataset = reactive({ # Subsetting point_data() based on inputs
        point_data() %>%
            subset(., Name ==input$graph_player1 | Name == input$graph_player2)
    })
    
    output$intro = renderText({
paste("This is a tool to use during an NBA Fantasy League Draft to assist", "in picking players with the highest fantasy points predicted for the","2019 season based on your league's scoring system.","","To use this app, first, choose a position that you are looking to draft","and enter the weight of each statistic (how many points each statistic","is worth in your league) and enter the number of top performing player","you would like to see in the Number of Players Shown in Table input.","","For example, if each turnover is worth minus one fantasy point put -1","in the Weight of Turnovers input then press enter and click on the tab","title Top Fantasy Picks where you will find a table ranking the players","we predicted will have the top total fantasy points based on your","leagues scoring rules.","","Next, based on the table, you can enter a players name into the Search","Player Prediction input (Warning: be sure to enter the players name how","it appears in the table) and choose a particular statistic in the Player","Stat to Study input. Once you hit submit go to the tab titled Player","Actual Stats and Prediction Stats Graph and you will find a graph of","that players predicted and actual performance of that particular","statistic in the past five seasons.","","You can also go to the tab titled Player Fantasy Point Contribution to","find that players predicted fantasy score for each individual statistic","in the 2019 season.","","Finally, you can enter a second players name into the Player You Would","Like to Compare to input and change the statistic if you desire. Once","you press submit you can go to the final tab and will find a graph","comparing the predicted fantasy score for the 2019 season for each","player.", sep="\n")     # Output for the first tab of the app 
 })
    
    output$table = renderTable({    # Output for the second tab of the app
        table_dataset()[order(-table_dataset()$Predicted_Total_Points),]
    })
    
    output$player_graph=renderPlot({    # Graph output for the third tab of the app
        
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
                labs(title = paste0("Predicted Defensive Rebounds made by ",input$graph_player1),
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
    
    output$ft_graph = renderPlot({  # Graph output for the fourth tab of the app
        ggplot(graph2_dataset(),
               mapping = aes(stat, player, fill = stat)) +
            geom_bar(stat = "Identity") +
            labs(title = paste0("Predicted 2019 Fantasy Point ",input$graph_player1),
                 subtitle = "All Stats Considered and Total",
                 x = "Stat",
                 y = "Predicted Fantasy Points",
                 color = "Stat") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        
        
        
    })
    
    output$comp_graph = renderPlot({    # Graph output for the fifth tab of the app 
        if(input$stat == "3 Point Shots"){
            ggplot(graph3_dataset(),
                   mapping = aes(x = Name, y = Three_Points, fill = Name)) +
                geom_bar(stat = "Identity") +
                labs(title = paste0("2019 Fantasty ", input$stat, " Prediction"),
                     subtitle = paste0("Comparison of ",input$graph_player1, " and ", input$graph_player2),
                     x = "Player",
                     y = paste0("Predicted", input$stat, " Fantasy Points"),
                     color = "Player")
            
        }
        else if(input$stat == "2 Point Shots"){
            ggplot(graph3_dataset(),
                   mapping = aes(x = Name, y = Two_Points, fill = Name)) +
                geom_bar(stat = "Identity") +
                labs(title = paste0("2019 Fantasty ", input$stat, " Prediction"),
                     subtitle = paste0("Comparison of ",input$graph_player1, " and ", input$graph_player2),
                     x = "Player",
                     y = paste0("Predicted", input$stat, " Fantasy Points"),
                     color = "Player")
        }
        else if(input$stat == "Free Throws"){
            ggplot(graph3_dataset(),
                   mapping = aes(x = Name, y = Free_Throws, fill = Name)) +
                geom_bar(stat = "Identity") +
                labs(title = paste0("2019 Fantasty ", input$stat, " Prediction"),
                     subtitle = paste0("Comparison of ",input$graph_player1, " and ", input$graph_player2),
                     x = "Player",
                     y = paste0("Predicted", input$stat, " Fantasy Points"),
                     color = "Player")
        }
        else if(input$stat == "Offensive Rebounds"){
            ggplot(graph3_dataset(),
                   mapping = aes(x = Name, y = Offensive_Rebounds, fill = Name)) +
                geom_bar(stat = "Identity") +
                labs(title = paste0("2019 Fantasty ", input$stat, " Prediction"),
                     subtitle = paste0("Comparison of ",input$graph_player1, " and ", input$graph_player2),
                     x = "Player",
                     y = paste0("Predicted", input$stat, " Fantasy Points"),
                     color = "Player")
        }
        else if(input$stat == "Defensive Rebounds"){
            ggplot(graph3_dataset(),
                   mapping = aes(x = Name, y = Defensive_Rebounds, fill = Name)) +
                geom_bar(stat = "Identity") +
                labs(title = paste0("2019 Fantasty ", input$stat, " Prediction"),
                     subtitle = paste0("Comparison of ",input$graph_player1, " and ", input$graph_player2),
                     x = "Player",
                     y = paste0("Predicted", input$stat, " Fantasy Points"),
                     color = "Player")
        }
        else if(input$stat == "Assists"){
            ggplot(graph3_dataset(),
                   mapping = aes(x = Name, y = Assists, fill = Name)) +
                geom_bar(stat = "Identity") +
                labs(title = paste0("2019 Fantasty ", input$stat, " Prediction"),
                     subtitle = paste0("Comparison of ",input$graph_player1, " and ", input$graph_player2),
                     x = "Player",
                     y = paste0("Predicted", input$stat, " Fantasy Points"),
                     color = "Player")
        }
        else if(input$stat == "Steals"){
            ggplot(graph3_dataset(),
                   mapping = aes(x = Name, y = Steals, fill = Name)) +
                geom_bar(stat = "Identity") +
                labs(title = paste0("2019 Fantasty ", input$stat, " Prediction"),
                     subtitle = paste0("Comparison of ",input$graph_player1, " and ", input$graph_player2),
                     x = "Player",
                     y = paste0("Predicted", input$stat, " Fantasy Points"),
                     color = "Player")
        }
        else if(input$stat == "Blocks"){
            ggplot(graph3_dataset(),
                   mapping = aes(x = Name, y = Blocks, fill = Name)) +
                geom_bar(stat = "Identity") +
                labs(title = paste0("2019 Fantasty ", input$stat, " Prediction"),
                     subtitle = paste0("Comparison of ",input$graph_player1, " and ", input$graph_player2),
                     x = "Player",
                     y = paste0("Predicted", input$stat, " Fantasy Points"),
                     color = "Player")
        }
        else if(input$stat == "Turnovers"){
            ggplot(graph3_dataset(),
                   mapping = aes(x = Name, y = Turnovers, fill = Name)) +
                geom_bar(stat = "Identity") +
                labs(title = paste0("2019 Fantasty ", input$stat, " Prediction"),
                     subtitle = paste0("Comparison of ",input$graph_player1, " and ", input$graph_player2),
                     x = "Player",
                     y = paste0("Predicted", input$stat, " Fantasy Points"),
                     color = "Player")
        }
        else if(input$stat == "Fouls"){
            ggplot(graph3_dataset(),
                   mapping = aes(x = Name, y = Fouls, fill = Name)) +
                geom_bar(stat = "Identity") +
                labs(title = paste0("2019 Fantasty ", input$stat, " Prediction"),
                     subtitle = paste0("Comparison of ",input$graph_player1, " and ", input$graph_player2),
                     x = "Player",
                     y = paste0("Predicted", input$stat, " Fantasy Points"),
                     color = "Player")
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

