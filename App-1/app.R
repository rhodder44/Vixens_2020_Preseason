# Source scripts, load libraries and read data sets at beginning of script
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
vixens_data <- read_csv("drill_2020_data.csv")

# Define UI
ui <- fluidPage(
  
  fluidRow(
  column(1, img(src = "logo.png", height = 80, width = 70)),
  column(5,
  
  
     
      
     
      selectInput("metric", 
                label   = "Variable",
                choices = c("PlayerLoad"          = "playerload_min",
                            "Jumps"               = "jumps_min",
                            "Explosive Efforts"   = "explosive_efforts_min",
                            "Accelerations"       = "accelerations_min",
                            "Decelerations"       = "decelerations_min",
                            "Change of Directions"= "change_of_directions_min"),
                selected = "PlayerLoad")
      
            )),
  
  
  
  fluidRow(
    
      
      plotOutput("outlier_plot", height = 700)
  ))



# Define server logic
server <- function(input, output) {
  
  

  output$outlier_plot <- renderPlot( {
    
   
 p <-   ggplot(data = vixens_data, aes_string(x = input$metric, colour = paste0(input$metric, "o"))) +
      geom_point(aes( y = drill, label = name) , size = 4.5) +
      geom_vline(xintercept = 100, linetype = "dashed") +
      scale_color_manual(values = c("#F8766D", "#619CFF", "grey80" )) +
   labs(color = "Outlier") +
   theme(text = element_text(size = 15, face = "bold"))
  
 p
    
  })
}


# Run the app
shinyApp(ui = ui, server = server)
