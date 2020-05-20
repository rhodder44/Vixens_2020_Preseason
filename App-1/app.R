# Source scripts, load libraries and read data sets at beginning of script
library(gghighlight)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(rsconnect)
library(shinymanager)
library(ggrepel)
library(DescTools)


vixens_data <- read_csv("drill_2020_data.csv")
vixens_data2 <- gather(vixens_data, variable, variable_value, 6:11) %>%
  gather(variable_z, z_score, 6:11) %>%
  gather(variable_o, outlier_type, 6:11)

################
# inactivy check
################

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


###################
# login credentials
###################

# data.frame with credentials info
credentials <- data.frame(
  user = 1,                         #"vixendrill",
  password = 1,                    #Foxyvixen1!",
  stringsAsFactors = FALSE
)




# Define UI
ui <- secure_app(head_auth = tags$script(inactivity),
                 dashboardPage(
  
  dashboardHeader(title = "Vixens Drill Outliers"),
  
  dashboardSidebar(
                   br(),
                     
                   img(src = "logo.png", height = 160, width = 140, style="display: block; margin-left: auto; margin-right: auto;"),
                   
                   br(),
                   br(),
               
                   
                    selectInput("metric", 
                                label   = "Variable",
                                choices = c("PlayerLoad"          = "playerload_min",
                                            "Jumps"               = "jumps_min",
                                            "Explosive Efforts"   = "explosive_efforts_min",
                                            "Accelerations"       = "accelerations_min",
                                            "Decelerations"       = "decelerations_min",
                                            "Change of Directions"= "change_of_directions_min"),
                                selected = "PlayerLoad"),
                   
                   selectInput("player",
                               label   = "Player",
                               choices = c( "All"              = "All",
                                            "Allie Smith"       = "Allie Smith",
                                            "Caitlin Thwaites"  = "Caitlin Thwaites",
                                           "Elizabeth Watson"  = "Elizabeth Watson",
                                            "Elle McDonald"     = "Elle McDonald",
                                            "Emily Mannix"      = "Emily Mannix",
                                            "Jacqueline Newton" = "Jacqueline Newton",
                                            "Joanna Weston"     = "Joanna Weston",
                                            "Jordan Cransberg"  = "Jordan Cransberg",
                                            "Kadie Ann Dehaney" = "Kadie Ann Dehaney",
                                            "Kate Eddy"         = "Kate Eddy",
                                            "Kate Moloney"      = "Kate Moloney",
                                            "Mwai Kumwenda"     = "Mwai Kumwenda",
                                            "Sacha McDonald"    = "Sacha McDonald",
                                            "Tegan Philip"      = "Tegan Philip"),
                               selected = "Allie Smith"),
                   
                   
                   
                   selectInput("year", 
                               label    = "Year",
                               choices  = "2020",
                               selected = "2020")),
  
 
  dashboardBody(plotlyOutput("outlier_plot", height = 680),
                
                verbatimTextOutput("res_auth"))
  )
 
)




# Define server logic
server <- function(input, output) {
  
  
  
  # Login Authorization
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  
  filtered_data <- reactive({
    if (input$player == "All") {
    vixens_data
    } else {
      dplyr::filter(vixens_data, name == input$player)
    }
  })
  # Player select filter
  # players_filter <- reactive({
  #   if (input$player == "All") {
  #     vixens_data
  #   } else {
  #     vixens_data %>% dplyr::filter(name %in% input$player) 
  #   }})
    
    # i{
    #   player_select <- input$player
    #   m <- m %>% dplyr::filter(name == player_select)
    # }
    # 
    # m <- as.data.frame(m)
    # 
    # })
  

  # Reactive colours
  cols <- reactive( {
    cols <- c("HIGH" = "#F8766D", "LOW" =  "#619CFF", "NORMAL" =  "grey80")
    # cols[input$player] <- "red"
    return(cols)
  } )

output$outlier_plot <- renderPlotly( {
     
 print(
    ggplotly(
        ggplot(filtered_data(), aes_string(x = input$metric, color = paste0(input$metric, "o"), label = "name")) +
        geom_point(aes(y = drill,  size = 3.5)) +
        geom_vline(xintercept = 100, linetype = "dashed") +
        labs(color = "Outlier",
          x = "% Match Intensity",
          y = "Drill") +
    theme(text = element_text(size = 15, face = "bold")) +
    scale_color_manual(values = cols())
    ))
# partial_bundle(toWebGL(ggplotly(p)))
 

  })
}

##############
# ADD TAB WITH INSTRUCTIONS
##############

# Run the app
shinyApp(ui = ui, server = server)
