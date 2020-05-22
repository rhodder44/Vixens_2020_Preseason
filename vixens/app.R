# Source scripts, load libraries and read data sets at beginning of script
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(rsconnect)
library(shinymanager)



vixens_data <- read_csv("drill_2020_data.csv")

#################
# inactivy check
#################

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
  user = "vixens",                       
  password = "SNHC2020",                   
  stringsAsFactors = FALSE
)




# Define UI
ui <- secure_app(head_auth = tags$script(inactivity),
  
  dashboardPage(
  
    dashboardHeader(title = "Vixens Drill Outliers"),
  
    dashboardSidebar(
                   br(),
                     
                   img(src = "logo.png", height = 130, width = 113.75, style="display: block; margin-left: auto; margin-right: auto;"),
                   
                   br(),
                   br(),
              
                  
    sidebarMenu(id = "sidebarmenu",
              
      menuItem("Instructions", tabName = "a", icon = icon("info-circle")), 
      menuItem("Dashboard",    tabName = "b", icon = icon("dashboard")),
      br(),
      br(),
        conditionalPanel("input.sidebarmenu === 'b'",
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
                               selected = "All"),
                   
                   
                   
                   selectInput("year", 
                               label    = "Year",
                               choices  = "2020",
                               selected = "2020")
                   )
      )
                    
  ),
  
   dashboardBody(
     tabItems(
       
     tabItem(tabName = "a",
             htmlOutput("video")),
     
             # tags$video(id = "video1", src ="AppTutorialNews.mp4", type = "video/mp4",  controls = "controls", height = 250)),
             
    
      tabItem(tabName = "b",
              
              plotlyOutput("outlier_plot", height = 600),
                
                verbatimTextOutput("res_auth"))
      
    
  )
  )
  ))
  
 





# Define server logic
server <- function(input, output) {
  
  # video output
  output$video <- renderUI({
    HTML('<iframe width="1000" height="600" src="https://www.youtube.com/embed/sIbvFjRNVFE?rel=0&autoplay=1" frameborder="2" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  })
  
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
    return(cols)
  } )

output$outlier_plot <- renderPlotly( {
     
 print(
    ggplotly(
        ggplot(filtered_data(), aes_string(x = input$metric, color = paste0(input$metric, "o"), label = "name")) +
        geom_point(aes(y = drill),  size = 3.5) +
        geom_vline(xintercept = 100, linetype = "dashed") +
        labs(color = "Outlier",
          x = "% Match Intensity",
          y = "Drill") +
    theme(text = element_text(size = 15, face = "bold")) +
    scale_color_manual(values = cols()) +
    theme(legend.title = element_blank())) %>%
      layout(legend = list(orientation = "v",
                           y = 0.5, x = 1)))
# partial_bundle(toWebGL(ggplotly(p)))
 

  })
}

##############
# ADD TAB WITH INSTRUCTIONS
##############

# Run the app
shinyApp(ui = ui, server = server)
