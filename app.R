#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(arules)
library(dplyr)
library(data.table)
library(DT)
library(shinyBS)
library(visNetwork)
library(emojifont)
library(shinyWidgets)
library(highcharter)
library(shinycssloaders)
library(shinyjs)
library(collapsibleTree)

# install.packages("collapsibleTree")

# Define UI for application that draws a histogram
ui <- fluidPage(

  
  img(src = "optum.png", height = 100, width = 200),
  #hr(),
  #HTML('<hr style="color: red;">'),
  tags$hr(style="border-color: orange;"),
   # Application title
   titlePanel("Project Roles"),
   #headerPanel("Miles Per Gallon"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
         # sliderInput("Options",
         #             "No of People",
         #             min = 1,
         #             max = 50,
         #             value = 10),
         
         
         # Input: Selector for variable to plot against mpg ----
         selectInput("variable", "Roles", 
                     c("Program Manager" = "PM",
                       "Business Analyst" = "BA",
                       "Product Owner" = "PO",
                       "Application Developer" = "AD",
                       "Data Analyst" = "DA")),
         
         selectInput("variable", "Business Title", 
                     c("Program Manager" = "PM",
                       "Business Analyst" = "BA",
                       "Product Owner" = "PO",
                       "Application Developer" = "AD",
                       "Data Analyst" = "DA")),
         
         # actionButton("action", "Submit"),
         
         textInput("recom", "Add Recommendation", ""),
         actionButton("submit", "Add"),
         textOutput("text"),
         numericInput("num", label = "Make changes", value = 1),
         dateInput("date2", "Date:"),
         dateInput("date3", "Date:", value = "Sys.Date()", format = "dd/mm/yy"),
         # Pass in a Date object
         dateInput("date4", "Date:", value = Sys.Date()-2),
         submitButton("Update View", icon("refresh")),
         helpText("When you click the button above, you should see",
                  "the output below update to reflect the value you",
                  "entered at the top:"),
         verbatimTextOutput("value")
         
         # submitButton("Submit")
         
         # Input: Checkbox for whether outliers should be included ----
         # checkboxInput("outliers", "Show outliers", TRUE),
         # radioButtons("radio", h3("Radio buttons"), choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1)
         
         
         
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
         plotOutput("distPlot")
      )
   )
)







# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   output$distPlot <- renderPlot({input$radioButtons})
   
   
   ######
   
   values <- reactiveValues(recom = NA)
   
   observeEvent(input$submit,{
     
     if(input$submit > 0) {
       
       values$recom <- isolate(input$recom)
       
     }
     
   })
   
   output$text <- renderText(print(as.numeric(input$submit)))
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

