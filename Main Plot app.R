#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(readr)

source("plot functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Calibration Bias Estimates"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("b",
                        "Value of beta",
                        min = -2,
                        max = 2,
                        value = 0,
                        step=0.5,
                        animate=animationOptions(interval=1500,loop=T)),
            sliderInput("g",
                        "Value of gamma",
                        min = -2,
                        max = 2,
                        value = 0,
                        step=0.5,
                        animate=animationOptions(interval=1500,loop=T)),
            sliderInput("e",
                        "Value of eta",
                        min = -1,
                        max = 1,
                        value = 0,
                        step=0.5,
                        animate=animationOptions(interval=1500,loop=T)),
            selectInput("slope_choice",
                        "Select Calibration type",
                        choices=c("Calibration-in-the-large",
                                  "Calibration Slope",
                                  "Both"))
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
        slope <- switch(input$slope_choice,
                        `Calibration-in-the-large` = "None",
                        `Calibration Slope` = "Only",
                        `Both` = "All")
        Make_MainPlot(input$b,input$g,input$e,"Aggregate Results",slope)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
