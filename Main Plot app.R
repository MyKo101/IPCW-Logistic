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
library(tidyverse)

Agg_dir <- "Aggregate Results"
Plot_dir <- "MainPlots"

Done <- Agg_dir %>%
    paste0("/00-Done.csv") %>%
    read_csv(col_types = cols())%>%
    group_by(b,g,e) %>%
    summarise(n=n()) %>%
    ungroup

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
                                  "Both")),
            textOutput("N_show")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("MainPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    output$N_show <- renderText({
        N <- Done %>% 
            filter(b == input$b & 
                   g == input$g & 
                   e == input$e & 
                   n > 5) %>%
            pull("n") %>%
            max(0,na.rm=T)
        
        if(N == 0)
        {
            .res <- "No Results Yet"
        } else
        {
            .res <- paste0("Number of Simulations: ",N)
        }
    })

    output$MainPlot <- renderImage({
        slope <- switch(input$slope_choice,
                        `Calibration-in-the-large` = "None",
                        `Calibration Slope` = "Only",
                        `Both` = "All")
        
        .filename <- paste0(Plot_dir,"/",
                            slope,"/",
                            "MainPlot",
                            "_b(",input$b,")",
                            "_g(",input$g,")",
                            "_e(",input$e,")",
                            ".png")
        
        ww <- session$clientData$output_MainPlot_width
        hh <- session$clientData$output_MainPlot_height
        
        ww <- min(ww,hh*(20/10))
        hh <- min(hh,ww/(20/10))
        
        return(list(src=.filename,
                    alt = .filename,
                    width=ww,
                    height=hh))
        
    },deleteFile=F)
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
