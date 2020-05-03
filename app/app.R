
library(shiny)
library(ggplot2)
library(readr)
library(tidyverse)

Agg_dir <- "https://raw.githubusercontent.com/MyKo101/IPCW-Logistic/master/Aggregate%20Results"
Plot_dir <- "https://raw.githubusercontent.com/MyKo101/IPCW-Logistic/master/MainPlots"

#https://raw.githubusercontent.com/MyKo101/IPCW-Logistic/master/MainPlots/None/MainPlot_b%281%29_g%28-0.5%29_e%280.5%29.png

Done <- Agg_dir %>%
    paste0("/00-Done.csv") %>%
    read_csv(col_types = cols())%>%
    group_by(b,g,e) %>%
    summarise(n=n()) %>%
    ungroup

ui <- fluidPage(

    titlePanel("Calibration Bias Estimates"),

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
            textOutput("N_show"),
        width=3),

        # Show a plot of the generated distribution
        mainPanel(
            htmlOutput("MainPlot")
        )
    )
)

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
    
    
    output$MainPlot <- renderUI({
        slope <- switch(input$slope_choice,
                        `Calibration-in-the-large` = "None",
                        `Calibration Slope` = "Only",
                        `Both` = "All")
        
        
        Check.Done <- Done %>%
            filter(b==input$b &
                   g==input$g &
                   e==input$e)
        
        if(nrow(Check.Done)==0 || Check.Done$n < 6)
        {
            .filename <- paste0(Plot_dir,"/",
                                slope,"/",
                                "MainPlot",
                                "_b%28n%29",
                                "_g%28n%29",
                                "_e%28n%29",
                                ".png")
            
        } else {
            .filename <- paste0(Plot_dir,"/",
                                slope,"/",
                                "MainPlot",
                                "_b%28",input$b,"%29",
                                "_g%28",input$g,"%29",
                                "_e%28",input$e,"%29",
                                ".png")
        }
        
        return(tags$img(src=.filename,width=1200,height=600))
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
