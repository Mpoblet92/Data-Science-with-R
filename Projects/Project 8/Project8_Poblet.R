#--------------------------------------------------------
# Marcel Poblet
# CSCI 3900C Data Science 
# Project 8
# Dr. Spence
# December 8, 2016
#--------------------------------------------------------
library(dplyr)
library(tidyr)
library(shiny)
library(plotly)

# Load Data frame
WeatherEvents <- read.csv("WeatherEvents.csv")

# Seperate top three events for health and safety.
healthDamage <- WeatherEvents %>%  group_by(EVENT) %>% 
                summarize(Fatalities = sum(FATALITIES), Injuries = sum(INJURIES)) %>% 
                mutate(Damage=Fatalities + Injuries) %>% 
                select(EVENT,Damage) %>% 
                ungroup(healthDamage) %>% 
                arrange(desc(Damage)) %>% 
                top_n(3, Damage)

# Seperate top three events of economic damage.
propertyDamage <- WeatherEvents %>% group_by(EVENT) %>%  
                  summarize(PropDamage = sum(PROPDMG), CropDamage = sum(CROPDMG)) %>% 
                  mutate(Damage = PropDamage + CropDamage) %>% 
                  select(EVENT,Damage) %>% 
                  ungroup(propertyDamage) %>% 
                  arrange(desc(Damage)) %>% 
                  top_n(3, Damage)
   
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   headerPanel("Weather Events Data"),
      
   # Show a plot of the selected category
      tabsetPanel(
        tabPanel('Health and Safety',
                 plotlyOutput("healthPlot")),
        tabPanel('Economic',
                 plotlyOutput("econPlot"))
        
      )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    # output plotly for top 3 health and safety events.
   output$healthPlot <- renderPlotly({
      plot_ly(data = healthDamage, 
               x=~EVENT, y=~Damage,
              mode = "markers", 
              type = "bar", 
              color = healthDamage$EVENT) %>% 
              layout(title = "Top 3 Events for Injuries and Death")
   })
   # outpu plotly for top 3 damaging economic events. 
   output$econPlot <- renderPlotly({
     plot_ly(data = propertyDamage, 
             x=~EVENT, y=~Damage, 
             mode = "markers", 
             type = "bar", 
             color = propertyDamage$EVENT) %>% 
             layout(title = "Top 3 Events of Economic Damage")
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

