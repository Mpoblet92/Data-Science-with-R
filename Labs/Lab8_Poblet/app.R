#--------------------------------------------------------
# Marcel Poblet
# CSCI 3900C Data Science 
# Lab 8
# Dr. Spence
# November 29, 2016
#--------------------------------------------------------

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Lab 8"),
   
   sidebarLayout(
      sidebarPanel(
        # Set title for sidebar
        titlePanel("Plot Settings"),
        # Set a numeric input that goes from 0 to 500 and increments by 50 with arrows.
        numericInput("points", "Number of Points", 50, 50, min = 0, max = 500),
        # Set slider input between 10 and 100, defaults to 50.
         sliderInput("range", "Range of X-axis:", min = 10, max = 100, value = 50)),
      
      # Show a plot of the generated distribution
      mainPanel(
        # Set title for main panel.
        titlePanel(h3("Plot of Uniform Distribution")),
         plotOutput("distPlot")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   data <- reactive(runif(input$points, max = input$range))
   data2 <- reactive(runif(input$points, max = 100))
   output$distPlot <- renderPlot({
     # Set title for plot to update when points change.
     title <- paste(input$points,"Random Ordered Pairs")
     # Set x-axis label to update based on max range value.
     xtitle <- paste("x value between 0 and",input$range)
     # draw the plot with the specified number of points and range
     plot(data(), data2(), xlim = c(0,100), ylim = c(0,100), col = 'darkgray', main = title,
           xlab = xtitle, ylab = "y value between 0 and 100")
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

