#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Simulateur de conversations en ligne"),
  
  # Sidebar with a slider input for number of bins
  fluidRow(
    
    column(6,
      sidebarPanel(
        sliderInput("n",
                    "taille:",
                    min = 1,
                    max = 1000,
                    value = 100),
        sliderInput("alpha",
                    "popularité:",
                    min = 0,
                    max = 10,
                    value = 1),
        sliderInput("beta",
                    "racine:",
                    min = 0,
                    max = 100,
                    value = 1),
        sliderInput("tau",
                    "memoire:",
                    min = 0,
                    max = 1,
                    value = 0.5),
        sliderInput("time",
                    "temps:",
                    min = 1,
                    max = 1000,
                    value = 100)
      )
    ),
    
    column(6, 
    # Show a plot of the generated distribution
    mainPanel(plotOutput("distPlot", width="100%"))
  ))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2]
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    parents = gen.parentsvector.Gomez2013(n=input$n, input$alpha, input$beta, input$tau)
    plot.tree.nicely(parents)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
