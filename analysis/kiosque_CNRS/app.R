#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

text <- "test"
text <- TeX('\\alpha')

# Define UI for application that draws a histogram
ui <- fluidPage(

  
   # Application title
   titlePanel("Simulateur de conversations en ligne"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
     
      sidebarPanel(width = 3,
          sliderInput("n",
                      "taille:",
                      min = 1,
                      max = 1000,
                      value = 100),
           sliderInput("alpha",
                       "alpha (coefficient popularité):",
                       min = 0,
                       max = 20,
                       value = 0.7),
           sliderInput("beta",
                       "beta (coefficient racine):",
                       min = 0,
                       max = 100,
                       value = 1),
           sliderInput("tau",
                       "tau (coefficient ancienneté):",
                       min = 0,
                       max = 1,
                       value = 0.5)
          ),
          
        # Show a plot of the generated distribution
        mainPanel(plotOutput("distPlot", width="100%"),  width = 9)
      )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
    parents = gen.parentsvector.Gomez2013(n=input$n, input$alpha/10, input$beta, input$tau)
    #plot.tree.nicely.sequential(parents, stepsecs = 0.001)
    plot.tree.nicely(parents)
   })
}

# Run the application
shinyApp(ui = ui, server = server)

