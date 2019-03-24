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
   titlePanel("Regresión lineal simple"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        textInput("vx",
                  "Valores de x:",
                  value = paste(1:10, collapse=', ')),
        textInput("vy",
                  "Valores de y:",
                  value = paste((1:10)*2+round(runif(10,0,1),2), collapse=', '))
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  library(ggplot2)
  library(stringr)
  library(dplyr)
  library(purrr)
  library(DT)

  conversor <- function(x){
    x <- x %>% str_remove_all(" ")
    if (suppressWarnings(!is.na(as.numeric(x))))
      x %>% as.numeric()
    else {
      str.temp <- x %>% str_split("-", simplify = T)
      str.temp[1]:str.temp[2]
    }
  }

  reactive({
    input$vx %>%
      str_split(",", simplify = TRUE) %>%
      map(conversor) %>%
      unlist()
  })-> x

  reactive({
    input$vy %>%
      str_split(",", simplify = TRUE) %>%
      map(conversor) %>%
      unlist()
  })-> y

   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     data <- reactive({data.frame(x=x(), y=y())})
     lmfit    <- reactive({lm(y ~ x, data = data())})



      ggplot(data(), aes(x = x, y = y)) +
        geom_point(size=3) +
        stat_smooth(method = "lm", col = "red", se = FALSE)
   })
}

# Run the application
shinyApp(ui = ui, server = server)

