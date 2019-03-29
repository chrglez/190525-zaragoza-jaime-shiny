#
# This is a Shiny web application about linear regression.It returns
# several plots based on the user's input.
#

library(shiny)
library(plotly)

# Define UI for application that draws four plots.
ui <- fluidPage(


   # Application title
   titlePanel("Regresión lineal simple"),

   # Sidebar with a two text inputs. In the server section we transform these
   # texts in a dataframe
   sidebarLayout(
      sidebarPanel(
        textInput("vx",
                  "Valores de x:",
                  value = paste(1:10, collapse = ', ')),
        textInput("vy",
                  "Valores de y:",
                  value = paste((1:10)*2 + round(runif(10,-2,7),2), collapse = ', '))
      ),

      # We create 4 tabs.
      mainPanel(
        tabsetPanel(
          tabPanel('Regresión',plotlyOutput('regplot'),
                   hr(),

                   uiOutput('formula')),
          tabPanel('Regresión Residuos',plotlyOutput('errplot')),

          tabPanel('Boxplot Residuos',plotlyOutput('errboxplot')),

          tabPanel('QQ-Plot Residuos',plotlyOutput('errqqplot'))


        )
      )
   )
)

# Define server logic required to draw the 4 tabs
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
  }) -> x

  reactive({
    input$vy %>%
      str_split(",", simplify = TRUE) %>%
      map(conversor) %>%
      unlist()
  }) -> y

  data <- reactive({data.frame(x = x(), y = y())})
  lmfit    <- reactive({lm(y ~ x, data = data())})

   output$regplot <- renderPlotly({
      # generate a scatter plot based on input$y vs input$x from ui.R
     # and the linear regression line.
    print(
#      ggplotly(
#    ggplot(data(), aes(x = x, y = y)) +
#        geom_point(size = 3, shape = 1) +
#        stat_smooth(method = "lm", col = "blue", se = FALSE)
#      )
      data() %>%
        plot_ly(name = 'Valores', y = ~y, x = ~x, mode = 'marker', # Hover text:
                text = ~paste("Residuo: ",round(lmfit()$residuals,4))) %>%
        add_markers(y = ~y) %>%
        add_trace(x = ~x, y = lmfit()$fitted.values, mode = 'lines', name = 'Ajuste')
    )
   })

   output$formula <- renderUI({
     withMathJax(paste0('$$\\hat{y}=',round(lmfit()$coefficients[1],4),'+',
                        round(lmfit()$coefficients[2],4),'x;\\qquad R^2=',round(summary(lmfit())$r.squared*100,2),'\\%$$'))

   })

   output$errplot <- renderPlotly({

     datar <- reactive({data.frame(x = x(), Residuos = lmfit()$residuals)})
     lmerrfit    <- reactive({lm(Residuos ~ x, data = datar())})
     #ggplot(datar(), aes(x = x, y = y)) +
      # geom_point(size = 3) +
      # stat_smooth(method = "lm", col = "blue", se = FALSE)
     print(
     datar() %>%
       plot_ly(name = 'Residuos', y = ~Residuos, x = ~x, mode = 'marker') %>%
       add_markers(y = ~Residuos) %>%
       add_trace(x = ~x, y = lmerrfit()$fitted.values, mode = 'lines', name = 'Ajuste')

     )
   })

   output$errboxplot <- renderPlotly({

     datab <- reactive({data.frame(y = lmfit()$residuals)})
     #ggplot(data = datab(), aes(x = '', y = y)) +
    #   geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) +
    #   scale_y_continuous(name = "residuals")
     print(
     plot_ly(data = datab()) %>%
       add_trace(name = 'boxplot', y = ~y, x = 1, type = "box", boxpoints = "none") %>%
       add_trace(name = 'residuos', type = "scatter", mode = "markers",
                 y = ~y, x = rnorm(nrow(datab()),1,0.05), marker = list(color = ~y, size = 10),
                 hoverinfo = 'none') %>%
       layout(xaxis = list(tickmode = "array", tickvals = c(1), ticktext = c("Residuos") ))
     )
   })


   output$errqqplot <- renderPlotly({

     qq <- reactive({qqnorm(lmfit()$residuals, plot.it = FALSE)})
     datab <- reactive({data.frame(x = qq()$x, y = qq()$y)})
     datos.cuartiles <- quantile(lmfit()$residuals,c(0.25,0.75))
     norm.cuartiles <- qnorm(c(0.25, 0.75))
     b <- (datos.cuartiles[2] - datos.cuartiles[1])/(norm.cuartiles[2] - norm.cuartiles[1])
     a <- datos.cuartiles[1] - norm.cuartiles[1]*b
     #ggplot(data = datab(), aes(sample = y)) +
    #   stat_qq() +
     #  stat_qq_line()
     print(
       datab() %>%
         plot_ly(name = 'qqnorm', y = ~y, x = ~x, mode = 'marker')) %>%
       add_markers(y = ~y) %>%
       add_trace(name = 'qqline', x = ~x, y = ~x*b + a, mode = 'lines', name = 'Ajuste') %>%
       layout(xaxis = list(title = 'teórica'), yaxis = list(title = 'muestra') )
   })
}

# Run the application
shinyApp(ui = ui, server = server)

