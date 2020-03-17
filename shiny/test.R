library(shiny)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "rxmo",
                   label = "Correlation between X and Mo",
                   value = 0.1807, min = 0, max = 1, step = 0.01),
      numericInput(inputId = "rxy",
                   label = "Correlation between X and Y",
                   value = 0.1602, min = 0, max = 1, step = 0.01),
      numericInput(inputId = "rymo",
                   label = "Correlation between Y and Mo",
                   value = 0.446, min = 0, max = 1, step = 0.01),
      numericInput(inputId = "nobs",
                   label = "Number of Observations",
                   value = 123, min = 1, max = NA, step = 1),
      numericInput(inputId = "conflevel",
                   label = "Confidence Interval",
                   value = 0.95, min = 0.01, max = 0.99, step = 0.01),
      radioButtons(inputId ="labelest",
                   label = "Parameter of interest",
                   choices = c("a1","b1", "c","a1b1indirect1"))),
  mainPanel(plotOutput("plot"))
  )
  )
server <- function(input, output){
  output$plot <- renderPlot({
    rxmu_plot(input$rxmo, input$rxy, input$rymo, input$nobs,
              input$labelest, input$conflevel, specifyunob = 0)
    })
}
shinyApp(ui = ui, server = server)
