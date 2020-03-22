library(shiny)
library(ConMed)

# ui.R
shinyUI(
  fluidPage(
    theme = shinythemes::shinytheme("superhero"),
    titlePanel("ConMed"),
    h3("Sensitivity Analysis of Post-treatment Confounder"),
    navbarPage("Have some ideas about the unobserved mediator?",
      tabPanel("No",
        sidebarLayout(
          sidebarPanel(
            tags$img(src="model.png",width="100%"),
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
              choices = c("a1","b1", "c","a1b1", "a1,b1, and a1b1")),
            actionButton("button_conmed", "Run"),
            htmlOutput("Note"),
            width = 3),
          mainPanel(plotOutput("plot", width = "100%"))
        )
      ),

      tabPanel("Yes",
               sidebarLayout(
                 sidebarPanel(
                   tags$img(src="model.png",width="100%"),
                   numericInput(inputId = "rxmo",
                                label = "Correlation between X and Mo",
                                value = 0.1807, min = 0, max = 1, step = 0.01),
                   numericInput(inputId = "rxy",
                                label = "Correlation between X and Y",
                                value = 0.1602, min = 0, max = 1, step = 0.01),
                   numericInput(inputId = "rymo",
                                label = "Correlation between Y and Mo",
                                value = 0.446, min = 0, max = 1, step = 0.01),
                   numericInput(inputId = "rmomu",
                                label = "Correlation between Mo and Mu",
                                value = 0.446, min = 0, max = 1, step = 0.01),
                   numericInput(inputId = "rymu",
                                label = "Correlation between Y and Mu",
                                value = 0.446, min = 0, max = 1, step = 0.01),
                   numericInput(inputId = "nobs",
                                label = "Number of Observations",
                                value = 123, min = 1, max = NA, step = 1),
                   numericInput(inputId = "conflevel",
                                label = "Confidence Interval",
                                value = 0.95, min = 0.01, max = 0.99, step = 0.01),
                   radioButtons(inputId ="labelest",
                                label = "Parameter of interest",
                                choices = c("a1","b1", "c","a1b1", "a1,b1, and a1b1")),
                   actionButton("button_rxmu", "Run"),
                   htmlOutput("Note"),
                   width = 3),
                 mainPanel(plotOutput("plot", width = "100%"))
               )
      )
    )
  )
)

#server.R
shinyServer <- function(input, output){

  # default outputs
  values <- reactiveValues(button_conmed = FALSE)
  values <- reactiveValues(button_rxmu = FALSE)

#  output$default_text <- renderText({
#    o <- if(!input$button_conmed) {
#      "Output will appear here when the app is run"
#    } else {
#      NULL
#    }
  })

  #conmed_plot
  df_conmed <- eventReactive(input$button_conmed, {
      validate(
        need(
          is.numeric(input$rxmo) &
          is.numeric(input$rymo) &
          is.numeric(input$rxy) &
          is.numeric(input$nobs) &
          is.numeric(input$conflevel),
           "Did not run! Did you enter correlations among X, Y, and Mo, number of observations, confidence level?
           Please type in appropriate numbers."
        )
      )

    output$plot <- renderPlot({
      if (input$labelest=="a1,b1, and a1b1"){
        conmed_plot_a1b1(
          input$rxmo, input$rxy, input$rymo, input$nobs,input$conflevel
        )
        } else {
          conmed_plot(
            input$rxmo, input$rxy, input$rymo, input$nobs,input$labelest,input$conflevel
          )
          }
      }, width = 1000, height = 1200)

    output$Note <- renderText({
      HTML("<b><em>Note:</em> The dashed line represents the estimated effect for
           no omitted post-treatment confounder <var>M<sub>U</sub></var>. The grey area
           represents the 95% confidence interval for the estimated effect at each value
           of the unobserved <var>M<sub>U</sub></var>-related correlation. The solid line
           represents the estimated effect at each value of the unobserved
           <var>M<sub>U</sub></var>-related correlation.</b>")})
  }
  )
}

