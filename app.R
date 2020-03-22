library(shiny)
library(ConMed)

ui <- fluidPage(
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
                          htmlOutput("Note"),
                          width = 3),
                        mainPanel(plotOutput("plot1", width = "100%"))
                                  )
                      ),
             tabPanel("Yes, I can guess rmomu and rymu.",
                      sidebarLayout(
                        sidebarPanel(tags$img(src="model.png",width="100%"),
                                     numericInput(inputId = "rxmo2",
                                                  label = "Correlation between X and Mo",
                                                  value = 0.1807, min = 0, max = 1, step = 0.01),
                                     numericInput(inputId = "rxy2",
                                                  label = "Correlation between X and Y",
                                                  value = 0.1602, min = 0, max = 1, step = 0.01),
                                     numericInput(inputId = "rymo2",
                                                  label = "Correlation between Y and Mo",
                                                  value = 0.446, min = 0, max = 1, step = 0.01),
                                     numericInput(inputId = "rmomu2",
                                                  label = "Correlation between Mo and Mu",
                                                  value = 0.1, min = 0, max = 1, step = 0.01),
                                     numericInput(inputId = "rymu2",
                                                  label = "Correlation between Y and Mu",
                                                  value = 0.1, min = 0, max = 1, step = 0.01),
                                     numericInput(inputId = "nobs2",
                                                  label = "Number of Observations",
                                                  value = 123, min = 1, max = NA, step = 1),
                                     numericInput(inputId = "conflevel2",
                                                  label = "Confidence Interval",
                                                  value = 0.95, min = 0.01, max = 0.99, step = 0.01),
                                     radioButtons(inputId ="labelest2",
                                                  label = "Parameter of interest",
                                                  choices = c("a1","b1", "c","a1b1", "a1,b1, and a1b1")),
                                     htmlOutput("Note2"),
                                     width = 3),
                        mainPanel(plotOutput("plot2", width = "100%"))
                      )),
             tabPanel("Yes, I can guess rxmu and rymu.",
                      sidebarLayout(
                        sidebarPanel(tags$img(src="model.png",width="100%"),
                                     numericInput(inputId = "rxmo3",
                                                  label = "Correlation between X and Mo",
                                                  value = 0.1807, min = 0, max = 1, step = 0.01),
                                     numericInput(inputId = "rxy3",
                                                  label = "Correlation between X and Y",
                                                  value = 0.1602, min = 0, max = 1, step = 0.01),
                                     numericInput(inputId = "rymo3",
                                                  label = "Correlation between Y and Mo",
                                                  value = 0.446, min = 0, max = 1, step = 0.01),
                                     numericInput(inputId = "rxmu3",
                                                  label = "Correlation between X and Mu",
                                                  value = 0.1, min = 0, max = 1, step = 0.01),
                                     numericInput(inputId = "rymu3",
                                                  label = "Correlation between Y and Mu",
                                                  value = 0.1, min = 0, max = 1, step = 0.01),
                                     numericInput(inputId = "nobs3",
                                                  label = "Number of Observations",
                                                  value = 123, min = 1, max = NA, step = 1),
                                     numericInput(inputId = "conflevel3",
                                                  label = "Confidence Interval",
                                                  value = 0.95, min = 0.01, max = 0.99, step = 0.01),
                                     radioButtons(inputId ="labelest3",
                                                  label = "Parameter of interest",
                                                  choices = c("a1","b1", "c","a1b1", "a1,b1, and a1b1")),
                                     htmlOutput("Note3"),
                                     width = 3),
                        mainPanel(plotOutput("plot3", width = "100%"))
                      )),
             tabPanel("Yes, I can guess rxmu and rmomu.",
                      sidebarLayout(
                        sidebarPanel(tags$img(src="model.png",width="100%"),
                                     numericInput(inputId = "rxmo4",
                                                  label = "Correlation between X and Mo",
                                                  value = 0.1807, min = 0, max = 1, step = 0.01),
                                     numericInput(inputId = "rxy4",
                                                  label = "Correlation between X and Y",
                                                  value = 0.1602, min = 0, max = 1, step = 0.01),
                                     numericInput(inputId = "rymo4",
                                                  label = "Correlation between Y and Mo",
                                                  value = 0.446, min = 0, max = 1, step = 0.01),
                                     numericInput(inputId = "rxmu4",
                                                  label = "Correlation between X and Mu",
                                                  value = 0.1, min = 0, max = 1, step = 0.01),
                                     numericInput(inputId = "rmomu4",
                                                  label = "Correlation between Mo and Mu",
                                                  value = 0.1, min = 0, max = 1, step = 0.01),
                                     numericInput(inputId = "nobs4",
                                                  label = "Number of Observations",
                                                  value = 123, min = 1, max = NA, step = 1),
                                     numericInput(inputId = "conflevel4",
                                                  label = "Confidence Interval",
                                                  value = 0.95, min = 0.01, max = 0.99, step = 0.01),
                                     radioButtons(inputId ="labelest4",
                                                  label = "Parameter of interest",
                                                  choices = c("a1","b1", "c","a1b1", "a1,b1, and a1b1")),
                                     htmlOutput("Note4"),
                                     width = 3),
                        mainPanel(plotOutput("plot4", width = "100%"))
                      ))
             )
)


server <- function(input, output, session){
  output$plot1 <- renderPlot(
    {if (input$labelest=="a1,b1, and a1b1")
    {conmed_plot_a1b1(input$rxmo, input$rxy, input$rymo, input$nobs,
                    input$conflevel)}
      else {conmed_plot(input$rxmo, input$rxy, input$rymo, input$nobs,input$labelest,
                      input$conflevel)}},
    width = 1000, height = 1200)

  output$Note <- renderText({
    HTML("<b><em>Note:</em> The dashed line represents the estimated effect for
no omitted post-treatment confounder <var>M<sub>U</sub></var>. The grey area
represents the confidence interval for the estimated effect at each value
of the unobserved <var>M<sub>U</sub></var>-related correlation. The solid line
represents the estimated effect at each value of the unobserved
     <var>M<sub>U</sub></var>-related correlation.</b>")})

  output$plot2 <- renderPlot(
    {if (input$labelest2=="a1,b1, and a1b1")
    {rxmu_plot_a1b1(input$rxmo2, input$rxy2, input$rymo2, input$nobs2,
                    input$conflevel2, specifyunob = 1, input$rmomu2, input$rymu2)}
      else {rxmu_plot(input$rxmo2, input$rxy2, input$rymo2, input$nobs2,
                      input$labelest2,input$conflevel2,specifyunob = 1, input$rmomu2, input$rymu2)}},
    width = 900, height = 900)

  output$Note2 <- renderText({
    HTML("<b><em>Note:</em> The dashed line represents the estimated effect for
         no omitted post-treatment confounder <var>M<sub>U</sub></var>. The grey area
         represents the confidence interval for the estimated effect at each value
         of the unobserved <var>r<sub>XM<sub>U</sub></sub></var>. The solid line
         represents the estimated effect at each value of the unobserved
         <var>r<sub>XM<sub>U</sub></sub></var>.</b>")})

  output$plot3 <- renderPlot(
    {if (input$labelest3=="a1,b1, and a1b1")
    {rmomu_plot_a1b1(input$rxmo3, input$rxy3, input$rymo3, input$nobs3,
                    input$conflevel2, specifyunob = 1, input$rxmu3, input$rymu3)}
      else {rmomu_plot(input$rxmo3, input$rxy3, input$rymo3, input$nobs3,
                      input$labelest3,input$conflevel3,specifyunob = 1, input$rxmu3, input$rymu3)}},
    width = 900, height = 900)

  output$Note3 <- renderText({
    HTML("<b><em>Note:</em> The dashed line represents the estimated effect for
         no omitted post-treatment confounder <var>r<sub>M<sub>O</sub>M<sub>U</sub></sub></var>. The grey area
         represents the confidence interval for the estimated effect at each value
         of the unobserved &rho<var><sub>XM<sub>U</sub></sub></var>. The solid line
         represents the estimated effect at each value of the unobserved
         <var>r<sub>M<sub>O</sub>M<sub>U</sub></sub></var>.</b>")})

  output$plot4 <- renderPlot(
    {if (input$labelest4=="a1,b1, and a1b1")
    {rymu_plot_a1b1(input$rxmo4, input$rxy4, input$rymo4, input$nobs4,
                    input$conflevel4, specifyunob = 1, input$rxmu4, input$rmomu4)}
      else {rymu_plot(input$rxmo4, input$rxy4, input$rymo4, input$nobs4,
                      input$labelest4,input$conflevel4,specifyunob = 1, input$rxmu4, input$rmomu4)}},
    width = 900, height = 900)

  output$Note4 <- renderText({
    HTML("<b><em>Note:</em> The dashed line represents the estimated effect for
         no omitted post-treatment confounder <var>r<sub>YM<sub>U</sub></sub></var>. The grey area
         represents the confidence interval for the estimated effect at each value
         of the unobserved &rho<var><sub>XM<sub>U</sub></sub></var>. The solid line
         represents the estimated effect at each value of the unobserved
         <var>r<sub>YM<sub>U</sub></sub></var>.</b>")})
}

shinyApp(ui = ui, server = server)
