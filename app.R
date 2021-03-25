#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
e <- ggplot2::diamonds
t <- dplyr::storms


ui <- fluidPage(
    titlePanel("Observations of Three Datasets"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "dataset",
                        label = "Choose a dataset:",
                        choices = c("iris", "diamonds", "storms")),
            #did not know if slider would be better, but went with number input
            numericInput(inputId = "obs",
                         label = "Number of observations:",
                         value = 1)
        ),
        mainPanel(
            verbatimTextOutput('summary'),
            tableOutput('view')
        )
    )
)
#dataset iris was not wokring when called seperately like diamonds and storms
#so just called it on the spot since it was in data unlike the other two
server <- function(input, output) {
    datasetInput <- reactive({
        switch(input$dataset,
               'iris' = iris,
               "diamonds" = e,
               "storms" = t)
    })
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    output$view <- renderTable({
        head(datasetInput(), n = input$obs)
    })
    
}

shinyApp(ui = ui, server = server)