ui <- fluidPage(
  titlePanel("𓆉 Sea Turtle Biomass Prediction - Stereo Video Camera Morphometrics 𓆉 "),
  sidebarLayout(
    sidebarPanel(
      numericInput("rSCL", "SCL - Straight Carapace Length (cm):", 47.4 , min = 10, max = 100),
      numericInput("rWFFL", "WFFL - Width of Front Flipper Left (cm):", 7.45, min = 2, max = 20),
      numericInput("rHL", "HL - Head length (cm):", 10.2, min = 5, max = 30),
      actionButton("go", "Predict Biomass")
    ),
    mainPanel(
      h3("Predicted Biomass (Kg):"),
      verbatimTextOutput("pred")
    )
  )
)

library(bundle)
library(shiny)
library(rsconnect)
library(tibble)
library(tidymodels)
library(ranger)


#### model
    model_ready <- bundle::unbundle(readRDS("data/model_bundle.rds"))

server <- function(input, output) {
  pred_value <- eventReactive(input$go, {
    new_data <- tibble(
      rSCL = input$rSCL,
      rWFFL  = input$rWFFL,
      rHL  = input$rHL
    )
    predict(model_ready, new_data)$.pred
  })

  output$pred <- renderPrint({
    pred_value()
  })
}

shinyApp(ui, server)



