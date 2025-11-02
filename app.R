ui <- fluidPage(
  titlePanel("𓆉 Sea Turtle Biomass Prediction - Stereo Video Camera Morphometrics 𓆉 "),
  sidebarLayout(
    sidebarPanel(
      numericInput("rSCL", "SCL - Straight Carapace Length (cm):", 6, min = 3, max = 8),
      numericInput("rWFFL", "WFFL - Width of Front Flipper Left (cm):", 110, min = 50, max = 400),
      numericInput("rHL", "HL - Head length (cm):", 3.0, min = 1.5, max = 6.0),
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



