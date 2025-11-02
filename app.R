ui <- fluidPage(
  titlePanel("𓆉 Biomass Prediction Based on Remote Morphometrics 𓆉 "),
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


#### Deploy

# Deploy the app
rsconnect::setAccountInfo(name='acohen',
			  token='CA6B85D75E3F1FD602737C0AE2968E2D',
			  secret='Yj1/2oQwrzVGorDIxFtmD4ZXwuAI+SHII4Szr2Qw')

rsconnect::deployApp("my_shiny_app")
