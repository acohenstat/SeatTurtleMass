library(shiny)
library(bundle)
library(tibble)
library(tidymodels)
library(ranger)
library(rsconnect)

# --- Load model ---
model_ready <- bundle::unbundle(readRDS("data/rf_biomass_alldata.rds"))

# --- Define example global metrics (replace with yours if available) ---
RMSE <- 1.83
MAPE <- 0.066 # 6.6%

# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #F9FBFC;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }
      .title-panel {
        text-align: center;
        color: #117864;
        font-weight: bold;
        font-size: 26px;
        margin-bottom: 25px;
      }
      .well {
        background-color: #EAF2F8;
        border: 2px solid #AED6F1;
        border-radius: 15px;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.1);
      }
      .btn-success {
        background-color: #117A65 !important;
        border-color: #0E6655 !important;
        width: 100%;
        font-weight: bold;
        font-size: 16px;
      }
    "))
  ),
  
  titlePanel(
    div(class = "title-panel",
        "𓆉 Sea Turtle Biomass Prediction based on Morphometrics 𓆉")
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Input Morphometric Measurements"),
      tags$hr(),
      numericInput("SCL", "SCL - Straight Carapace Length (cm):", 47.4 , min = 10, max = 100),
      numericInput("WFFL", "WFFL - Width of Front Flipper Left (cm):", 7.45, min = 2, max = 20),
      numericInput("HL", "HL - Head Length (cm):", 10.2, min = 5, max = 30),
      tags$br(),
      actionButton("go", "Predict Biomass", class = "btn btn-success")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Prediction",
                 h3("Predicted Biomass (Kg):", style = "color:#1A5276; font-weight:bold;"),
                 wellPanel(
                   h2(textOutput("pred"), 
                      style = "color:#2E86C1; font-weight:bold; text-align:center;"),
                   tags$hr(),
                   h4("Uncertainty Estimates", style = "color:#117A65; font-weight:bold;"),
                   htmlOutput("uncertainty_rmse"),
                   htmlOutput("uncertainty_mape")
                 )
        ),
        tabPanel("Project",
                 wellPanel(
                   h4("Project description", style = "color:#1A5276; font-weight:bold;"),
                   HTML("<p>This application predicts sea turtle biomass (kg) from combined data from stereo-video-derived and manual morphometrics (Straight Carapace Length, Width of Front Flipper, Head Length) using a pre-trained model on 3 species from the Florida Gulf of Mexico. The model was trained and validated with field-collected morphometric and mass data; uncertainty is communicated with an RMSE-based 95% interval and a typical ±MAPE range.</p>
                         <p>Data sources include stereo-video camera-derived measurements and manual morphometrics collected in rehabilitation facilities. The model combines features from different measurement modalities and was evaluated with cross-validation and external validation where available.</p>")
                 )
        ),
        tabPanel("Reference",
                 wellPanel(
                   h4("Citation & links", style = "color:#117A65; font-weight:bold;"),
                   HTML("<p>Please cite this work as:</p>
                         <p><strong>E. Roberto, T. Siegfried, A. Cohen, S. Piacenza</strong> (2025). Predicting sea turtle biomass using stereo-video camera derived and manual morphometrics in rehabilitation facilities in Florida. <em>Journal / Report</em>. <a href='https://doi.org/your-doi-here' target='_blank'>https://doi.org/your-doi-here</a></p>
                         <p>Project page / code: <a href='https://github.com/your-org/your-repo' target='_blank'>https://github.com/your-org/your-repo</a></p>")
                 )
        ),
        tabPanel("About",
                 wellPanel(
                   h4("About this app", style = "color:#1A5276; font-weight:bold;"),
                   HTML("<p>This Shiny app provides quick biomass predictions and uncertainty summaries for field operators and researchers. For more details on the model, please see the Project and Reference tabs.</p>"),
                   tags$hr(),
                   HTML("<p style='font-size:12px;color:#5D6D7E;'>Contact: Achraf Cohen · Computational Statistics and Data Analytics Lab (CSDA) @ UWF</p>")
                 )
        )
      )
    )
  ),
  # ---- author----
  tags$hr(),
  div(
    style = "text-align:center; font-size:14px; color:#5D6D7E; margin-top:20px;",
    HTML("Developed by <b>Achraf Cohen</b> ·  <a href='https://csdalab.github.io/' target='_blank'>Computational Statistics and Data Analytics Lab (CSDA) @ UWF</a><br>
       © 2025")
  )
)

# --- Server ---
server <- function(input, output) {
  
  pred_value <- eventReactive(input$go, {
    new_data <- tibble(
      SCL  = input$SCL,
      WFFL = input$WFFL,
      HL   = input$HL
    )
    predict(model_ready, new_data)$.pred
  })
  
  # Display predicted value
  output$pred <- renderText({
    req(pred_value())
    paste0(round(pred_value(), 2), " Kg")
  })
  
  # RMSE-based 95% interval
  output$uncertainty_rmse <- renderUI({
    req(pred_value())
    lower <- pred_value() - 1.96 * RMSE
    upper <- pred_value() + 1.96 * RMSE
    HTML(paste0(
      "<b style='color:#117A65;'>RMSE-based 95% interval:</b> ",
      round(lower, 2), " – ", round(upper, 2), " Kg"
    ))
  })
  
  # MAPE-based typical range
  output$uncertainty_mape <- renderUI({
    req(pred_value())
    lower <- pred_value() * (1 - MAPE)
    upper <- pred_value() * (1 + MAPE)
    HTML(paste0(
      "<b style='color:#AF601A;'>Typical ±MAPE range:</b> ",
      round(lower, 2), " – ", round(upper, 2), " Kg"
    ))
  })
}

# --- Launch app ---
shinyApp(ui, server)
