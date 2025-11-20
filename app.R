library(shiny)
library(bundle)
library(tibble)
library(tidymodels)
library(ranger)
library(rsconnect)

# --- Load model ---
model_ready <- bundle::unbundle(readRDS("data/rf_biomass_alldata.rds"))

# --- Define example global metrics (replace with yours if available) ---
RMSE <- 1.82
MAPE <- 0.066
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
        margin-bottom: 10px;
      }
      .project-desc {
        background-color: #FFFFFF;
        border: 1px solid #D6EAF8;
        border-radius: 10px;
        padding: 12px 18px;
        margin-bottom: 20px;
        color: #2C3E50;
        box-shadow: 1px 1px 6px rgba(0,0,0,0.04);
        font-size: 14px;
        line-height: 1.5;
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
        "𓆉 Sea Turtle Biomass Prediction - Stereo Video Camera Morphometrics 𓆉")
  ),

  # Top project description panel
  fluidRow(
    column(
      12,
      div(class = "project-desc",
          strong("Project description: "), 
          HTML("This Shiny application predicts sea turtle biomass (kg) using morphometric measurements derived from stereo video camera or manual data. 
          A trained random forest model is used to produce point predictions along with uncertainty estimates (RMSE-based 95% interval and ±MAPE range). 
          Enter SCL, WFFL and HL in the input panel and click <em>Predict Biomass</em>.
          <br><br>
          <b>References:</b><br>
          E. Roberto, T. Siegfried, A. Cohen, S. Piacenza. <em>Predicting Biomass Sea Turtle using Validated Stereo-video Camera Derived Morphometrics: Data from Florida (ongoing study)</em>.<br>")
      )
    )
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
      h3("Predicted Biomass (Kg):", style = "color:#1A5276; font-weight:bold;"),
      wellPanel(
        h2(textOutput("pred"), 
           style = "color:#2E86C1; font-weight:bold; text-align:center;"),
        tags$hr(),
        h4("Uncertainty Estimates", style = "color:#117A65; font-weight:bold;"),
        htmlOutput("uncertainty_rmse"),
        htmlOutput("uncertainty_mape")
      )
    )
  ),
  # ---- author----
    tags$hr(),
      div(
      style = "text-align:center; font-size:14px; color:#5D6D7E; margin-top:20px;",
      HTML("Developed by <b>Achraf Cohen</b> · Computational Statistics and Data Analytics Lab (CSDA) @ UWF <br>
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




# ui <- fluidPage(
#   titlePanel("𓆉 Sea Turtle Biomass Prediction - Stereo Video Camera Morphometrics 𓆉 "),
#   sidebarLayout(
#     sidebarPanel(
#       numericInput("rSCL", "SCL - Straight Carapace Length (cm):", 47.4 , min = 10, max = 100),
#       numericInput("rWFFL", "WFFL - Width of Front Flipper Left (cm):", 7.45, min = 2, max = 20),
#       numericInput("rHL", "HL - Head length (cm):", 10.2, min = 5, max = 30),
#       actionButton("go", "Predict Biomass")
#     ),


#  mainPanel(
#   h3("Predicted Biomass (Kg):"),
#   wellPanel(
#     h2(textOutput("pred"), style = "color:#2E86C1; font-weight:bold;"),
    
#     tags$hr(),
#     h4("Uncertainty Estimates"),
#     htmlOutput("uncertainty_rmse"),
#     htmlOutput("uncertainty_mape")
#   )
# )

#   )
# )

# library(bundle)
# library(shiny)
# library(rsconnect)
# library(tibble)
# library(tidymodels)
# library(ranger)


# #### model
#     model_ready <- bundle::unbundle(readRDS("data/model_bundle.rds"))

# server <- function(input, output) {
#   pred_value <- eventReactive(input$go, {
#     new_data <- tibble(
#       rSCL = input$rSCL,
#       rWFFL  = input$rWFFL,
#       rHL  = input$rHL
#     )
#     predict(model_ready, new_data)$.pred
#   })

#   output$pred <- renderPrint({
#     pred_value()
#   })
# }

# shinyApp(ui, server)
