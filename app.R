source("global.R") 

# ===========================================
#                     UI
# ===========================================
ui <- fluidPage(
  
  # 3. Call the Module UI
  # We give it the ID "main_analysis". This string is crucial.
  cusumUI("main_analysis")
)

# ===========================================
#                   SERVER
# ===========================================
server <- function(input, output, session) {
  
  # 4. Call the Module Server
  # The ID "main_analysis" MUST match the ID used in the UI above.
  cusumServer("main_analysis")

  
}

shinyApp(ui, server)