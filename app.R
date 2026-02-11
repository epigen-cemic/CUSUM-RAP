source("global.R") 

# ===========================================
#                     UI
# ===========================================
ui <- fluidPage(
  titlePanel("CUSUM Rumor Analysis Dashboard"),
  
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
  
  
  
  # When the user's browser session ends, kill the R process
  # Specific for the portable version
  session$onSessionEnded(function() {
    stopApp()
    q("no")
  })
  
}

shinyApp(ui, server)