library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs for UI manipulation
  
  # Custom CSS for full-screen experience
  tags$head(
    tags$style(HTML("
      .fullscreen-iframe {
        position: fixed;
        display: inline;
        top: 50px;
        left: 0;
        width: 100vw;
        height: 100vh;
        z-index: 10000;
        background: white;
        border: none;
      }
      .iframe-navbar {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 50px;
        background: #f8f9fa;
        padding: 10px 20px;
        z-index: 10000;
        border-bottom: 1px solid #dee2e6;
        display: none;
        justify-content: space-between;
        align-items: center;
      }
      .hidden {
        display: none;
      }
      .showing {
        display: flex;
      }
      #main-content {
        transition: all 0.3s ease;
      }
      .blurred {
        filter: blur(5px);
        pointer-events: none;
      }
    "))
  ),
  
  # Navigation bar for iframe (initially hidden)
  div(
    id = "iframe-navbar",
 #   class = "iframe-navbar hidden",
    class = "iframe-navbar",
    h4("External Content Viewer", style = "margin: 0;"),
    actionButton("back-to-app", "Return to App", class = "btn-primary btn-sm")
  ),
  
  # Main app content
  div(
    id = "main-content",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h3("App Navigation"),
        p("Use the tabs to navigate through the application.")
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = "mainTabs",
          type = "tabs",
          tabPanel(
            "Dashboard",
            h2("Application Dashboard"),
            p("This is the main dashboard of the application."),
            plotOutput("plot", height = "250px")
          ),
          tabPanel(
            "Data Analysis",
            h2("Data Analysis Tools"),
            p("Various data analysis tools would be available here.")
          ),
          tabPanel(
            "External Content",
            h2("External Content Viewer"),
            p("Click the button below to view external content in full-screen mode."),
            actionButton("view-external", "View External Content", class = "btn-success"),
            br(), br(),
            div(
              id = "preview-container",
              style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px;",
              
              h4("Content Preview"),
              htmlOutput("framePreview")
            )
          ),
          tabPanel(
            "Settings",
            h2("Application Settings"),
            p("Configure your application settings here.")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Create sample plot
  output$plot <- renderPlot({
    plot(cars$speed, cars$dist, main = "Car Speed vs Distance",
         xlab = "Speed (mph)", ylab = "Distance (ft)", pch = 19, col = "blue")
    abline(lm(dist ~ speed, data = cars), col = "red")
  })
  
  # Create iframe preview
  output$framePreview <- renderUI({
    tags$iframe(
      src = "https://en.wikipedia.org/wiki/Shiny_(web_framework)",
      style = "width: 100%; height: 300px; border: 1px solid #ddd;"
    )
  })
  
  # Observe when External Content tab is selected
  observeEvent(input$mainTabs, {
    if (input$mainTabs == "External Content") {
      # Tab is selected, but don't automatically go fullscreen
      # We'll let the user click the button instead
    }
  })
  
  # Handle fullscreen view request
  observeEvent(input$`view-external`, {
    # Show the iframe navbar
    shinyjs::addClass("iframe-navbar","showing")
    #shinyjs::toggle("iframe-navbar")
    # Insert the fullscreen iframe
    insertUI(
      selector = "body",
      where = "beforeEnd",
      ui = tags$iframe(
        id = "fullscreen-iframe",
        class = "fullscreen-iframe",
        src = "https://en.wikipedia.org/wiki/Shiny_(web_framework)"
      )
    )
    
    # Add blur effect to main content
    shinyjs::addClass("main-content", "blurred")
  })
  
  # Handle return to app
  observeEvent(input$`back-to-app`, {
    # Remove the iframe
    removeUI("#fullscreen-iframe")
    
    # Hide the navbar
    shinyjs::hide("iframe-navbar")
    
    # Remove blur effect
    shinyjs::removeClass("main-content", "blurred")
    
    # Switch back to the first tab
    updateTabsetPanel(session, "mainTabs", selected = "Dashboard")
  })
}

shinyApp(ui, server)