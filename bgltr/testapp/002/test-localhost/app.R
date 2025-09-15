library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .fullscreen-iframe {
        position: fixed;
        top: 50px;
        left: 0;
        width: 100vw;
        height: calc(100vh - 50px);
        z-index: 9999;
        background: white;
        border: none;
      }
      .iframe-navbar {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        background: #f8f9fa;
        padding: 10px 20px;
        z-index: 10000;
        border-bottom: 1px solid #dee2e6;
        display: flex;
        justify-content: space-between;
        align-items: center;
        height: 50px;
      }
      .hidden {
        display: none;
      }
      .status-indicator {
        margin-left: 15px;
        padding: 4px 8px;
        border-radius: 4px;
        font-size: 12px;
      }
      .status-connected {
        background: #d4edda;
        color: #155724;
      }
      .status-disconnected {
        background: #f8d7da;
        color: #721c24;
      }
    "))
  ),
  
  div(
    id = "iframe-navbar",
    class = "iframe-navbar hidden",
    div(style = "display: flex; align-items: center;",
        h4("Local Server Content", style = "margin: 0;"),
        span(id = "connection-status", class = "status-indicator status-disconnected", 
             "Checking connection...")
    ),
    actionButton("back-to-app", "Return to App", class = "btn-primary btn-sm")
  ),
  
  div(
    id = "main-content",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h3("Server Content Viewer"),
        p("View content served from localhost:8088 on this server."),
        br(),
        div(
          style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
          h5("Connection Info:"),
          verbatimTextOutput("serverInfo"),
          actionButton("test-connection", "Test Connection", class = "btn-info btn-sm")
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = "mainTabs",
          type = "tabs",
          tabPanel(
            "Dashboard",
            h2("Server Content Dashboard"),
            p("This application displays content from localhost:8088 on the same server."),
            actionButton("view-server-content", "View Server Content", 
                         class = "btn-success", icon = icon("desktop"))
          ),
          tabPanel(
            "Configuration",
            h2("Server Configuration"),
            p("Configure the local server connection settings:"),
            textInput("server_path", "Content Path:", value = "sample.html"),
            numericInput("server_port", "Server Port:", value = 8088, min = 1, max = 65535),
            actionButton("save-config", "Save Configuration", class = "btn-primary")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values for server configuration
  server_config <- reactiveValues(
    port = 8088,
    path = "files",
    base_url = "http://localhost:8088"
  )
  
  # Update server info display
  output$serverInfo <- renderText({
    paste0("Server: localhost:", server_config$port, "\n",
           "Path: /", server_config$path, "\n",
           "URL: ", server_config$base_url, "/", server_config$path)
  })
  
  # Test connection to local server
  observeEvent(input$`test-connection`, {
    showModal(modalDialog(
      title = "Testing Connection",
      "Attempting to connect to local server...",
      footer = NULL,
      easyClose = FALSE
    ))
    
    # Try to connect to the local server
    tryCatch({
      # Simple HTTP request to check if server is reachable
      response <- httr::GET(paste0("http://localhost:", server_config$port, "/"))
      
      if (httr::status_code(response) == 200) {
        showNotification("Connection successful! Server is running.", type = "message")
        shinyjs::runjs("$('#connection-status')
          .removeClass('status-disconnected')
          .addClass('status-connected')
          .text('Connected');")
      } else {
        stop("Server responded with error")
      }
    }, error = function(e) {
      showNotification(paste("Connection failed:", e$message), type = "error")
      shinyjs::runjs("$('#connection-status')
        .removeClass('status-connected')
        .addClass('status-disconnected')
        .text('Not connected');")
    })
    
    removeModal()
  })
  
  # Handle configuration changes
  observeEvent(input$`save-config`, {
    server_config$port <- input$server_port
    server_config$path <- input$server_path
    server_config$base_url <- paste0("http://localhost:", input$server_port)
    showNotification("Configuration saved!", type = "message")
  })
  
  # Handle server content view request
  observeEvent(input$`view-server-content`, {
    # Show the iframe navbar
    shinyjs::show("iframe-navbar")
    
    # Build the URL for the local server content
    content_url <- paste0("http://localhost:", server_config$port, "/", server_config$path)
    
    # Remove existing iframe if any
    removeUI("#fullscreen-iframe", immediate = TRUE)
    
    # Insert the fullscreen iframe with local server content
    insertUI(
      selector = "body",
      where = "beforeEnd",
      ui = tags$iframe(
        id = "fullscreen-iframe",
        class = "fullscreen-iframe",
        src = content_url
      )
    )
    
    # Test connection when showing content
    shinyjs::runjs("
      setTimeout(function() {
        var iframe = document.getElementById('fullscreen-iframe');
        iframe.onload = function() {
          $('#connection-status')
            .removeClass('status-disconnected')
            .addClass('status-connected')
            .text('Connected');
        };
        iframe.onerror = function() {
          $('#connection-status')
            .removeClass('status-connected')
            .addClass('status-disconnected')
            .text('Connection failed');
        };
      }, 1000);
    ")
  })
  
  # Handle return to app
  observeEvent(input$`back-to-app`, {
    removeUI("#fullscreen-iframe", immediate = TRUE)
    shinyjs::hide("iframe-navbar")
    updateTabsetPanel(session, "mainTabs", selected = "Dashboard")
  })
  
  # Initial connection test
  observe({
    invalidateLater(30000) # Check every 30 seconds
    # Try to check server status in background
    tryCatch({
      response <- httr::GET(paste0("http://localhost:", server_config$port, "/"), 
                            timeout(2))
      if (httr::status_code(response) == 200) {
        shinyjs::runjs("$('#connection-status')
          .removeClass('status-disconnected')
          .addClass('status-connected')
          .text('Connected');")
      }
    }, error = function(e) {
      # Silently fail for background checks
    })
  })
}

shinyApp(ui, server)