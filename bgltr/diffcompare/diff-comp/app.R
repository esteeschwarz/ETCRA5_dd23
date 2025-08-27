library(shiny)
library(diffobj)
library(shinyAce)

ui <- fluidPage(
  titlePanel("Text Comparison Tool with Proper HTML Rendering"),
  tags$head(
    tags$style(HTML("
      .diff-container { 
        border: 1px solid #ddd; 
        border-radius: 5px; 
        padding: 10px; 
        margin: 10px 0; 
        background: #f8f9fa;
        overflow-x: auto;
      }
      .diff-header {
        background: #e9ecef;
        padding: 10px;
        border-radius: 3px;
        margin-bottom: 10px;
      }
      .ace_editor {
        border: 1px solid #ddd;
        border-radius: 4px;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "diff-container",
          h4("Original Text", class = "diff-header"),
          aceEditor("text1", mode = "text", height = "200px", 
                    value = "This is the original text.\nIt contains some errors.\nWe need to correct them.\nThere might be missing words or typos.\nThe structure could be improved."),
          h4("Corrected Text", class = "diff-header"),
          aceEditor("text2", mode = "text", height = "200px", 
                    value = "This is the corrected text.\nIt contains no errors.\nWe fixed all issues.\nThere are no missing words or typos.\nThe structure has been significantly improved.")
      ),
      br(),
      actionButton("compare", "Compare Texts", class = "btn-primary", icon = icon("code-compare")),
      br(), br(),
      selectInput("diff_style", "Diff Style:",
                  choices = c("Auto" = "auto", 
                              "Side by Side" = "sidebyside", 
                              "Unified" = "unified", 
                              "Context" = "context"),
                  selected = "sidebyside"),
      sliderInput("context_size", "Context Size:",
                  min = 0, max = 10, value = 3, step = 1)
    ),
    mainPanel(
      width = 9,
      div(class = "diff-container",
          h4("Differences", class = "diff-header"),
          uiOutput("diff_output")
      )
    )
  )
)

server <- function(input, output) {
  
  diff_result <- eventReactive(input$compare, {
    text1 <- input$text1
    text2 <- input$text2
    
    # Split into lines for better diff display
    lines1 <- unlist(strsplit(text1, "\n"))
    lines2 <- unlist(strsplit(text2, "\n"))
    
    # Create diff object
    tryCatch({
      diff <- diffobj::diffChr(
        lines1, 
        lines2,
        mode = input$diff_style,
        format = "html",
        #contextSize = input$context_size,
        style = list(html.output = "page")
      )
      
      # Convert to HTML
      htmltools::HTML(as.character(diff))
    }, error = function(e) {
      HTML(paste0("<div class='alert alert-danger'>Error: ", e$message, "</div>"))
    })
  })
  
  output$diff_output <- renderUI({
    if (input$compare == 0) {
      return(HTML("<div class='alert alert-info'>Click 'Compare Texts' to see the differences</div>"))
    }
    
    diff_result()
  })
}

shinyApp(ui, server)