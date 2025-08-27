library(shiny)
library(diffobj)

ui <- fluidPage(
  titlePanel("Advanced Text Comparison"),
  tags$head(
    tags$style(HTML("
      .diff-container { 
        border: 1px solid #ddd; 
        border-radius: 5px; 
        padding: 10px; 
        margin: 10px 0; 
        background: #f8f9fa;
      }
      .diff-header {
        background: #e9ecef;
        padding: 10px;
        border-radius: 3px;
        margin-bottom: 10px;
      }
    "))
  ),
  
  fluidRow(
    column(6,
           div(class = "diff-container",
               h4("Original Text", class = "diff-header"),
               textAreaInput("original_text", NULL, 
                             "This is the first version of the transcript.\nIt contains several errors and typos.\nSome sentences need rephrasing.\nThe overall structure could be improved.",
                             rows = 12, width = "100%")
           )
    ),
    column(6,
           div(class = "diff-container",
               h4("Corrected Text", class = "diff-header"),
               textAreaInput("corrected_text", NULL, 
                             "This is the corrected version of the transcript.\nIt contains no errors and is properly formatted.\nAll sentences have been rephrased for clarity.\nThe structure has been significantly improved.",
                             rows = 12, width = "100%")
           )
    )
  ),
  
  fluidRow(
    column(12,
           actionButton("compare", "Compare Texts", class = "btn-primary btn-lg"),
           selectInput("view_mode", "View Mode:",
                       choices = c("Side by Side" = "sidebyside",
                                   "Unified" = "unified",
                                   "Context" = "context"),
                       selected = "sidebyside"),
           div(class = "diff-container",
               h4("Comparison Results", class = "diff-header"),
               uiOutput("diff_display")
           )
    )
  )
)

server <- function(input, output) {
  
  output$diff_display <- renderUI({
    input$compare
    
    isolate({
      if (nchar(input$original_text) > 0 && nchar(input$corrected_text) > 0) {
        lines1 <- unlist(strsplit(input$original_text, "\n"))
        lines2 <- unlist(strsplit(input$corrected_text, "\n"))
        
        diff <- diffobj::diffChr(
          lines1, 
          lines2,
          mode = input$view_mode,
          format = "html",
          style = list(html.output = "page")
        )
        
        return(diff)
      } else {
        return(HTML("<p>Please enter text in both fields to compare.</p>"))
      }
    })
  })
}

shinyApp(ui, server)