# Install required packages
# install.packages(c("shiny", "diffobj", "shinyAce"))

library(shiny)
library(diffobj)
library(shinyAce)

ui <- fluidPage(
  titlePanel("Text Comparison Tool"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Original Text"),
      aceEditor("text1", mode = "text", height = "200px", value = "This is the original text.\nIt contains some errors.\nWe need to correct them."),
      h4("Corrected Text"),
      aceEditor("text2", mode = "text", height = "200px", value = "This is the corrected text.\nIt contains no errors.\nWe fixed all issues."),
      actionButton("compare", "Compare Texts", class = "btn-primary"),
      br(), br(),
      selectInput("diff_style", "Diff Style:",
                  choices = c("auto", "unified", "sidebyside", "context"),
                  selected = "sidebyside")
    ),
    mainPanel(
      width = 9,
      h3("Differences"),
      uiOutput("diff_output")
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
    diff <- diffobj::diffChr(
      lines1, 
      lines2,
      mode = input$diff_style,
      format = "html",
      style = list(html.output = "page")
    )
    
    return(diff)
  })
  
  output$diff_output <- renderUI({
    diff_result()
  })
}

shinyApp(ui, server)