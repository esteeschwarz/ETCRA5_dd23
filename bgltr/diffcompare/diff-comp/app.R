# install.packages("diffr")

library(shiny)
library(diffr)

ui <- fluidPage(
  tags$style(HTML("
      .td.code.replace.after {
       white-space: pre !important;
        overflow-x: auto !important;
      }
      .td.code.equal.after {
       white-space: pre !important;
        overflow-x: auto !important;
      }")),
  titlePanel("Text Diff Tool"),
  fluidRow(
    column(6,
           h4("Original Text"),
           textAreaInput("text1", NULL, 
                         "ezdmarkup.txt",
                         rows = 10, width = "100%")
    ),
    column(6,
           h4("Corrected Text"),
           textAreaInput("text2", NULL, 
                         "goue_iwanette_ggl.txt",
                         rows = 10, width = "100%")
    )
  ),
  actionButton("diff_btn", "Show Differences", class = "btn-primary"),
  br(), br(),
  diffrOutput("diff_output")
)
?diffr

server <- function(input, output) {
  
  output$diff_output <- renderDiffr({
    input$diff_btn
    isolate({
      # div(style="height:50%;",
          
      diffr(
        file1 = input$text1,
        file2 = input$text2,
        before = "Original",
        after = "Corrected",
        contextSize = 3,
        wordWrap = TRUE
      )
      # )
    })
    # )
  })
}

shinyApp(ui, server)