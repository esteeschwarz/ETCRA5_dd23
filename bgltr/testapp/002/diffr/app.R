library(shiny)
library(diffr)
library(xml2)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .comparison-container {
        display: flex;
        width: 100%;
        height: 500px;
        border: 1px solid #ddd;
      }
      .pane {
        width: 50%;
        padding: 15px;
        overflow: auto;
        font-family: monospace;
        white-space: pre-wrap;
      }
      .left-pane {
        background-color: #f8f9fa;
        border-right: 2px solid #ccc;
      }
      .right-pane {
        background-color: #fff;
      }
      /* CSS to style the HTML elements in the right pane */
      .right-pane h1, .right-pane h2, .right-pane h3, .right-pane h4, .right-pane h5, .right-pane h6 {
        color: #2c3e50;
        margin: 10px 0;
        font-weight: bold;
      }
      .right-pane strong, .right-pane b {
        font-weight: bold;
        color: #e74c3c;
      }
      .right-pane em, .right-pane i {
        font-style: italic;
        color: #3498db;
      }
      .right-pane u {
        text-decoration: underline;
        color: #9b59b6;
      }
      .right-pane p {
        margin: 8px 0;
        line-height: 1.4;
      }
      .right-pane ul, .right-pane ol {
        margin: 8px 0;
        padding-left: 20px;
      }
      .right-pane li {
        margin: 4px 0;
      }
      .right-pane code {
        background-color: #f8f9fa;
        padding: 2px 4px;
        border-radius: 3px;
        font-family: 'Courier New', monospace;
      }
      .diff-highlight {
        background-color: #fff3cd;
        border: 1px solid #ffeaa7;
      }
    "))
  ),
  titlePanel("HTML Tagging Validation Tool"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textAreaInput("plain_text", "Original Plain Text:", 
                    rows = 10,
                    value = "Welcome to our application\nThis is important content\nPlease read carefully\nVersion 1.0\n\nKey features:\n- Fast processing\n- Accurate results\n- Easy to use"),
      textAreaInput("html_text", "HTML Source (Tagged):", 
                    rows = 10,
                    value = "<h1>Welcome to our application</h1>\n<p>This is <strong>important</strong> content</p>\n<p>Please read <em>carefully</em></p>\n<p>Version <u>2.0</u></p>\n\n<p>Key features:</p>\n<ul>\n<li>Fast processing</li>\n<li>Accurate results</li>\n<li>Easy to use</li>\n</ul>"),
      actionButton("validate", "Validate Tagging")
    ),
    mainPanel(
      width = 9,
      h4("Visual Tagging Validation"),
      div(class = "comparison-container",
          div(class = "pane left-pane", 
              id = "leftDisplay",
              "Original plain text will appear here"
          ),
          div(class = "pane right-pane",
              id = "rightDisplay",
              "HTML source with styling will appear here"
          )
      ),
      h4("Text Content Differences"),
      diffrOutput("content_diff")
    )
  )
)

server <- function(input, output, session) {
  
  # Function to safely render HTML with styling
  render_html_with_style <- function(html_content) {
    if (is.null(html_content) || html_content == "") {
      return("No HTML content")
    }
    
    # Create a safe wrapper that maintains the source code view but applies CSS
    safe_html <- gsub("\n", "<br>", htmlTools::htmlEscape(html_content))
    
    # Apply CSS classes to specific HTML elements for visual distinction
    safe_html <- gsub("&lt;(/?)(h1|h2|h3|h4|h5|h6)&gt;", 
                      "&lt;\\1\\2 class='heading'&gt;", safe_html)
    safe_html <- gsub("&lt;(/?)(strong|b)&gt;", 
                      "&lt;\\1\\2 class='strong'&gt;", safe_html)
    safe_html <- gsub("&lt;(/?)(em|i)&gt;", 
                      "&lt;\\1\\2 class='em'&gt;", safe_html)
    safe_html <- gsub("&lt;(/?)(u)&gt;", 
                      "&lt;\\1\\2 class='u'&gt;", safe_html)
    
    return(HTML(safe_html))
  }
  
  # Function to extract text content from HTML (for comparison)
  extract_text_content <- function(html_content) {
    if (is.null(html_content) || html_content == "") return("")
    
    tryCatch({
      doc <- read_html(html_content)
      text_content <- xml_text(doc)
      # Clean up but maintain structure
      text_content <- gsub("\\s+", " ", text_content)
      text_content <- gsub(" *\\n *", "\n", text_content)
      text_content <- trimws(text_content)
      return(text_content)
    }, error = function(e) {
      # Fallback: remove tags with regex
      text_content <- gsub("<[^>]+>", "", html_content)
      text_content <- gsub("\\s+", " ", text_content)
      text_content <- gsub(" *\\n *", "\n", text_content)
      return(trimws(text_content))
    })
  }
  
  observeEvent(input$validate, {
    req(input$plain_text, input$html_text)
    
    # Update left pane (plain text)
    output$leftDisplay <- renderUI({
      div(style = "font-family: monospace; white-space: pre-wrap;", 
          HTML(gsub("\n", "<br>", htmlTools::htmlEscape(input$plain_text))))
    })
    
    # Update right pane (HTML source with visual styling)
    output$rightDisplay <- renderUI({
      render_html_with_style(input$html_text)
    })
    
    # Perform text content comparison
    html_text_content <- extract_text_content(input$html_text)
    temptx<-tempfile("left.txt")
    temphtm<-tempfile("right.html")
    writeLines(input$plain_text,temptx)
    writeLines(html_text_content,temphtm)
    output$content_diff <- renderDiffr({
      
      # diffr(input$plain_text, html_text_content)
      diffr(temptx,temphtm)
    })
  })
  
  # Initialize
  output$leftDisplay <- renderUI({
    div(style = "font-family: monospace; white-space: pre-wrap;", 
        HTML(gsub("\n", "<br>", htmlTools::htmlEscape("Welcome to our application\nThis is important content\nPlease read carefully\nVersion 1.0\n\nKey features:\n- Fast processing\n- Accurate results\n- Easy to use"))))
  })
  
  output$rightDisplay <- renderUI({
    render_html_with_style("<h1>Welcome to our application</h1>\n<p>This is <strong>important</strong> content</p>\n<p>Please read <em>carefully</em></p>\n<p>Version <u>2.0</u></p>\n\n<p>Key features:</p>\n<ul>\n<li>Fast processing</li>\n<li>Accurate results</li>\n<li>Easy to use</li>\n</ul>")
  })
  
  output$content_diff <- renderDiffr({
    diffr("left.txt","right.html")
  })
}

shinyApp(ui, server)