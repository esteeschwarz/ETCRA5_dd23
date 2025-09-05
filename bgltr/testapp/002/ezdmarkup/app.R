library(shiny)
library(shinyAce)

ui <- fluidPage(
  titlePanel("EzDrama Syntax Highlighting Test"),
  
  fluidRow(
    column(6,
           h3("Test Basic Highlighting First"),
           selectInput("test_mode", "Choose Language Mode:", 
                       choices = c("Bash" = "sh",
                                   "R" = "r", 
                                   "Python" = "python",
                                   "JavaScript" = "javascript",
                                   "Text" = "text",
                                   "ezdrama" = "ezdrama"),
                       selected = "sh"),
           aceEditor("test_editor",
                     value = "#!/bin/bash
# This is a bash comment
echo 'Hello World'
for i in {1..5}; do
    echo $i
done",
                     mode = "sh",
                     theme = "monokai",
                     height = "200px")
    ),
    column(6,
           h3("Theme Test"),
           selectInput("test_theme", "Choose Theme:", 
                       choices = c("Monokai" = "monokai",
                                   "GitHub" = "github", 
                                   "Tomorrow" = "tomorrow",
                                   "Solarized Dark" = "solarized_dark",
                                   "TextMate" = "textmate"),
                       selected = "monokai"),
           aceEditor("theme_editor",
                     value = "function hello() {
    // JavaScript comment
    var message = 'Hello World';
    console.log(message);
    return 42;
}",
                     mode = "javascript",
                     theme = "monokai",
                     height = "200px")
    )
  ),
  
  hr(),
  
  h3("EzDrama Editor with Custom Highlighting"),
  
  fluidRow(
    column(12,
           aceEditor("ezdrama_editor",
                     value = "# Act 1 - The Beginning
## Scene 1 - A Room

@JOHN: 
Hello there, Mary. How wonderful to see you!

@MARY: (surprised) 
Oh, John! I didn't expect you so early.

$ The stage lights dim slightly as thunder rumbles outside

@JOHN: 
The weather seems to be turning rather dramatic.
(walks to the window and peers out)

@MARY: 
Indeed it is. (aside) Little does he know what I've discovered.",
                     mode = "text",
                     theme = "github",
                     height = "400px",
                     fontSize = 14,
                     showLineNumbers = TRUE,
                     highlightActiveLine = TRUE,
                     wordWrap = TRUE)
    )
  ),
  
  # Custom CSS for EzDrama highlighting
  tags$head(
    tags$style(HTML("
      /* Apply EzDrama highlighting to the text mode editor */
      .ace_editor .ace_line {
        position: relative;
      }
    ")),
    
    # JavaScript to manually highlight EzDrama patterns
    tags$script(HTML("
      $(document).ready(function() {
        function applyEzDramaHighlighting() {
          setTimeout(function() {
            // Get all ace editors
            $('.ace_editor').each(function() {
              var editorId = $(this).attr('id');
              if (editorId && editorId.includes('ezdrama')) {
                var editor = ace.edit(editorId);
                if (editor) {
                  var session = editor.getSession();
                  
                  // Get all lines
                  var lines = session.getDocument().getAllLines();
                  
                  // Process each line for EzDrama syntax
                  lines.forEach(function(lineText, lineNumber) {
                    var $line = $(editor.container).find('.ace_line').eq(lineNumber);
                    if ($line.length > 0) {
                      var originalText = $line.text();
                      var highlightedText = originalText;
                      
                      // Headers: # or ##
                      if (/^#{1,2}\\s/.test(lineText)) {
                        $line.css({
                          'color': '#000080',
                          'font-weight': 'bold',
                          'background-color': 'rgba(240, 248, 255, 0.3)'
                        });
                      }
                      // Speaker names: @
                      else if (/^@\\s*\\w+/.test(lineText)) {
                        $line.css({
                          'color': '#800000',
                          'font-weight': 'bold'
                        });
                      }
                      // Dollar stage directions
                      else if (/^\\$/.test(lineText)) {
                        $line.css({
                          'color': '#666666',
                          'font-style': 'italic',
                        });
                      }
                      // Comments (# but not ##)
                      else if (/^\\s*#(?!#)/.test(lineText)) {
                        $line.css({
                          'color': '#008000',
                          'font-style': 'italic'
                        });
                      }
                    }
                  });
                  
                  // Highlight parentheses
                  $(editor.container).find('.ace_line').each(function() {
                    var $line = $(this);
                    var text = $line.html();
                    // Simple parentheses highlighting
                    text = text.replace(/(\\([^)]*\\))/g, '<span style=\"color: #666666; font-style: italic; background-color: rgba(249, 249, 249, 0.5);\">$1</span>');
                    $line.html(text);
                  });
                }
              }
            });
          }, 500);
        }
        
        // Apply highlighting initially and on changes
        applyEzDramaHighlighting();
        
        // Reapply when content changes
        $(document).on('shiny:inputchanged', function(event) {
          if (event.name === 'ezdrama_editor') {
            applyEzDramaHighlighting();
          }
        });
      });
    "))
  ),
  
  hr(),
  
  fluidRow(
    column(12,
           h4("Expected EzDrama Highlighting:"),
           tags$ul(
             tags$li(tags$code("# Act 1"), " and ", tags$code("## Scene 1"), " - Headers in blue, bold"),
             tags$li(tags$code("@JOHN:"), " - Speaker names in maroon, bold"),
             tags$li(tags$code("(surprised)"), " - Parenthetical stage directions in gray, italic"),
             tags$li(tags$code("$ Stage direction"), " - Dollar stage directions in gray, italic, left border"),
             tags$li(tags$code("# Comment"), " - Comments in green, italic")
           )
    )
  )
)

server <- function(input, output, session) {
  # Update test editor mode
  observe({
    updateAceEditor(session, "test_editor", mode = input$test_mode)
  })
  
  # Update theme editor
  observe({
    updateAceEditor(session, "theme_editor", theme = input$test_theme)
  })
  
  # Debug: Print editor content to console
  observe({
    if (!is.null(input$test_editor)) {
      cat("Test editor content:", input$test_editor, "\n")
    }
  })
  
  observe({
    if (!is.null(input$ezdrama_editor)) {
      cat("EzDrama editor content:", substr(input$ezdrama_editor, 1, 50), "...\n")
    }
  })
}

shinyApp(ui, server)