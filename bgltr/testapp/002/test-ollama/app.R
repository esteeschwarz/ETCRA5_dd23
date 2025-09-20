# Shiny App: Drama Structure Analyzer with Ollama
# Optimized for server deployment

library(shiny)
library(shinydashboard)
library(DT)
library(httr)
library(jsonlite)
library(stringr)
library(dplyr)
library(promises)
library(future)

# Enable async processing for better server performance
plan(multisession)

tlama<-function(){
turl<-"http://localhost:11434/api/tags"
r<-GET(turl)
t<-content(r,"text")
print(t)
}

# Ollama configuration
OLLAMA_BASE_URL <- Sys.getenv("OLLAMA_URL", "http://localhost:11434")
DEFAULT_MODEL <- Sys.getenv("OLLAMA_MODEL", "llama3.2")

# Cache for Ollama status to avoid repeated checks
ollama_cache <- reactiveVal(list(last_check = NULL, status = NULL))
#x<-models_info$models
#x$name
#x
# Ollama functions (optimized for Shiny)
check_ollama_status_async <- function() {
  future({
    tryCatch({
      response <- GET(paste0(OLLAMA_BASE_URL, "/api/tags"), timeout(5))
      if (status_code(response) == 200) {
        models_info <- fromJSON(content(response, "text", encoding = "UTF-8"))
        models <- sapply(models_info, function(x) x$name)
        list(running = TRUE, models = models, error = NULL)
      } else {
        list(running = FALSE, models = character(0), error = "Not responding")
      }
    }, error = function(e) {
      list(running = FALSE, models = character(0), error = e$message)
    })
  })
}

analyze_with_ollama_async <- function(text_sample, model = DEFAULT_MODEL) {
  future({
    # Prepare prompt (shorter for faster processing)
    prompt <- paste0(
      "Analyse the drama segmentation into headers (acts, scenes) and speakers within scenes. Usually these are occuring as full line content, but thats not sure. For improvement you ought to c",
      "Extract drama structure patterns as JSON only:\n",
      '{"h1_patterns": ["act_regex1", "act_regex2"], ',
      '"h2_patterns": ["scene_regex1", "scene_regex2"], ',
      '"speaker_patterns": ["speaker_regex1", "speaker_regex2"], ',
      '"h1_examples": ["Act example"], "h2_examples": ["Scene example"], ',
      '"speaker_examples": ["Speaker example"]}\n\n',
      "TEXT:\n", substr(text_sample, 1, 2000)  # Limit sample size for speed
    )
    # Prepare prompt for Claude
    prompt <- paste0(p.text,
      "TEXT SAMPLE:\n",
      substr(text_sample, 1, 2000)    )
    request_body <- list(
      model = model,
      prompt = prompt,
      stream = FALSE,
      options = list(temperature = 0.1, num_predict = 500)
    )
    
    tryCatch({
      response <- POST(
        url = paste0(OLLAMA_BASE_URL, "/api/generate"),
        body = toJSON(request_body, auto_unbox = TRUE),
        add_headers("Content-Type" = "application/json"),
        encode = "raw",
        timeout(30)  # 30 second timeout
      )
      
      if (status_code(response) == 200) {
        result <- fromJSON(content(response, "text", encoding = "UTF-8"))
        response_text <- result$response
        
        # Extract JSON
        json_pattern <- "\\{[^}]*(?:\"[^\"]*\"\\s*:\\s*\\[[^\\]]*\\][^}]*)*\\}"
        json_match <- str_extract(response_text, json_pattern)
        
        if (!is.na(json_match)) {
          json_match <- str_replace_all(json_match, "\\\\\\\\", "\\\\")
          return(fromJSON(json_match))
        }
      }
      return(NULL)
    }, error = function(e) {
      return(NULL)
    })
  })
}

# Enhanced fallback patterns
get_fallback_patterns <- function() {
  list(
    h1_patterns = c(
      "^\\s*FØRSTE\\s+AKT", "^\\s*ANDEN\\s+AKT", "^\\s*TREDJE\\s+AKT",
      "^\\s*Erster\\s+Aufzug", "^\\s*Zweiter\\s+Aufzug",
      "^\\s*ACT\\s+[IVX]+", "^\\s*Act\\s+[0-9]+"
    ),
    h2_patterns = c(
      "^\\s*.*AKT,\\s*SCENE\\s+[0-9IVX]+", "^\\s*SCENE\\s+[0-9IVX]+",
      "^\\s*.*Auftritt", "^\\s*[0-9]+\\.\\s*Auftritt"
    ),
    speaker_patterns = c(
      "^\\s*[A-ZÆØÅ][A-ZÆØÅ\\s'-]*[A-ZÆØÅ]\\s*:",
      "^\\s*[A-Z][a-zA-ZÆØÅæøå\\s'-]+\\s*:",
      "^\\s*[A-ZÄÖÜ][A-ZÄÖÜß\\s'-]*\\s*\\."
    ),
    h1_examples = c("FØRSTE AKT", "Erster Aufzug"),
    h2_examples = c("SCENE 1", "Erster Auftritt"),
    speaker_examples = c("HAMLET:", "Der Prinz.")
  )
}

# Apply patterns function (optimized)
apply_patterns <- function(text_lines, patterns) {
  if (length(text_lines) == 0) return(list(h1_headers = c(), h2_headers = c(), speakers = c()))
  
  # Filter non-empty lines
  clean_lines <- text_lines[!is.na(text_lines) & str_trim(text_lines) != ""]
  
  extract_matches <- function(lines, patterns) {
    matches <- c()
    for (pattern in patterns) {
      tryCatch({
        new_matches <- lines[str_detect(lines, regex(pattern, ignore_case = TRUE))]
        matches <- c(matches, new_matches)
      }, error = function(e) {})
    }
    unique(str_trim(matches[matches != ""]))
  }
  
  list(
    h1_headers = extract_matches(clean_lines, patterns$h1_patterns %>% c()),
    h2_headers = extract_matches(clean_lines, patterns$h2_patterns %>% c()),
    speakers = extract_matches(clean_lines, patterns$speaker_patterns %>% c())
  )
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Drama Structure Analyzer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analysis", tabName = "analysis", icon = icon("search")),
      menuItem("System Status", tabName = "status", icon = icon("server"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  title = "Upload Drama Text", status = "primary", solidHeader = TRUE, width = 12,
                  fileInput("file", "Choose Text File",
                            accept = c(".txt", ".text")),
                  numericInput("sample_lines", "Sample Lines for AI Analysis:", 
                               value = 100, min = 50, max = 500, step = 50),
                  selectInput("model", "Ollama Model:", choices = NULL),
                  checkboxInput("use_ai", "Use AI Analysis (uncheck for regex-only)", TRUE),
                  actionButton("analyze", "Analyze Structure", class = "btn-primary"),
                  br(), br(),
                  verbatimTextOutput("progress")
                )
              ),
              
              fluidRow(
                box(
                  title = "Acts (H1)", status = "success", solidHeader = TRUE, width = 4,
                  DT::dataTableOutput("h1_table")
                ),
                box(
                  title = "Scenes (H2)", status = "info", solidHeader = TRUE, width = 4,
                  DT::dataTableOutput("h2_table")
                ),
                box(
                  title = "Speakers", status = "warning", solidHeader = TRUE, width = 4,
                  DT::dataTableOutput("speakers_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Export Results", status = "primary", solidHeader = TRUE, width = 12,
                  downloadButton("download", "Download Results as CSV", class = "btn-success"),
                  br(), br(),
                  verbatimTextOutput("summary")
                )
              )
      ),
      
      tabItem(tabName = "status",
              fluidRow(
                box(
                  title = "Ollama Status", status = "primary", solidHeader = TRUE, width = 6,
                  verbatimTextOutput("ollama_status"),
                  br(),
                  actionButton("refresh_status", "Refresh Status", class = "btn-info")
                ),
                box(
                  title = "Performance", status = "success", solidHeader = TRUE, width = 6,
                  verbatimTextOutput("performance_info")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  results <- reactiveVal(NULL)
  file_content <- reactiveVal(NULL)
  
  # Check Ollama status on startup
  observe({
    check_ollama_status_async() %...>% {
      status <- .
      ollama_cache(list(last_check = Sys.time(), status = status))
      
      if (status$running && length(status$models) > 0) {
        updateSelectInput(session, "model", 
                          choices = status$models, 
                          selected = if(DEFAULT_MODEL %in% status$models) DEFAULT_MODEL else status$models[1])
      } else {
        updateSelectInput(session, "model", choices = "No models available")
      }
    }
  })
  
  # File upload handler
  observeEvent(input$file, {
    if (is.null(input$file)) return()
    
    # Try different encodings
    content <- NULL
    for (enc in c("UTF-8", "latin1", "CP1252")) {
      tryCatch({
        content <- readLines(input$file$datapath, encoding = enc, warn = FALSE)
        break
      }, error = function(e) {})
    }
    
    if (!is.null(content)) {
      file_content(content)
      output$progress <- renderText(paste("File loaded:", length(content), "lines"))
    } else {
      output$progress <- renderText("Error: Could not read file")
    }
  })
  
  # Analysis handler
  observeEvent(input$analyze, {
    if (is.null(file_content())) {
      output$progress <- renderText("Please upload a file first")
      return()
    }
    
    output$progress <- renderText("Analyzing...")
    content <- file_content()
    sample_text <- paste(head(content, input$sample_lines), collapse = "\n")
    
    if (input$use_ai) {
      # AI analysis
      analyze_with_ollama_async(sample_text, input$model) %...>% {
        patterns <- .
        if (is.null(patterns)) {
          patterns <- get_fallback_patterns()
          output$progress <- renderText("AI analysis failed, using fallback patterns")
        } else {
          output$progress <- renderText("AI analysis complete, applying to full text")
        }
        
        # Apply patterns
        analysis_results <- apply_patterns(content, patterns)
        results(analysis_results)
        output$progress <- renderText("Analysis complete!")
      }
    } else {
      # Direct regex analysis
      patterns <- get_fallback_patterns()
      analysis_results <- apply_patterns(content, patterns)
      results(analysis_results)
      output$progress <- renderText("Regex analysis complete!")
    }
  })
  
  # Results tables
  output$h1_table <- DT::renderDataTable({
    if (is.null(results())) return(data.frame())
    data.frame(Acts = results()$h1_headers)
  }, options = list(pageLength = 10, scrollY = "300px"))
  
  output$h2_table <- DT::renderDataTable({
    if (is.null(results())) return(data.frame())
    data.frame(Scenes = results()$h2_headers)
  }, options = list(pageLength = 10, scrollY = "300px"))
  
  output$speakers_table <- DT::renderDataTable({
    if (is.null(results())) return(data.frame())
    data.frame(Speakers = results()$speakers)
  }, options = list(pageLength = 15, scrollY = "300px"))
  
  # Status display
  output$ollama_status <- renderText({
    cache <- ollama_cache()
    if (is.null(cache$status)) {
      return("Checking Ollama status...")
    }
    
    status <- cache$status
    if (status$running) {
      paste0("✓ Ollama is running\n",
             "Available models: ", paste(status$models, collapse = ", "),
             "\nLast checked: ", format(cache$last_check, "%H:%M:%S"))
    } else {
      paste0("✗ Ollama not available\n",
             "Error: ", status$error %||% "Unknown",
             "\nUsing fallback patterns only")
    }
  })
  
  # Performance info
  output$performance_info <- renderText({
    paste0("Server Resources:\n",
           "R Session: ", format(object.size(ls()), units = "MB"), "\n",
           "Recommended Ollama models:\n",
           "- phi3 (4GB RAM)\n", 
           "- llama3.2 (4GB RAM)\n",
           "- mistral (7GB RAM)")
  })
  
  # Summary
  output$summary <- renderText({
    if (is.null(results())) return("No analysis results yet")
    
    r <- results()
    paste0("Analysis Summary:\n",
           "Acts found: ", length(r$h1_headers), "\n",
           "Scenes found: ", length(r$h2_headers), "\n", 
           "Speakers found: ", length(r$speakers))
  })
  
  # Download handler
  output$download <- downloadHandler(
    filename = function() {
      paste0("drama_analysis_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (is.null(results())) return()
      
      r <- results()
      max_len <- max(length(r$h1_headers), length(r$h2_headers), length(r$speakers))
      
      # Pad vectors to same length
      df <- data.frame(
        Acts = c(r$h1_headers, rep("", max_len - length(r$h1_headers))),
        Scenes = c(r$h2_headers, rep("", max_len - length(r$h2_headers))),
        Speakers = c(r$speakers, rep("", max_len - length(r$speakers)))
      )
      
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Refresh status
  observeEvent(input$refresh_status, {
    check_ollama_status_async() %...>% {
      status <- .
      ollama_cache(list(last_check = Sys.time(), status = status))
    }
  })
}

# Run app
shinyApp(ui = ui, server = server)