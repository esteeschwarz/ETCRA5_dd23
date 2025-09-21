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
p.text<-paste0("Analyse the drama and find all speakernames. Extract speakernames as JSON only. dont change speakernames, output as appear in the speaker declaration within the dialogues. dont output any preamble")
p.text
analyze_with_ollama_async <- function(text_sample, p.text,model = DEFAULT_MODEL) {
  # future({
    # Prepare prompt (shorter for faster processing)
    # prompt <- paste0(
    #   "Analyse the drama segmentation into headers (acts, scenes) and speakers within scenes. Usually these are occuring as full line content, but thats not sure. For improvement you ought to c",
    #   "Extract drama structure patterns as JSON only:\n",
    #   '{"h1_patterns": ["act_regex1", "act_regex2"], ',
    #   '"h2_patterns": ["scene_regex1", "scene_regex2"], ',
    #   '"speaker_patterns": ["speaker_regex1", "speaker_regex2"], ',
    #   '"h1_examples": ["Act example"], "h2_examples": ["Scene example"], ',
    #   '"speaker_examples": ["Speaker example"]}\n\n',
    #   "TEXT:\n", substr(text_sample, 1, 2000)  # Limit sample size for speed
    # )
    limit<-"\nDONT output any preamble or introduction, dont output/repeat the spoken text. just output the speakernames and only the speakernames in pure json.\nExtract only the unique speaker names as they appear in the following play. Output a valid JSON array of strings. Do not add any explanation, preamble, or extra text. Only output the JSON. DONT hallucinate, interprete, summarize the text or anything else which is not demanded by this prompt"
    # Prepare prompt for Claude
    prompt <- paste0(p.text,limit,
      "TEXT SAMPLE:\n",
      text_sample    )
    request_body <- list(
      model = model,
      prompt = prompt,
      stream = FALSE,
      options = list(temperature = 0.1, num_predict = 500)
    )
    print(prompt)
    tryCatch({
      response <- POST(
        url = paste0(OLLAMA_BASE_URL, "/api/generate"),
        body = toJSON(request_body, auto_unbox = TRUE),
        add_headers("Content-Type" = "application/json"),
        encode = "raw",
        timeout(100)  # 30 second timeout
      )
     # content(GET(paste0(OLLAMA_BASE_URL,"/api/info")),"text")
      print(content(response,"text"))
      
      if (status_code(response) == 200) {
        result <- fromJSON(content(response, "text", encoding = "UTF-8"))
        response_text <- result$response
       # print(response_text)
      #  response_text<-'{"Algernon": ["ALGERNON"], "Lane": ["LANE"], "Jack": ["JACK"]}'
        # Extract JSON
        json_pattern <- "\\{[^}]*(?:\"[^\"]*\"\\s*:\\s*\\[[^\\]]*\\][^}]*)*\\}"
        json_match <- str_extract(response_text, json_pattern)
        json_match
        #json<-fromJSON(response_text)
        if (!is.na(json_match)) {
          json_match <- str_replace_all(json_match, "\\\\\\\\", "\\\\")
          return(unique(unlist(fromJSON(json_match))))
        }
      }
      print("not 200")
      return(NULL)
    }, error = function(e) {
      print("error...")
      print(content(response,"text"))
      return(NULL)
    })
  
}

# Enhanced fallback patterns

# Apply patterns function (optimized)

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
                  numericInput("sample_start", "sample FROM line", 
                               value = 1, min = 50, max = 500, step = 50),
                  numericInput("sample_end", "sample TO line", 
                               value = 300, min = 50, max = 500, step = 50),
                  textInput("prompt","query...","Analyse the drama and find all speakernames. Extract speakernames as JSON only. dont change speakernames, output as appear in the speaker declaration within the dialogues. dont output any preamble"),
                  selectInput("model", "Ollama Model:", choices = NULL),
                  checkboxInput("use_ai", "Use AI Analysis (uncheck for regex-only)", TRUE),
                  actionButton("analyze", "Analyze Structure", class = "btn-primary"),
                  br(), br(),
                  verbatimTextOutput("progress")
                )
              ),
              
              fluidRow(
                box(
                  title = "speaker", status = "success", solidHeader = TRUE, width = 4,
                  DT::dataTableOutput("speakers")
                )
                # box(
                #   title = "Scenes (H2)", status = "info", solidHeader = TRUE, width = 4,
                #   DT::dataTableOutput("h2_table")
                # ),
                # box(
                #   title = "Speakers", status = "warning", solidHeader = TRUE, width = 4,
                #   DT::dataTableOutput("speakers_table")
                # )
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
  rv <- reactiveValues(
    speaker = NULL
  )
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
    range<-input$sample_start:input$sample_end
    sample_text<-paste0(content[range],collapse = "\n")
    # sample_text <- paste(head(content, input$sample_lines), collapse = "\n")
    print(sample_text)
    # if (input$use_ai) {
      # AI analysis
      patterns<-analyze_with_ollama_async(sample_text, input$prompt,input$model)
        if (is.null(patterns)) {
#          patterns <- get_fallback_patterns()
          output$progress <- renderText("AI analysis failed, using fallback patterns")
        } else {
          output$progress <- renderText("AI analysis complete, applying to full text")
         print(patterns)
        
        # Apply patterns
        # analysis_results <- patterns
         rv$speaker<-patterns
         output$progress <- renderText("Analysis complete!")
      
        } 
  }
  )
  
  # Results tables
  output$speakers <- DT::renderDataTable({
    if (is.null(rv$speaker))
      return(data.frame())
    data.frame(speakers = rv$speaker)
  }, options = list(pageLength = 10, scrollY = "300px"))
  
  # output$h2_table <- DT::renderDataTable({
  #   if (is.null(results())) return(data.frame())
  #   data.frame(Scenes = results()$h2_headers)
  # }, options = list(pageLength = 10, scrollY = "300px"))
  # 
  # output$speakers_table <- DT::renderDataTable({
  #   if (is.null(results())) return(data.frame())
  #   data.frame(Speakers = results()$speakers)
  # }, options = list(pageLength = 15, scrollY = "300px"))
  
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
    if (is.null(rv$speaker))
        return("No analysis results yet")
    
#    r <- rv$speaker
    paste0("Analysis Summary:\n",
           "Speakers found: ", length(rv$speaker))
  
})
  
  # Download handler
  output$download <- downloadHandler(
    filename = function() {
      paste0("drama_analysis_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (is.null(rv$speaker))
          return()
      
#      r <- results()
      max_len <- max(length(rv$speaker), length(r$h2_headers), length(r$speakers))
      
      # Pad vectors to same length
      df <- data.frame(
        speaker = rv$speaker)
        # Scenes = c(r$h2_headers, rep("", max_len - length(r$h2_headers))),
        # Speakers = c(r$speakers, rep("", max_len - length(r$speakers)))
      
      
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