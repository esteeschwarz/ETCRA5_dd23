#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })
    xml.test<-c("<p>testxmlrender</p>","<h1>head1</h1><p>paragraph</p>")
    
        xml.str<-paste0("<html><body>",paste0(xml.test,collapse = ""),"</body></html>")
   # xml.str<-paste0(readLines(output_file),collapse = "")
    #print("----- xmlstr ------")
    # print(xml.str)
   # b64 <- jsonlite::base64_enc(charToRaw(xml.str))
    xmltemp<-tempfile("temp.html")
    writeLines(xml.str,xmltemp)
    xml.li<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/testapp/002/testrender/www/r-tempxmlout.xml")
    xml.str<-paste0(xml.li,collapse = "")
    b64 <- jsonlite::base64_enc(charToRaw(xml.str))
    
    output$xmlrendered <- renderUI({
      # div(id="xml",
      # style="width:100%; height:100%;",
      tags$iframe(
       src = paste0("data:application/xml;base64,", b64),
       # src = "r-tempxmlout.xml",
        # src = output_file_s_www, # this wks.
         # src = xmltemp,
        #  src = paste0("data:text/xml,", xml.str),
         style="width:100%; height:100vH; border:none;"
      )
    })

}
