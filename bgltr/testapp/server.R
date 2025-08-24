library(shiny)
# Define server logic
transform.ezd<-function(input){
  t<-readLines(input)
  
}
function(input, output, session) {
  # Reactive values to store intermediate states
  rv <- reactiveValues(
    t1 = "input docname...",
    t2 = NULL,
    t3 = NULL,
    heads = "not defined...",
    speaker = array(),
    speaker.crit = array(),
    h1.sf = "",
    h2.sf = "",
    sp.sf = "",
    id.sf = NULL,
    cast = NULL
  )
  observeEvent(input$submit.xml, {
    xml.t<-transform.ezd("../dracorTEI/hamlet.xml")
#    xml.test<-c("<p>testxmlrender</p>","<h1>head1</h1><p><stage>stages</stage>paragraph</p>")
   # xml.test<-list.files(".")
#    xml.str<-paste0("<div>",paste0(xml.t),"</div>")
    xml.str<-paste0(xml.t,collapse = "")
    print("----- xmlstr ------")
    print(xml.str)
    b64 <- jsonlite::base64_enc(charToRaw(xml.str))
    output$xmlrendered <- renderUI({
   # div(id="xml",
     # style="width:100%; height:100%;",
      tags$iframe(
      #  src = paste0("data:application/xml;base64,", b64),
       src = "hamlet.xml",
        style="width:100%; height:100%; border:none;"
      )
    })
  })
 
}