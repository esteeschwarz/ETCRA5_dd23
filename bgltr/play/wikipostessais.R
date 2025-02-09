library(httr)
#page<-"https://en.wikisource.beta.wmflabs.org/wiki/Index:Human_Rights_Act_1998.pdf"
page<-"Index:Human_Rights_Act_1998.pdf"
# read_url<-paste0("https://de.wikisource.org/w/api.php?action=query&prop=extracts&titles=",page)
format<-"json"
#raw<-paste0("https://de.wikisource.org/w/index.php?action=raw&format=",format,"&title=",page)
page.get.content<-function(api,page){
  # #page<-url_escape(page)
  # read_url<-paste0("https://de.wikisource.org/w/api.php?action=query&format=json&prop=revisions&titles=",page,"&formatversion=2&rvprop=content&rvslots=*")
  # read_url<-paste0("https://de.wikisource.org/w/api.php?action=query&prop=extracts&titles=",page)
  # "https://de.wikisource.org/w/api.php?action=query&prop=extracts&exchars=175&titles=Seite:Steltzer_montenegro.pdf/5"
  page
  format<-"json"
  raw<-paste0(api,"?action=raw&format=",format,"&title=",page)
  x<-GET(raw)
  r<-content(x,"text")
}
api<-paste0("https://en.wikisource.beta.wmflabs.org/wiki/index.php?action=raw&format=",format,"&title=",page)
api<-"https://en.wikisource.beta.wmflabs.org/w/index.php"
api.de<-"https://de.wikisource.org/w/index.php"
page<-"Index:Human_Rights_Act_1998.pdf"
# x<-GET(api)
# r<-content(x,"text")
page<-"Index:Steltzer_montenegro.pdf"
page<-"indexTemplate"
#raw<-paste0("https://de.wikisource.org/w/index.php?action=raw&format=",format,"&title=",page)
page<-"MediaWiki:Proofreadpage_index_template"
page<-"Vorlage:Index"
page<-"Module:Index template/styles.css"
t1<-page.get.content(api,page)
t1<-page.get.content(api.de,page)
writeLines(t1,"~/Documents/GitHub/ETCRA5_dd23/bgltr/play/PR.ws.de.template.txt")





