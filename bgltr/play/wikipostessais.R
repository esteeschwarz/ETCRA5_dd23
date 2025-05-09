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
  raw<-paste0(api,"/index.php?action=raw&format=",format,"&title=",page)
  x<-GET(raw)
  r<-content(x,"text")
}
api<-paste0("https://en.wikisource.beta.wmflabs.org/wiki/index.php?action=raw&format=",format,"&title=",page)
api<-"https://en.wikisource.beta.wmflabs.org/w/index.php"
api.de<-"https://de.wikisource.org/w"
api.m<-"http://mini12/w"
mwiki.dhm<-credits$mwiki
page<-"Index:Human_Rights_Act_1998.pdf"
# x<-GET(api)
# r<-content(x,"text")
page<-"Index:Steltzer_montenegro.pdf"
page<-"indexTemplate"
credits<-get_credit(c(F,4,F))

#raw<-paste0("https://de.wikisource.org/w/index.php?action=raw&format=",format,"&title=",page)
page<-"MediaWiki:Proofreadpage_index_template"
page<-"Vorlage:Seitenstatus2"
page<-"Module:Index template/styles.css"
page<-"Vorlage:Index/styles.css"
page<-"Seite:Hb09201_wstest.pdf/2"
page<-"Vorlage:Zitierempfehlung"
page<-"Seite:Steltzer_montenegro.pdf/77"
page<-"Vorlage:Textdaten"
t1<-page.get.content(api,page)
t1<-page.get.content(api.de,page)
t1<-page.get.content(api.m,page)
t1<-page.get.content(mwiki.dhm,page)
writeLines(t1,"~/Documents/GitHub/ETCRA5_dd23/bgltr/play/de.textdaten.txt")
t1
page<-"Seite:Hb09201_wstest.pdf/2"
"https://de.wikisource.org/w/api.php?action=query&prop=info&titles=Hauptseite"
"https://www.mediawiki.org/w/api.php?action=query&list=recentchanges"
url<-paste0(credits$url,"?action=query&list=recentchanges&format=json")
x<-GET(url)
t1<-content(x,"text")
t1
library(jsonlite)
t2<-fromJSON(t1,simplifyDataFrame = T)
t2
t2<-page.get.info(page,c(F,5,F))
t2
