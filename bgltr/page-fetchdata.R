library(readr)

# Read the CSV file
#csv_data <- read_csv("~/Documents/GitHub/ETCRA5_dd23/bgltr/data/wikisource_pages-data.csv"
csv_data <- read_csv("~/Documents/GitHub/ETCRA5_dd23/bgltr/data/pages-data.csv")

# Example manipulation: Create a list of entries
entries.index <- apply(csv_data, 1, function(row) {
  #row<-1
  c("{{:MediaWiki:Proofreadpage_index_template\n",
         "|BILD=[[File:", row["file"], "|page=2|thumb|", row["title"], "]]\n",
         "|AUTOR=", row["auth_entry"], "\n",
         "|TITEL=", row["title"], "\n",
         "|VERLAG=", row["publisher"], "\n",
         "|JAHR=", row["year"], "\n",
         "|ORT=", row["location"], "\n",
         "|QUELLE=[", row["src_orig_loc"], "] =",row["src_wiki_loc"],"\n",
         "|SEITEN=<pagelist />\n",
         "|ER=siehe [[Wikisource:Editionsrichtlinien]]\n",
         "|PROJEKTFORTSCHRITT=", row["progress"], "\n",
         "|BEARBEITUNGSSTAND=", row["status"], "\n",
         "}}")
})
entries.index[6]
#entries.pages<-apply(csv_data,1,function(row){
    paste0("{{Textdaten\n",
"|VORIGER=",
"|NÄCHSTER=",
"|AUTOR=", row["auth_entry"], "\n",
"|TITEL=", row["title"], "\n",
"|SUBTITEL=",row["subtitle"],"\n",
"|HERKUNFT",row["origin"],"\n",
"|HERAUSGEBER",row["hrsg"],"\n",
"|VERLAG=", row["publisher"], "\n",
"|AUFLAGE",row["edition"],"\n",
"|ENTSTEHUNGSJAHR",row["y_written"],"\n",
"|ERSCHIENEN",row["y_pub"],"\n",
"|ERSCHEINUNGSORT",row["location"],"\n",
"|ÜBERSETZER",row["translator"],"\n",
"|ORIGINALTITEL",row["title_orig"],"\n",
"|ORIGINALSUBTITEL",row["subtitle_orig"],"\n",
"|ORIGINALHERKUNFT=", row["origin_orig"], "\n",
"|WIKIPEDIA=",row["wikipedia"],"\n",
"|BILD=",row["pdf_wiki_pg_entry"],"\n",
"|QUELLE=[", row["src_orig_loc"], "] =",row["src_wiki_loc"],"\n",
"|KURZBESCHREIBUNG=",row["description"],"\n",
"|SONSTIGES=",row["alii"],"\n",
"|BENUTZERHILFE=on",
"|INDEXSEITE=",row["ns_pg_index"],"\n",
"|BEARBEITUNGSSTAND=",row["status"],"\n",
"}}",

"{{BlockSatzStart}}",
rep(
"{{SeitePR|1|Bodmer polytimet 1760.pdf/1}}
{{SeitePR|2|Bodmer polytimet 1760.pdf/2}}
{{SeitePR|3|Bodmer polytimet 1760.pdf/3}}
{{SeitePR|4|Bodmer polytimet 1760.pdf/4}}
{{SeitePR|5|Bodmer polytimet 1760.pdf/5}}
{{SeitePR|6|Bodmer polytimet 1760.pdf/6}}
{{SeitePR|7|Bodmer polytimet 1760.pdf/7}}
{{SeitePR|8|Bodmer polytimet 1760.pdf/8}}
{{SeitePR|9|Bodmer polytimet 1760.pdf/9}}
{{SeitePR|10|Bodmer polytimet 1760.pdf/10}}
{{SeitePR|11|Bodmer polytimet 1760.pdf/11}}
{{SeitePR|12|Bodmer polytimet 1760.pdf/12}}
{{SeitePR|13|Bodmer polytimet 1760.pdf/13}}
{{SeitePR|14|Bodmer polytimet 1760.pdf/14}}
{{SeitePR|15|Bodmer polytimet 1760.pdf/15}}
{{SeitePR|16|Bodmer polytimet 1760.pdf/16}}
{{SeitePR|17|Bodmer polytimet 1760.pdf/17}}
{{SeitePR|18|Bodmer polytimet 1760.pdf/18}}
{{SeitePR|19|Bodmer polytimet 1760.pdf/19}}
{{SeitePR|20|Bodmer polytimet 1760.pdf/20}}
{{SeitePR|21|Bodmer polytimet 1760.pdf/21}}
{{SeitePR|22|Bodmer polytimet 1760.pdf/22}}
{{SeitePR|23|Bodmer polytimet 1760.pdf/23}}
{{SeitePR|24|Bodmer polytimet 1760.pdf/24}}
{{SeitePR|25|Bodmer polytimet 1760.pdf/25}}
{{SeitePR|26|Bodmer polytimet 1760.pdf/26}}
{{SeitePR|27|Bodmer polytimet 1760.pdf/27}}
{{SeitePR|28|Bodmer polytimet 1760.pdf/28}}
{{SeitePR|29|Bodmer polytimet 1760.pdf/29}}
{{SeitePR|30|Bodmer polytimet 1760.pdf/30}}
{{SeitePR|31|Bodmer polytimet 1760.pdf/31}}
{{SeitePR|32|Bodmer polytimet 1760.pdf/32}}
{{SeitePR|33|Bodmer polytimet 1760.pdf/33}}
{{SeitePR|34|Bodmer polytimet 1760.pdf/34}}
{{SeitePR|35|Bodmer polytimet 1760.pdf/35}}
{{SeitePR|36|Bodmer polytimet 1760.pdf/36}}
{{SeitePR|37|Bodmer polytimet 1760.pdf/37}}
{{SeitePR|38|Bodmer polytimet 1760.pdf/38}}
{{SeitePR|39|Bodmer polytimet 1760.pdf/39}}
{{SeitePR|40|Bodmer polytimet 1760.pdf/40}}
{{SeitePR|41|Bodmer polytimet 1760.pdf/41}}
{{SeitePR|42|Bodmer polytimet 1760.pdf/42}}
{{SeitePR|43|Bodmer polytimet 1760.pdf/43}}
{{SeitePR|44|Bodmer polytimet 1760.pdf/44}}
{{SeitePR|45|Bodmer polytimet 1760.pdf/45}}
{{SeitePR|46|Bodmer polytimet 1760.pdf/46}}
{{SeitePR|47|Bodmer polytimet 1760.pdf/47}}
{{SeitePR|48|Bodmer polytimet 1760.pdf/48}}
{{SeitePR|49|Bodmer polytimet 1760.pdf/49}}
{{SeitePR|50|Bodmer polytimet 1760.pdf/50}}
{{SeitePR|51|Bodmer polytimet 1760.pdf/51}}
{{SeitePR|52|Bodmer polytimet 1760.pdf/52}}
{{SeitePR|53|Bodmer polytimet 1760.pdf/53}}
{{SeitePR|54|Bodmer polytimet 1760.pdf/54}}
{{SeitePR|55|Bodmer polytimet 1760.pdf/55}}
{{SeitePR|56|Bodmer polytimet 1760.pdf/56}}
{{SeitePR|57|Bodmer polytimet 1760.pdf/57}}
{{SeitePR|58|Bodmer polytimet 1760.pdf/58}}
{{BlockSatzEnd}}
[[Kategorie:Deutsche Philologie]]
[[Kategorie:1760er Jahre]]
[[Kategorie:Deutschland]]
[[Kategorie:Drama]]"
})
library(httr)

# Function to update a Wikisource page
update_wikisource_page <- function(page_title, content, username, password) {
  # Login to Wikisource
  login_url <- "https://en.wikisource.org/w/api.php?action=login&format=json"
  login_url <- "https://de.wikisource.org/w/api.php?action=login&format=json"
  login_response <- POST(login_url, body = list(lgname = username, lgpassword = password))
  login_token <- content(login_response)$login$token
  #login_response$cookies$value
  # Get edit token
  edit_token_url <- "https://de.wikisource.org/w/api.php?action=query&meta=tokens&format=json"
  edit_token_response <- GET(edit_token_url, set_cookies(login_response$cookies$value))
  #r<-content(edit_token_response)
  #r$query
  edit_token <- content(edit_token_response)$query$tokens$csrftoken
  
  # Edit the page
  edit_url <- "https://de.wikisource.org/w/api.php?action=parse&format=json"
  #edit_url <- "https://de.wikisource.org/w/api.php?action=edit&format=json"
  edit_response <- POST(edit_url, body = list(
    title = page_title,
    #page = page_title,
    text = content,
    token = edit_token
  ), set_cookies(login_response$cookies$value))
  
  return(content(edit_response))
}
#library(jsonlite)
#library(xml2)
#?url_escape
page_title<-"Seite:Steltzer_montenegro.pdf/5"
page<-page_title
#page<-url_escape(page)
page.get.content<-function(page){
# #page<-url_escape(page)
# read_url<-paste0("https://de.wikisource.org/w/api.php?action=query&format=json&prop=revisions&titles=",page,"&formatversion=2&rvprop=content&rvslots=*")
# read_url<-paste0("https://de.wikisource.org/w/api.php?action=query&prop=extracts&titles=",page)
# "https://de.wikisource.org/w/api.php?action=query&prop=extracts&exchars=175&titles=Seite:Steltzer_montenegro.pdf/5"
page
raw<-paste0("https://de.wikisource.org/w/index.php?action=raw&title=",page)
x<-GET(raw)
r<-content(x,"text")
}
tx<-page.get.content(page)
tx
writeLines(tx,"wikitempread.txt")

# Example usage
#username <- "your_username"
#page_title <- paste0("User:",username,"/sandbox"
page.content<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/data/steltzer-pe5.txt")
#content <- paste(entries, collapse = "\n")
content<-page.content
content
#password <- "your_password"
page_title<-"Seite:Steltzer_montenegro.pdf/5"
pagedir<-"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/pages"
f<-list.files(pagedir)
fns<-paste(pagedir,f,sep = "/")
template<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/data/wikitemplate_proof-steltzer.html")
# template<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/data/wikitemplate_proof_basic-steltzer.html")
template
#?insert
page<-fns[5]
page.edit<-function(page,template){
  t<-readLines(page)
  m<-grepl("<temptext/>",template)
  template<-append(template,t,after = which(m))
  template
  template<-template[!m]
  template
  template<-paste(template,collapse = "<br>")
  p.nr<-strsplit(page,"\\.")[[1]][2]
  template<-gsub("<temppagenr/>",p.nr,template)
 # cat(template)
  wiki.ns<-paste0("Seite:Steltzer_montenegro.pdf/",p.nr)
  return(list(content=template,ns=wiki.ns))
}
k<-5
# fns[6]
#p.nr<-strsplit(fns[5],"\\.")[[1]][2]
#fns.o<-fns[order(fns,method = "auto")]
#fns.o
template
for(k in 1:length(fns)){
seite<-fns[k]
page.x<-page.edit(seite,template)
page.x$ns
page.x$content
x<-update_wikisource_page(page.x$ns, page.x$content, username, password)
cat("processed page:",k,"\n")
#x<-update_wikisource_page(page_title, content, username, password)
}
t<-content(x,"text")




