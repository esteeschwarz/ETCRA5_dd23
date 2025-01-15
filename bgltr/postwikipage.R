# 20250112(13.05)
# 15032.bgltr.play.postwikipage
################################
library(httr)

cred<-read.csv("~/boxHKW/21S/DH/local/R/cred_gener.csv")

get_credit<-function(admin=F){
m<-grep("dhiwswiki",cred$q)
mu<-grep("dhi2wswiki-st",cred$q)
ma<-grep("dhi2wswiki-admin",cred$q)

ifelse(admin==F,username<-cred$bn[mu],username<-cred$bn[ma])
ifelse(admin==T,password<-cred$pwd[ma],password<-cred$pwd[ma])
#password<-cred$pwd[m]
mwiki<-cred$url[ma]

api_url<-mwiki
mwiki

api_url <- paste0(mwiki,"api.php")
return(list(url=api_url,username=username,password=password))
}
#username <- "your-username"
#password <- "your-password"

wiki_login<-function(admin,api_url){

credits<-get_credit(admin)
#credits
api_url<-credits$url
login_response <- POST(api_url, body = list(
  action = "login",
  lgname = credits$username,
  lgpassword = credits$password,
  format = "json"
))

login_token <- content(login_response)$login$token

login_confirm_response <- POST(api_url, body = list(
  action = "login",
  lgname = username,
  lgpassword = password,
  lgtoken = login_token,
  format = "json"
))

token_response <- GET(api_url, query = list(
  action = "query",
  meta = "tokens",
  type = "csrf",
  format = "json"
))

csrf_token <- content(token_response)$query$tokens$csrftoken
}

post.page<-function(page.x){
api_url<-get_credit(F)$url
    csrf_token<-wiki_login(admin = F)
  edit_response <- POST(api_url, body = list(
  action = "edit",
  title = page.x$ns,
  text = page.x$content,
  #title = "Testpage",
  
  
  #text = "[[new page]] to testing API, time: 25/01/14-10:43",
  token = csrf_token,
  format = "json"
))

edit_result <- content(edit_response)
print(edit_result)
return(edit_result)
}
#admin=T
del.page<-function(admin,page_name){
credits<-get_credit(admin)
credits
api_url<-credits$url
csrf_token<-wiki_login(admin,api_url)
    edit_response <- POST(api_url, body = list(
    action = "delete",
    title = page_name,
#    pageid = page_id,
 #   text = page.x$content,
    #title = "Testpage",
    
    
    #text = "[[new page]] to testing API, time: 25/01/14-10:43",
    token = csrf_token,
    format = "json"
  ))
  
  edit_result <- content(edit_response)
  print(edit_result)
  return(edit_result)
}
post.file<-function(pdf_path,file_name,pdf_desc){
  api_url<-get_credit(F)$url
  csrf_token<-wiki_login(admin = F)
file_path <- pdf_path
print(file.exists(pdf_path))
upload_response <- POST(api_url, body = list(
  action = "upload",
  filename = file_name,
  token = csrf_token,
  format = "json",
  file = upload_file(file_path),
  text = pdf_desc
), encode = "multipart", multipart = TRUE, 
add_headers('Content-Type' = "multipart/form-data")
)

upload_result <- content(upload_response)
print(upload_result)
return(upload_result)
}
# x<-GET(raw)
# r<-content(x,"text")
# r
### POST page ########
page.x<-list()
page.ns<-c("Index:","Steltzer_franziska_montenegro")
page.ns<-c("MediaWiki:","Proofreadpage_index_template","",".txt")
#page.ns<-c("MediaWiki:","Proofreadpage_index_data_config",".json",".json")
#page.x$ns<-paste0(paste0(page.ns,collapse = ""),".pdf",collapse = "")
page.x$ns<-paste0(page.ns[1:3],collapse = "")
page.x$ns
#page.x$ns<-paste0(page.ns,".001")
page.x$content<-readLines(paste0("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/wiki/dhiws/",page.ns[2],page.ns[4]))
page.x$content<-paste0(page.x$content,collapse = "\n")
page.x
#page_title<-page.x$ns
#content<-page.x$content
####################
x<-post.page(page.x)
x
####################
#?file
### POST pdf #######
pdf_path = "/Users/guhl/boxHKW/21S/DH/local/EXC2020/bgltr/ocr/steltzer_franziska_montenegro_ggl.pdf"
pdf_desc<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/wiki/dhiws/steltzer_desc.txt")
pdf_desc<-paste0(pdf_desc,collapse = "\n")
pdf_desc
pdf_path
####################################################################
#x2<-post.file(pdf_path,"Steltzer_franziska_montenegro.pdf",pdf_desc)
#page.x
####################################################################
#x<-update.page(mwiki,page.x)
#url
#x<-update_wikisource_page(mwiki,page.x$ns, page.x$content, username, password,test = F)
#library(xml2)
#xml_text(x2)
#delete<-del.page(admin=T,page_name = "Index:.pdf")
