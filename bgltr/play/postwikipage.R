# 20250112(13.05)
# 15032.bgltr.play.postwikipage
################################
library(httr)

cred<-read.csv("~/boxHKW/21S/DH/local/R/cred_gener.csv")
m<-grep("dhiwswiki",cred$q)
username<-cred$bn[m]
password<-cred$pwd[m]
mwiki<-cred$url[m]

api_url<-mwiki
mwiki

api_url <- paste0(mwiki,"api.php")
#username <- "your-username"
#password <- "your-password"

login_response <- POST(api_url, body = list(
  action = "login",
  lgname = username,
  lgpassword = password,
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

post.page<-function(page.x){
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
post.file<-function(pdf_path,file_name){
# upload_response<-POST(api_url,body=list(
# action="upload",
# token = csrf_token,
# format = "multipart/form-data",
# file=pdf_path,
# description="no desc")
# )
# return(upload_response)
# }
file_path <- pdf_path
print(file.exists(pdf_path))
#file_name <- "file.pdf"

upload_response <- POST(api_url, body = list(
  action = "upload",
  filename = file_name,
  token = csrf_token,
  format = "json",
  file = upload_file(file_path)
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
page.x<-list()
page.ns<-"Seite:testpost"
#page.x$ns<-paste0(page.ns,".pdf")
page.x$ns<-paste0(page.ns,".001")
page.x$content<-readLines(paste0("~/Documents/GitHub/ETCRA5_dd23/bgltr/play/",page.ns,".txt"))
page.x$content<-paste0(page.x$content,collapse = "\n")
page.x
page_title<-page.x$ns
content<-page.x$content
#x<-post.page(page.x)
?file
pdf_path = "/Users/guhl/boxHKW/21S/DH/local/EXC2020/bgltr/ocr/steltzer_franziska_montenegro_ggl.pdf"
pdf_path
x<-post.file(pdf_path,"testfile.pdf")
#page.x
#x<-update.page(mwiki,page.x)
#url
#x<-update_wikisource_page(mwiki,page.x$ns, page.x$content, username, password,test = F)
x
