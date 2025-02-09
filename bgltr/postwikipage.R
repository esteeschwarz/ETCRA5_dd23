# 20250112(13.05)
# 15032.bgltr.play.postwikipage
################################
library(httr)

cred<-read.csv("~/boxHKW/21S/DH/local/R/cred_gener.csv")
#credit
#################################
get_credit<-function(credit){
m<-grep("dhiwswiki",cred$q)
mu<-grep("dhi2wswiki-st",cred$q)
ma<-grep("dhi2wswiki-admin",cred$q)
mws<-grep("wikisource",cred$q)
miad<-grep("mini12wiki-ad",cred$q)
miguhl<-grep("mini12wiki-guhl",cred$q)

admin<-credit[1]
ws<-credit[2]
mw<-credit[3]

mwiki<-cred$url[ma]
username<-cred$bn[ma]
password<-cred$pwd[ma]

if(admin==T&ws!="yes"&mw==F)
  grp<-ma
if(admin==T&ws!="yes"&mw==T)
  grp<-miad
if(admin==F&ws!="yes"&mw==F)
  grp<-mu
if(admin==F&ws!="yes"&mw==T)
  grp<-miguhl
if(mw==F&ws==5)
  grp<-mws

username<-cred$bn[grp]
password<-cred$pwd[grp]
mwiki<-cred$url[grp]


api_url<-mwiki
mwiki

api_url <- paste0(mwiki,"api.php")
crlist<-list(mwiki=mwiki,url=api_url,username=username,password=password)
print(crlist)
return(crlist)
}
#########
#username <- "your-username"
#password <- "your-password"

wiki_login<-function(credit,api_url){
  credits<-get_credit(credit)
  
# credits<-get_credit(credit=c(admin=T,ws=F))
 credits<-get_credit(credit=c(admin=T,ws=5,mw=F))
credits
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
  lgname = credits$username,
  lgpassword = credits$password,
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

post.page<-function(page.x,model,inuse,credit){
  if(inuse==T)
    page.x$content<-paste0("{{Vorlage:inuse}}",page.x$content)
  page.x$content
api_url<-get_credit(credit)$url
    csrf_token<-wiki_login(credit)
  edit_response <- POST(api_url, body = list(
  action = "edit",
  contentmodel = model,
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
set.content.model<-function(page,credit){
  # Step 4: Set the content model of the page
  page_title = page
  content_model = "sanitized-css"
  api_url<-get_credit(credit)$url
  csrf_token<-wiki_login(credit)
  edit_response <- POST(api_url, body = list(
    action = "changecontentmodel",
    title = page_title,
    model= content_model,
    token= csrf_token,
    format= "json"
  ))
    
    
  
  edit_result <- content(edit_response)
  print(edit_result)
  return(edit_result)
}


#admin=T
del.page<-function(credit,page_name){
credits<-get_credit(credit)
credits
api_url<-credits$url
csrf_token<-wiki_login(credit,api_url)
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
  api_url<-get_credit(credit)$url
  csrf_token<-wiki_login(credit)
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
out.func<-function(){
page.x<-list()
page.ns<-c("Index:","Steltzer_montenegro",".pdf",".txt")
#page.ns<-c("MediaWiki:","Proofreadpage_index_template","",".txt")
#page.ns<-c("MediaWiki:","Proofreadpage_index_data_config",".json",".json")
#page.x$ns<-paste0(paste0(page.ns,collapse = ""),".pdf",collapse = "")
page.x$ns<-paste0(page.ns[1:3],collapse = "")
page.x$ns
#page.x$ns<-paste0(page.ns,".001")
page.x$content<-readLines(paste0("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/wiki/",page.ns[2],page.ns[4]))
page.x$content<-paste0(page.x$content,collapse = "\n")
page.x
x<-post.page(page.x,inuse = T,credit = c(admin=F,ws=T))
x

######################
### HB to dhiws
page.x<-list()
page.x$content<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/play/mw.hbindex.txt")
page.x$content<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/play/de.seitenstatus2.txt")
page.x$content<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/play/dhws.styles.css")
page.x$content<-paste0(page.x$content,collapse = "\n")
page.x$content
page.ns<-c("Index:","Hb09201_wstest",".pdf",".txt")
page.ns<-c("Mediawiki:","Proofreadpage_index_template","",".txt")
page.ns<-c("Vorlage:","Seitenstatus2","",".txt")
page.ns<-c("Index/","styles.css","",".txt")
page.x$ns<-paste0(page.ns[1:3],collapse = "")
page.x$ns
credits<-get_credit(credit = c(F,4,F))
credits
#page_title<-page.x$ns
#content<-page.x$content
####################
x<-post.page(page.x,model="sanitized-css",inuse = F,credit = c(T,4,F))
x

#set css contentmodel
page.ns<-c("Vorlage:Index/","styles.css","",".txt")
page.x$ns<-paste0(page.ns[1:3],collapse = "")
page.x$ns
set.content.model(page.x$ns,c(T,4,F))


}
####################
### reupload pg 2-14/15-17(@17,18), corrected:
get.cor.pages<-function(){
f.save.dir<-"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/pagesave"
f.saves<-list.files(f.save.dir)
f.s.ns<-paste(f.save.dir,f.saves,sep="/")
f.s.ns
library(abind)
fns<-f.s.ns
p.nr.l<-strsplit(fns,"_")
p.nr.l<-data.frame(abind(p.nr.l,along=0))
#p.nr.l<-p.nr.l[order(p.nr.l$X2),]
fns.o<-fns[order(as.double(p.nr.l$X4))]
fns.o
fns.df<-p.nr.l[order(as.double(p.nr.l$X4)),]
fns<-fns.o
fns[11]
k<-14
tx<-list()
range<-14:length(fns)
for (k in range){
  tx$content<-paste0(readLines(fns[k]),collapse="\n")
  
  tx$ns<-paste0("Seite:Steltzer_",fns.df$X3[k],"/",16,collapse = "")
tx
x<-post.page(tx,inuse = T,credit = c(admin=F,ws=T))
x

  }
#fns
push.page<-lapply(seq_along(1:length(fns)),function(i){
  page.edit(fns[i],template,inuse = T,"json",i)
})
push.page[[14]]
tx.15<-page.edit(fns[14],template,repl.df,inuse = T)
tx.15
x<-post.page(push.page[[15]],inuse = T,credit = c(admin=F,ws=T))
x
tx.16<-page.edit(fns[16],template,repl.df,inuse = T)
tx.16
x<-post.page(tx.16,inuse = T,credit = c(admin=F,ws=T))
x
}
#?file
### POST pdf #######
tempfun<-function(){
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
}