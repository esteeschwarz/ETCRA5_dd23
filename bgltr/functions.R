library(httr)

#update_wikisource_page <- function(url,page_title, content, username, password,test) {
  update_wikisource_page <- function(page.x,inuse,credit) {
    # base_url<-"https://en.wikisource.org/w/"
#   base_url<-url
#   login_url <- paste0(base_url,"api.php?action=login&format=json")
#   login_url <- paste0(base_url,"api.php?action=login&format=json")
#   login_response <- POST(login_url, body = list(lgname = username, lgpassword = password))
#   login_token <- content(login_response)$login$token
#   # Get edit token
#   edit_token_url <- paste0(base_url,"api.php?action=query&meta=tokens&format=json")
#   edit_token_response <- GET(edit_token_url, set_cookies(login_response$cookies$value))
#   edit_token <- content(edit_token_response)$query$tokens$csrftoken
# #  edit_url <- "https://de.wikisource.org/w/api.php?action=parse&format=json"
#   edit_url <- paste0(base_url,"api.php?action=edit&assert=user&assertuser=guhlglaser&format=json")
#   edit_url <- paste0(base_url,"api.php?action=edit&format=json")
#   test_url<-paste0(base_url,"api.php?action=query&assert=user&assertuser=guhlglaser&format=json")
    #credits<-get_credit(credit)
    
  # if(test==T){
  #   test_response<-POST(test_url, body = list(
  #     title = page_title,
  #     #page = page_title,
  #     text = content,
  #     # token = edit_token
  #     token = "+\\"
  #   ), set_cookies(login_response$cookies$value))
  #   return(test_response)
  # }
### RUN post:
  x<-post.page(page.x,inuse,credit)
  # chg.name_url<-paste0(base_url,"api.php?action=options&format=json&optionname=nickname&optionvalue=",username)
  # name_response<-POST(chg.name_url,body = list(
  #   token = edit_token),set_cookies(login_response$cookies$value))
  # edit_response <- POST(edit_url, body = list(
  #   title = page_title,
  #   #page = page_title,
  #   text = content,
  #   token = edit_token
  # ), set_cookies(login_response$cookies$value))
  # 
  # return(content(edit_response))
}
#content(edit_response)
#template
#page<-fns[6]
page.edit<-function(page,template,inuse,i){
  t<-readLines(page)
  t[1]<-paste0("<!--",t[1],"-->")
  if(inuse==T)
    t<-c("{{Vorlage:inuse}}",t)
  t
  m<-grepl("<temptext/>",template)
  template.x<-append(template,t,after = which(m))
  template.x
  m2<-c(!m,rep(T,length(template.x)-length(m)))
  m2
  #!m:length(template.x)
  template.x<-template.x[m2]
  
  #template.x<-paste(template.x,collapse = "<br>")
  p.nr<-strsplit(page,"\\.")[[1]][2]
  p.nr<-i
  template.x<-gsub("<temppagenr/>",p.nr,template.x)
  # for (r in 1:length(repl.df)){
  #   if(!is.na(repl.df$regx[r]))
  #     template.x<-gsub(repl.df$regx[r],repl.df$repl[r],template.x)
  # }
  ### SPEAKER:
  m3<-grepl("@",template.x)
  sum(m3)
  m3.p<-which(m3)+1
  template.x[m3.p]<-paste0("\n'''",template.x[m3],"'''\t",template.x[m3.p])
  template.x[m3.p]<-gsub(":",".",template.x[m3.p])
  template.x<-template.x[!m3]
  ### STAGE directions
  m3<-grepl("[$]",template.x)
  sum(m3)
  m3.p<-which(m3)+1
  m3.p<-m3
  template.x[m3.p]<-paste0("\n''",template.x[m3],"''\n")
#  template.x[m3.p]<-gsub(":",".",template.x[m3.p])
  
 # template.x<-gsub("([$^~@])","<!--\\1-->",template.x)
  m4<-grepl("#",template.x)
  "{{LineCenterSize|120|20|=== <!--#--> Erster Aufzug.<br><!--##--> ===}}"
  # h2, aufzug: 170|30, h3, auftritt: 130|30
  # {{LineCenterSize|130|30|Zweyter Auftrit.}}

  sum(m4)
#  m4.p<-which(m4)+1
  template.x[m4]<-gsub("([#]{2})(.*)",
                       "\n{{LineCenterSize|130|30|\\2}}\n",
                       template.x[m4]) 
  template.x[m4]<-gsub("([#]{1})(.*)",
                       "\n{{LineCenterSize|170|30|\\2}}\n",
                       template.x[m4]) 
  # template.x[m4]<-gsub("([#]{1})(.*)",
  #                       "\n==\\2==\n<br><br>",
  #                       template.x[m4])
  # template.x[m4]<-gsub("([#]{2})(.*)",
  #                      "\n===\\2===\n<br><br>",
  #                      template.x[m4]) 
  # template.x[m4]<-gsub("([#]{1})(.*)",
  #                      "\n==\\2==\n<br><br>",
  #                      template.x[m4])
  repl.df<-get.regex()
  
  # for (r in 1:length(repl.df$regx)){
  #   if(!is.na(repl.df$regx[r])&repl.df$category[r]!="post")
  #     template.x<-gsub(repl.df$regx[r],repl.df$repl[r],template.x)
  # }
  
  #   template.x[m4]<-gsub("([#]{2})(.*)",
#                      "{{LineCenterSize|120|20|===\\1\\2===}}",
#                      template.x[m4]) 
# template.x[!m4]<-gsub("([#]{1})(.*)",
#                      "{{LineCenterSize|120|20|==\\1\\2==}}",
#                      template.x[!m4])

#  template.x<-template.x[!m4]
 # m4<-grepl("##",template.x)
  "{{LineCenterSize|120|20|=== <!--#--> Erster Aufzug.<br><!--##--> ===}}"
  #sum(m4)
  #m4.p<-which(m4)+1
  #template.x[m4.p]<-paste0("{{LineCenterSize|120|20|==",template.x[m4],"==}}")
  #template.x<-template.x[!m4]
  # template.x<-gsub("([#]{1,5})","<!--\\1-->",template.x)
  #template.x<-paste(template.x,collapse = "<br>")
  template.x<-paste(template.x,collapse = "\n")
#  postrepl<-repl.df[repl.df$category=="post",]
 # postrepl<-postrepl[!is.na(postrepl$regx),]
#   for (p in 1:length(postrepl$regx)){
# #  template.x<-gsub("#","",template.x)
#  # template.x<-gsub(repl.df,"",template.x)
#     template.x<-gsub(postrepl[p,1],postrepl[p,2],template.x)
#   }
  for (r in 1:length(repl.df$regx)){
    if(!is.na(repl.df$regx[r]))
      template.x<-gsub(repl.df$regx[r],repl.df$repl[r],template.x)
  }
  
  wiki.ns<-paste0("Seite:Steltzer_montenegro.pdf/",p.nr)
  return(list(content=template.x,ns=wiki.ns))
}
#page.edit(fns[6],template,repl.df)
#page<-url_escape(page)
page.get.content<-function(page,format){
  # #page<-url_escape(page)
  # read_url<-paste0("https://de.wikisource.org/w/api.php?action=query&format=json&prop=revisions&titles=",page,"&formatversion=2&rvprop=content&rvslots=*")
  # read_url<-paste0("https://de.wikisource.org/w/api.php?action=query&prop=extracts&titles=",page)
  # "https://de.wikisource.org/w/api.php?action=query&prop=extracts&exchars=175&titles=Seite:Steltzer_montenegro.pdf/5"
  page
  format<-"json"
  raw<-paste0("https://de.wikisource.org/w/index.php?action=raw&format=",format,"&title=",page)
  x<-GET(raw)
  r<-content(x,"text")
}
get.regex<-function(){
  repl.df<-data.frame(regx=1:20,repl=NA,category=NA)
  repl.df$regx<-NA
#  repl.df$category<-NA
  repl.df[1,1]<-"uͤ"
  repl.df[1,2]<-"ü"
  # repl.df[1,]<-c("[ſ]{2}","ss")
  #  repl.df[6,]<-c("([ſ])([^l])","<!--\\1-->s\\2")
  repl.df[3,]<-c("[@^#$]","","post")
  repl.df[4,]<-c("⸗\n","","post")
  repl.df[5,]<-c("Franzista","Franziska","ocr")
  repl.df[6,]<-c("Geschäste","Geschäfte","ocr")
  
  #  repl.df[2,]<-c("([ſ])","<!--\\1-->s")
   repl.df[7,]<-c("([ſ])","s","ocr")
  # repl.df[2,]<-c("([\u017F])","s")
  # #repl.df[7,]<-c("([ſ])([^l])","<!--\\1-->s\\2")
  # # repl.df[6,]<-c("([ſ])([^l])","<!--\\1-->s\\2")
   repl.df[8,1]<-"aͤ"
   repl.df[8,2]<-"ä"
   repl.df[9,1]<-"üͤ"
   repl.df[9,2]<-"ü"
   repl.df[10,1]<-"üͤ"
   repl.df[10,2]<-"ü"
   repl.df[11,1]<-"oͤ"
   repl.df[11,2]<-"ö"
  # repl.df$category<-"orth"
  #repl.df$category[3]<-"meta"
  return(repl.df)
}
#get.regex()
update.page<-function(url,page.x){
  x<-update_wikisource_page(url,page.x$ns, page.x$content, username, password)
  print(x)
}

###############################
# make.TEI
page.f<-"Seite:Steltzer_montenegro.pdf/11"

get.htm<-function(page.f){
html<-page.get.content(page.f,"raw")
html
temphtm<-tempfile("xtemp.html")
writeLines(html,"xtemp.html")
#writeLines(html,temphtm)

p.htm<-read_html(html,encoding = "UTF-8")
p.htm<-unlist(strsplit(html,"\n"))
noinc<-"<noinclude>.+</noinclude>"
m<-grep(noinc,p.htm)
p.htm[m]<-gsub(noinc,"",p.htm[m])
return(p.htm)
#}
#?read_xml.character
#p.htm
p.tx<-get.htm(page.f)
#p.htm
#p.tx<-xml_text(p.htm)
p.tx
rsp<-"'''(.+)'''(.+)"
m1<-grep(rsp,p.tx)
p.tx[m1]<-gsub(rsp,"\n@\\1\\2",p.tx[m1])
p.tx
rsp<-"''(.+)''(.+)"
m2<-grep(rsp,p.tx)
p.tx[m2]<-gsub(rsp,"\n$\\1\\2",p.tx[m2])
p.tx
p.tx<-p.tx[p.tx!=""]
p.tx.c<-paste0(p.tx,collapse = " ")
p.tx.c
}

