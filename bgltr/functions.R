library(httr)

update_wikisource_page <- function(page_title, content, username, password) {
  login_url <- "https://en.wikisource.org/w/api.php?action=login&format=json"
  login_url <- "https://de.wikisource.org/w/api.php?action=login&format=json"
  login_response <- POST(login_url, body = list(lgname = username, lgpassword = password))
  login_token <- content(login_response)$login$token
  # Get edit token
  edit_token_url <- "https://de.wikisource.org/w/api.php?action=query&meta=tokens&format=json"
  edit_token_response <- GET(edit_token_url, set_cookies(login_response$cookies$value))
  edit_token <- content(edit_token_response)$query$tokens$csrftoken
#  edit_url <- "https://de.wikisource.org/w/api.php?action=parse&format=json"
  edit_url <- "https://de.wikisource.org/w/api.php?action=edit&format=json"
  edit_response <- POST(edit_url, body = list(
    title = page_title,
    #page = page_title,
    text = content,
    token = edit_token
  ), set_cookies(login_response$cookies$value))
  
  return(content(edit_response))
}
#template
#page<-fns[6]
page.edit<-function(page,template,repl.df,inuse=F){
  t<-readLines(page)
  t[1]<-paste0("<!--",t[1],"-->")
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
  template.x<-gsub("<temppagenr/>",p.nr,template.x)
  # for (r in 1:length(repl.df)){
  #   if(!is.na(repl.df$regx[r]))
  #     template.x<-gsub(repl.df$regx[r],repl.df$repl[r],template.x)
  # }
  m3<-grepl("@",template.x)
  sum(m3)
  m3.p<-which(m3)+1
  template.x[m3.p]<-paste0("'''",template.x[m3],"'''\t",template.x[m3.p])
  template.x<-template.x[!m3]
 # template.x<-gsub("([$^~@])","<!--\\1-->",template.x)
  m4<-grepl("#",template.x)
  "{{LineCenterSize|120|20|=== <!--#--> Erster Aufzug.<br><!--##--> ===}}"
  sum(m4)
#  m4.p<-which(m4)+1
  template.x[m4]<-gsub("([#]{2})(.*)",
                       "\n===\\2===\n<br><br>",
                       template.x[m4]) 
  template.x[m4]<-gsub("([#]{1})(.*)",
                        "\n==\\2==\n<br><br>",
                        template.x[m4])
  repl.df<-get.regex()
  
  for (r in 1:length(repl.df$regx)){
    if(!is.na(repl.df$regx[r]))
      template.x<-gsub(repl.df$regx[r],repl.df$repl[r],template.x)
  }
  
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
  if(inuse)
    template.x<-c("{{Vorlage:inuse}}",template.x)
  template.x<-paste(template.x,collapse = "<br>")
  
  template.x<-gsub("#","",template.x)
  
  wiki.ns<-paste0("Seite:Steltzer_montenegro.pdf/",p.nr)
  return(list(content=template.x,ns=wiki.ns))
}
#page.edit(fns[6],template,repl.df)
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
get.regex<-function(){
  repl.df<-data.frame(regx=1:20,repl=NA,category=NA)
  repl.df$regx<-NA
#  repl.df$category<-NA
  #repl.df[1,1]<-"uͤ"
  #repl.df[1,2]<-"ü"
  # repl.df[1,]<-c("[ſ]{2}","ss")
  #  repl.df[6,]<-c("([ſ])([^l])","<!--\\1-->s\\2")
  repl.df[3,]<-c("[@^#$]","","meta")
  #  repl.df[2,]<-c("([ſ])","<!--\\1-->s")
  # repl.df[2,]<-c("([ſ])","s")
  # repl.df[2,]<-c("([\u017F])","s")
  # #repl.df[7,]<-c("([ſ])([^l])","<!--\\1-->s\\2")
  # # repl.df[6,]<-c("([ſ])([^l])","<!--\\1-->s\\2")
  # repl.df[4,1]<-"aͤ"
  # repl.df[4,2]<-"ä"
  # repl.df[5,1]<-"üͤ"
  # repl.df[5,2]<-"ü"
  # repl.df[6,1]<-"üͤ"
  # repl.df[6,2]<-"ü"
  # repl.df$category<-"orth"
  #repl.df$category[3]<-"meta"
  return(repl.df)
}
#get.regex()
