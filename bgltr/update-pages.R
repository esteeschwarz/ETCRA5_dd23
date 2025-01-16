# 20250108(06.49)
# 15025.ETCRA5.bgltr.steltzer-montenegro
# upload text to wikisource, edit transcription
###############################################
cred<-read.csv("~/boxHKW/21S/DH/local/R/cred_gener.csv")
m<-grep("wikisource",cred$q)
username<-cred$bn[m]
password<-cred$pwd[m]
source("/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/functions.R")
text_path = "/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/steltzer(1781)_franziska-montenegro.mod.txt"
text_path = "/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/steltzer(1781)_franziska-montenegro.mod.2.txt"
pagedir<-"/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/pagesmod"
dir.create(pagedir)
t<-readLines(text_path)
t2<-gsub("\u017F","s",t)
m<-grep("steltzer_montenegro(.*?).pdf_0000",t2) # 95
m2<-c(m[2:length(m)],length(t2))
m2
template<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/data/wikitemplate_proof-steltzer.html")
# template<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/data/wikitemplate_proof_basic-steltzer.html")

get.page<-function(x,m,m2,i){
  m2<-m2-1
  page<-x[m[i]:m2[i]]
  page.ns<-paste0(pagedir,"/steltzer-p.",i,".txt")
  writeLines(page,page.ns)
}
write.pages.from.file<-function(){
  t.pages<-lapply(seq_along(1:length(m)),function(i){
    get.page(t2,m,m2,i)
    
  }
  )
}
f<-list.files(pagedir)
fns<-paste(pagedir,f,sep = "/")
library(abind)
p.nr.l<-strsplit(fns,"\\.")
p.nr.l<-data.frame(abind(p.nr.l,along=0))
#p.nr.l<-p.nr.l[order(p.nr.l$X2),]
fns.o<-fns[order(as.double(p.nr.l$X2))]
fns.o
fns<-fns.o
fns[15]
#######################
write.pages.from.file()
#######################
get.page.ns<-function(which.dir){
  if(which.dir=="ocr"){
    f<-list.files(pagedir)
    fns<-paste(pagedir,f,sep = "/")
#    template
    k<-6
    fns
    library(abind)
    p.nr.l<-strsplit(fns,"\\.")
    p.nr.l<-data.frame(abind(p.nr.l,along=0))
    #p.nr.l<-p.nr.l[order(p.nr.l$X2),]
    fns.o<-fns[order(as.double(p.nr.l$X2))]
    fns.o
    fns<-fns.o
    fns[11]
    
  }
  return(fns)
}
fns<-get.page.ns("ocr")
  k<-18
  library(xml2)
  logfile<-"~/Documents/GitHub/ETCRA5_dd23/bgltr/data/post.log"
  logfile<-"/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/post.log"
  #file.create(logfile)
  postlist<-list()
  # next 20, break due rate limit
  # run edit >
#  for(k in 16:length(fns)){
  ####################################
  k<-19
  range<-19:19
  inuse=T
  returnformat<-"json"
  credit<-c(admin=F,ws=T)
 #get_credit(credit)
  # test=F
  do.post.wiki<-function(range,inuse,credit){
 # range<-9:9
    for(k in range){
  ifelse(40<=k|k<=15,wait<-15,wait<-k)
    tx.ql<-NA
    tx.user<-NA
    cat("\nprocess page:",k,"\n")
    page.get<-paste0("Seite:Steltzer_montenegro.pdf/",k)
    #page<-page_title
    tx<-page.get.content(page.get,"raw")
    tx
    tx.temp<-tempfile("tx.html")
    writeLines(tx,tx.temp)
    tx.html<-read_html(tx.temp)
    head<-xml_find_all(tx.html,"//pagequality")
    tx.user<-xml_attr(head[1],"user")
    tx.ql<-xml_attr(head[1],"level")
    page<-fns[k]
    page
    page.x<-page.edit(page,template,inuse,k)
    page.x$ns
    print(page.x)
    
    message<-paste0("page: ",k," edited by another user, not uploading...\n")
    #ws<-"https://de.wikisource.org/w/"
    if(tx.ql==1&tx.user=="Guhlglaser"){
      Sys.sleep(wait) # wiki spits us out, maybe even higher though
      # x<-update_wikisource_page(ws,page.x$ns, page.x$content, username, password,test)
      x<-update_wikisource_page(page.x,inuse,credit)
      message<-paste0("\nedited page: ",k,"\n")
     #x
     log<-c(k,as.character(unlist(x)))
     log<-t(log)
     cat(log)
     write.table(log,logfile,append = T,col.names = F)
     postlist[[k]]<-x
     
    }
    cat(message)

    #x<-update_wikisource_page(page_title, content, username, password)
    }
  }
  tx<-page.get.content(page.get,"raw")
  tx
#########################################
### TEST:
  repl.df<-get.regex()
  k<-20
  page.edit(fns[k],template,inuse = F,i=k)
  do.post.wiki(c(20:20),inuse=F,credit)
#########################################  
  do.post.wiki(c(21:length(fns)),inuse=F,credit)
#########################################
    utf8ToInt("ſ")
  utf8ToInt("ß")
  
  sz<-c("wuͤßt","koͤmm"," nöͤthi")
  page.edit(fns[9],template,repl.df)
  #repl.df[4,]<-c("(^@)","<!--\\1-->")
 #is.na(repl.df$regx) 
#  repl.df[6,]<-c("([@$^~])","<!--\\1-->")
  #repl.df[7,]<-c("([#]{1,5})","<!--\\1-->")
  ### check page
  page_title<-"Index:Steltzer_montenegro.pdf"
  page_title<-"Seite:Steltzer_montenegro.pdf/18"
  page_title<-"Philotas_(Gleim_1767)"
  page_title<-"File:Stelzer_montenegro.pdf"
  #page<-page_title
  tx<-page.get.content(page_title,"json")
  tx
  writeLines(tx,"~/Documents/GitHub/ETCRA5_dd23/bgltr/data/Philotas_(Gleim_1767).txt")
  writeLines(tx,"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/wiki/Index_Steltzer_montenegro_act.txt")
  
### save ws corrected pages to upload after reassign google pdf
### pages 2:17 corrected, 1 is not existing, 2 is frontispiz wt picture, file 3 = 2 on ws
  range<-1:17
  k<-18
  for(k in range){
    page_title<-paste0("Seite:Steltzer_montenegro.pdf/",k)
    tx<-page.get.content(page_title,"json")
    tx
    writeLines(tx,"~/Documents/GitHub/ETCRA5_dd23/bgltr/data/Philotas_(Gleim_1767).txt")
    writeLines(tx,paste0("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/pagesave/Steltzer_montenegro.pdf_",k))
  }
  
  ## edit source ground and write to pages
  # ß issue: replaces if search for repl.df[2,]<-c("([ſ])","s") also all ß
  # ſ is: \u383, ß = \u223
  # Sample UTF-8 character
  char <- "ſ" #\\u017F
  char <- "ß" #\\u00DF
  
  # Convert the character to its Unicode code point
  code_point <- utf8ToInt(char)
  code_point <- lapply(sz,utf8ToInt)
  code_point
  ?utf8ToInt
  lapply(code_point[[1]],intToUtf8)
  code_clean<-c("wüst","kömm","nöthi")
  code_point.cl<-lapply(code_clean,utf8ToInt)
  code_point.cl
  # Format the code point as a \u escape sequence
  uni_m<- sprintf("\\u%04X",code_point[[1]])
  uni_cl <- sprintf("\\u%04X",code_point.cl[[1]])
  uni_m
  uni_cl
    # ß = \u00DF
  print(unicode_escape)
  m<-grep("\u017F",t)
  head(t[m])
  m<-grep("\u00DF",t)
  m<-grep("",t)
  utf8ToInt("a")
  library(stringi)
  m<-stri_extract_all_regex(t,"\u383")
  m
  utf
  gsub("\u017F","s",head(t[64]))

  repl.df
  repl.array<-repl.df[repl.df$category=="orth",]
  repl.array<-repl.array[!is.na(repl.array$regx),]
  t2<-t
  t2<-gsub("\u017F","s",t2)
  for(r in 1:length(repl.array)){
    t2<-gsub(repl.array$regx[r],repl.array$repl[r],t2)
  }
  writeLines(t2,"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/steltzer(1781)_franziska-montenegro.mod.2.txt")
  
  #}

page.x<-list()
page.ns<-"Index:Steltzer_montenegro"
page.x$ns<-paste0(page.ns,".pdf")
page.x$content<-readLines(paste0("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/wiki/",page.ns,".txt"))
page.x$content<-paste0(page.x$content,collapse = "\n")
page.x
page.x
update.page(url,page.x)
