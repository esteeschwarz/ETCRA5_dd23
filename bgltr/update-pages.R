# 20250108(06.49)
# 15025.ETCRA5.bgltr.steltzer-montenegro
# upload text to wikisource, edit transcription
###############################################
source("/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/functions.R")
text_path = "/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/steltzer(1781)_franziska-montenegro.mod.txt"
pagedir<-"/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/pagesmod"
dir.create(pagedir)
t<-readLines(text_path)
m<-grep("steltzer_montenegro.pdf_0000",t) # 95
m2<-c(m[2:length(m)],length(t))
m2
get.page<-function(x,m,m2,i){
  m2<-m2-1
  page<-x[m[i]:m2[i]]
  page.ns<-paste0(pagedir,"/steltzer-p.",i,".txt")
  writeLines(page,page.ns)
}
  t.pages<-lapply(seq_along(1:length(m)),function(i){
    get.page(t,m,m2,i)
    
  }
  )
  f<-list.files(pagedir)
  fns<-paste(pagedir,f,sep = "/")
  template<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/data/wikitemplate_proof-steltzer.html")
  # template<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/data/wikitemplate_proof_basic-steltzer.html")
  template
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
  k<-13
  library(xml2)
  for(k in 8:length(fns)){
    cat("process page:",k,"\n")
    Sys.sleep(10) # wiki spits us out, maybe even higher though
    page<-fns[k]
    page
    page.x<-page.edit(page,template,repl.df)
    page.x$ns
    page.x$content
    page.get<-paste0("Seite:Steltzer_montenegro.pdf/",k)
    #page<-page_title
    tx<-page.get.content(page.get)
    tx
    tx.temp<-tempfile("tx.html")
    writeLines(tx,tx.temp)
    tx.html<-read_html(tx.temp)
    head<-xml_find_all(tx.html,"//pagequality")
    tx.user<-xml_attr(head[1],"user")
    tx.ql<-xml_attr(head[1],"level")
    message<-paste0("page: ",k," edited by another user, not uploading...\n")
    if(tx.ql<=1&tx.user=="Guhlglaser"){
      update_wikisource_page(page.x$ns, page.x$content, username, password)
     message<-paste0("edited page: ",k,"\n")
       
    }
    cat(message)
    
    #x<-update_wikisource_page(page_title, content, username, password)
  }
  tx<-page.get.content(page.get)
  tx
  
    page.edit(fns[6],template,repl.df)
  
  get.regex<-function(){
  repl.df<-data.frame(regx=1:20,repl=NA)
  repl.df$regx<-NA
  #repl.df[1,1]<-"uͤ"
  #repl.df[1,2]<-"ü"
  repl.df[1,]<-c("[ſ]{2}","ss")
#  repl.df[6,]<-c("([ſ])([^l])","<!--\\1-->s\\2")
  repl.df[3,]<-c("[@^#$]","")
#  repl.df[2,]<-c("([ſ])","<!--\\1-->s")
  repl.df[2,]<-c("([ſ])","s")
  #repl.df[7,]<-c("([ſ])([^l])","<!--\\1-->s\\2")
  # repl.df[6,]<-c("([ſ])([^l])","<!--\\1-->s\\2")
   repl.df[4,1]<-"aͤ"
   repl.df[4,2]<-"ä"
   repl.df[5,1]<-"üͤ"
   repl.df[5,2]<-"ü"
   repl.df[6,1]<-"üͤ"
   repl.df[6,2]<-"ü"
   return(repl.df)
  }
  repl.df<-get.regex()
  page.edit(fns[13],template,repl.df)
  #repl.df[4,]<-c("(^@)","<!--\\1-->")
 #is.na(repl.df$regx) 
#  repl.df[6,]<-c("([@$^~])","<!--\\1-->")
  #repl.df[7,]<-c("([#]{1,5})","<!--\\1-->")
  ### check page
  page_title<-"Seite:Steltzer_montenegro.pdf/13"
  #page<-page_title
  tx<-page.get.content(page_title)
  tx
  