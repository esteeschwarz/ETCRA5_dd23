# 20250108(06.49)
# 15025.ETCRA5.bgltr.steltzer-montenegro
# upload text to wikisource, edit transcription
###############################################
source("/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/functions.R")
text_path = "/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/steltzer(1781)_franziska-montenegro.txt"
pagedir<-"/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/pages"
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
  template<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/data/wikitemplate_proof_basic-steltzer.html")
  template
  k<-5
  for(k in 1:length(fns)){
    page<-fns[k]
    page.x<-page.edit(page,template)
    page.x$ns
    page.x$content
    update_wikisource_page(page.x$ns, page.x$content, username, password)
    #x
    cat("processed page:",k,"\n")
    #x<-update_wikisource_page(page_title, content, username, password)
  }
  