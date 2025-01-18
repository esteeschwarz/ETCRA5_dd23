# 20250118(07.14)
# 15041.bgltr.WS.TEI
# TEI refactoring of wikisource transcription
#############################################
source("/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/functions.R")
######
t1<-page.get.content("Franziska_Montenegro","json")
head(t1,200)
epub<-tempfile("t1.epub")
download.file("https://ws-export.wmcloud.org/?format=epub&lang=de&page=Franziska_Montenegro",epub)
ep<-GET("https://ws-export.wmcloud.org/?format=epub&lang=de&page=Franziska_Montenegro")
t1<-content(ep,"text")
htm<-read_html(t1)
xml_text(htm)
# no.
#############
r2<-GET("https://de.wikisource.org/wiki/Franziska_Montenegro")
t1<-content(r2,"text")
htm<-read_html(t1)
xml_text(htm)
tcontent<-xml_find_all(htm,'//*[@id="mw-content-text"]')
write_html(tcontent,"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/steltzer_wiki-html_001.html")
all.div<-xml_find_all(tcontent,"//div")
xml_text(all.div[1:20])
t1<-xml_find_all(all.div,'//*[@id="mw-content-text"]/div[1]/div[2]')
all.h<-xml_find_all(t1,"//h4")
xml_text(all.h)
# no.
t2<-unlist(t1[[1]])
length(xml_children(t1[[1]]))
all.elements<-xml_children(t1[[1]])
all.h<-xml_attr(all.elements,"style")
m1<-grep("170%",all.h)
m2<-grep("130%",all.h)
xml_text(all.elements[m1])
xml_text(all.elements[m2])
# wks.
#all.b<-xml_find_all(all.elements,"//b")
#xml_text(all.b[1:20])
x<-all.elements
i<-41
xi<-x[[90]]
xi
sub<-xi
xml_text(xi)
assign.sp<-function(x,i){
  sub<-x
  sub
#  p<-xml_find_all(sub,"p")
  xml_name(xi)
  pb<-xml_attr(sub,"class")=="PageNumber"
    if(!is.na(pb)){
    pb<-xml_text(sub)
    pb<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',xml_text(sub))
    return(pb)

    }
  #b<-xml_find_all(sub,"i") # find kursive stage directions
  if(xml_name(sub)=="i"){
    #print(length(b))
    p.tx<-xml_text(sub)
    #b.tx<-xml_text(b)
    print(p.tx)
    #print(b.tx)
#    p.tx<-gsub(b.tx,"",p.tx)
 #   b.tx<-gsub("\\.",":",b.tx)
    sp.ezd<-paste0("$ ",p.tx)
    sp.ezd<-gsub("\n"," ",sp.ezd)
#    sp.ezd<-gsub(":[ ]{1,2}\t",":\t",sp.ezd)
    sp.ezd<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',sp.ezd)
    return(sp.ezd)
  }
  b<-xml_find_all(sub,"i")
  if(length(b)>0){
    print(length(b))
    p.tx<-xml_text(sub)
    b.tx<-xml_text(b)
    print(p.tx)
    print(b.tx)
#    p.tx<-gsub(b.tx,"",p.tx)
 #   b.tx<-gsub("\\.",":",b.tx)
    sp.ezd<-paste0("$ ",p.tx)
    sp.ezd<-gsub("\n"," ",sp.ezd)
    #sp.ezd<-gsub(":[ ]{1,2}\t",":\t",sp.ezd)
    sp.ezd<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',sp.ezd)
    return(sp.ezd)
  }
  b<-xml_find_all(sub,"b")
  if(length(b)>0){
  print(length(b))
  p.tx<-xml_text(sub)
  b.tx<-xml_text(b)
  print(p.tx)
  print(b.tx)
  p.tx<-gsub(b.tx,"",p.tx)
  b.tx<-gsub("\\.",":",b.tx)
  sp.ezd<-paste0("@",b.tx,p.tx)
  sp.ezd<-gsub("\n"," ",sp.ezd)
  sp.ezd<-gsub(":[ ]{1,2}\t",":\t",sp.ezd)
  sp.ezd<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',sp.ezd)
  return(sp.ezd)
  }
  div<-xml_find_all(sub,"div")
  m1<-grepl("170%",xml_attr(sub,"style"))
  if(m1){
    h2.tx<-paste0("#",xml_text(sub))
    return(h2.tx)
  }  
  m2<-grepl("130%",xml_attr(sub,"style"))
  if(m2){
    h3.tx<-paste0("##",xml_text(sub))
    return(h3.tx)
  }
}
# ###
# result <- lapply(seq_along(my_list), function(i) {
#   my_function(my_list[[i]], i)
# })
###
sp.lines<-lapply(seq_along(all.elements),function(i){
  assign.sp(all.elements[[i]],i)
})
head(sp.lines,200)
xml_text(x[[88]])
sp.lines[[88]]
head(xml_text(x),30)
x[[79]]
m<-is.null(unlist(sp.lines))
