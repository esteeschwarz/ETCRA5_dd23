# 20250118(07.14)
# 15041.bgltr.WS.TEI
# TEI refactoring of wikisource transcription
#############################################
source("/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/functions.R")
######
# t1<-page.get.content("Franziska_Montenegro","json")
# head(t1,200)
# epub<-tempfile("t1.epub")
# download.file("https://ws-export.wmcloud.org/?format=epub&lang=de&page=Franziska_Montenegro",epub)
ep<-GET("https://ws-export.wmcloud.org/?format=epub&lang=de&page=Franziska_Montenegro")
t1<-content(ep,"text")
htm<-read_html(t1)
#xml_text(htm)
# no.
#############
get.all.elements<-function(){
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
return(all.elements)
}
# wks.
#all.b<-xml_find_all(all.elements,"//b")
#xml_text(all.b[1:20])
all.elements<-get.all.elements()
# x<-all.elements
# i<-41
# xi<-x[[552]]
# xi
# sub<-xi
# xml_text(xi)
assign.sp<-function(x,i){
  sub<-x
  sub
#  p<-xml_find_all(sub,"p")
  xml_name(xi)
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
    sp.ezd<-paste0("$",p.tx)
    sp.ezd<-gsub("\n"," ",sp.ezd)
#    sp.ezd<-gsub(":[ ]{1,2}\t",":\t",sp.ezd)
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
  b<-xml_find_all(sub,"i")
  if(length(b)>0){
    print(length(b))
    p.tx<-xml_text(sub)
    b.tx<-xml_text(b)
    print(p.tx)
    print(b.tx)
#    p.tx<-gsub(b.tx,"",p.tx)
 #   b.tx<-gsub("\\.",":",b.tx)
    sp.ezd<-paste0("$",p.tx)
    sp.ezd<-gsub("\n"," ",sp.ezd)
    #sp.ezd<-gsub(":[ ]{1,2}\t",":\t",sp.ezd)
    sp.ezd<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',sp.ezd)
    return(sp.ezd)
  }
  # b<-xml_find_all(sub,"b")
  # if(length(b)>0){
  # print(length(b))
  # p.tx<-xml_text(sub)
  # b.tx<-xml_text(b)
  # print(p.tx)
  # print(b.tx)
  # p.tx<-gsub(b.tx,"",p.tx)
  # b.tx<-gsub("\\.",":",b.tx)
  # sp.ezd<-paste0("@",b.tx,p.tx)
  # sp.ezd<-gsub("\n"," ",sp.ezd)
  # sp.ezd<-gsub(":[ ]{1,2}\t",":\t",sp.ezd)
  # sp.ezd<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',sp.ezd)
  # return(sp.ezd)
  # }
#  div<-xml_find_all(sub,"div")
 
}
# ###
# result <- lapply(seq_along(my_list), function(i) {
#   my_function(my_list[[i]], i)
# })
###
sp.lines<-lapply(seq_along(all.elements),function(i){
  assign.sp(all.elements[[i]],i)
})
ezd.lines<-unlist(sp.lines)
#ezd.lines[310:330]
# m<-is.null(ezd.lines[1:length(ezd.lines)])
# sp.lines[[322]]
# s1<-sp.lines[[322]]
# s1
m<-ezd.lines=="$"|ezd.lines=="@ *"|ezd.lines==""|
  ezd.lines=="[ ]{1,10}"|ezd.lines=="[.]{1,2}"|ezd.lines=="\n"
sum(m)
ezd.lines<-gsub("\n"," ",ezd.lines)
ezd.lines<-ezd.lines[!m]
#ezd.lines[632]
### wks.
m1<-grepl("^<pb",ezd.lines)
sum(m1)
ezd.lines[m1]
m2<-grep("^<pb",ezd.lines)
m3<-m2-1
ezd.lines[m3]<-paste0(ezd.lines[m3],ezd.lines[m2])
ezd.lines<-ezd.lines[!m1]
ezd.nl<-gsub("  "," ",ezd.lines)
ezd.nl<-gsub("\t","\n",ezd.nl)
#ezd.nl<-gsub("  "," ",ezd.nl)
for (k in 1:3){
ezd.nl<-gsub("(^@.*:)[ ]{1,2}","\\1\n",ezd.nl)
}
### normalise speaker ids
m<-grep("^@.*[:.]",ezd.nl)
strsplit()
sp.u<-unique(strsplit(ezd.nl[m],"\n"))
sp.u[1]

sp.u.df<-abind(lapply(sp.u,function(x){x[1]}),along = 0)
sp.un<-unique(sp.u.df)
sp.un[,2]<-""
#sp.cor<-fix(sp.un)
# library(readr)
# write.table(sp.cor,"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/speaker-ids.csv",row.names = F)
sp.cor<-read.csv("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/speaker-ids.csv",sep = " ")
replace.sp.ids<-function(x){
  if(!is.na(x[,2])){
  m<-grep(x[,1],ezd.nl)
  return(gsub(x[,1],x[,2],ezd.nl[m]))
  }
}
ezd.nl.sp<-ezd.nl
k<-1
for (k in 1:length(sp.cor$speaker)){
  if(sp.cor$corrected[k]!=""){
    m<-grep(sp.cor$speaker[k],ezd.nl.sp)
    ezd.nl.sp[m]<-gsub(sp.cor$speaker[k],sp.cor$corrected[k],ezd.nl.sp[m])
  }
}
#wks.
### personal:
m<-grep("II",ezd.nl.sp)
ezd.nl.sp[m[1]+1]
p.head<-strsplit(ezd.nl.sp[m[1]+1],"\\.")
p.head<-gsub("^ ","",unlist(p.head))
p.head<-p.head[p.head!=""]
p.head<-strsplit(p.head,",")
p.head<-abind(lapply(p.head,function(x){x[1:4]}),along = 0)
p.person<-p.head[,1]
?apply
x<-p.head[1,]
paste0(x,collapse = ",")
p.head[is.na(p.head)]<-""
p.desc<-apply(p.head[,2:length(p.head[1,])],c(1),FUN =function(x)paste0(x,collapse = ","))
p.desc<-gsub("([,]{1,3}$)","",p.desc)
p.desc<-gsub("^ ","",p.desc)
p.desc
#p.desc<-p.desc[p.desc!=""]
p.desc.m<-paste0(p.head[,1],"<roleDesc>",p.desc,"</roleDesc>")
p.desc.m
ezd.nl.sp[m[1]]<-"^"
m.out<-c(m[1],(m[2]+1):length(ezd.nl.sp.head))
ezd.nl.sp.head<-ezd.nl.sp[m.out]
ezd.nl.sp.head<-append(ezd.nl.sp.head,p.desc.m,m[1])

# writeLines(unlist(ezd.lines),"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/ezd/steltzer_ezd.001.txt")
ezd_markup.ns<-"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/ezd/steltzer_ezd.001"
ezd_markup_text<-paste0(ezd_markup.ns,".txt")
writeLines(ezd.nl.sp.head,ezd_markup_text)
local<-T
path.local.home<-"~/Documents/GitHub/dybbuk-cor"
process.ezd<-function(){
  #check.local()
  # ifelse(local,path.local.home,path.git.home)
#  ezd_markup_text<-check.src("txt")
  system(paste0("python3 ",ifelse(local,path.local.home,path.git.home),"/convert/actuel/","parser.git.py ",ezd_markup_text))
  print("finished python ezd")
} #end ezd process .txt
process.ezd()
xml.ns<-paste0(ezd_markup.ns,".xml")
xml<-read_xml(xml.ns)
library(purrr)
xml<-xml%>%xml_ns_strip()
castlist<-xml_find_all(xml,"//castList")
castlist.r<-gsub("&lt;","<",castlist)
castlist.r<-gsub("&gt;","/>",castlist.r)
castlist.r<-gsub("<castList>|</castList>","",castlist.r)
casthtm<-read_html(castlist.r)
casthtm<-xml_find_all(casthtm,"//body")
castnode<-xml_new_root("castList")
k<-1
for(k in 1:length(p.desc)){
  xml_add_child(castnode,"castItem",p.head[k,1])
  if(p.desc[k]!=""){
    xml_add_child(xml_find_all(castnode,"//castItem")[[k]],"roleDesc",p.desc[k])
  }
}
castnode
#xml_add_child(castnode)
#?xml_new_root
#?xml_replace
#xml_add_child(xml_find_all(xml,"//front"),"castList")
xml_replace(xml_find_all(xml,"//castList"),castnode)#,xml_find_all(xml,"//castList"))
#xml_find_all(xml,"//castList")[[2]]<-casthtm
write_xml(xml,xml.ns)
#URLencode("<")


