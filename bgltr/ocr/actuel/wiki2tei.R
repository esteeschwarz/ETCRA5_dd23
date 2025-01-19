# 20250118(07.14)
# 15041.bgltr.WS.TEI
# TEI refactoring of wikisource transcription
#############################################
source("/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/functions.R")
library(xml2)
library(abind)
library(purrr)
######
# t1<-page.get.content("Franziska_Montenegro","json")
# head(t1,200)
# epub<-tempfile("t1.epub")
# download.file("https://ws-export.wmcloud.org/?format=epub&lang=de&page=Franziska_Montenegro",epub)
#ep<-GET("https://ws-export.wmcloud.org/?format=epub&lang=de&page=Franziska_Montenegro")
#t1<-content(ep,"text")
#htm<-read_html(t1)
#xml_text(htm)
# no.
#############
get.all.elements<-function(){
# get raw html page of play
r2<-GET("https://de.wikisource.org/wiki/Franziska_Montenegro")
t1<-content(r2,"text")
htm<-read_html(t1)
tcontent<-xml_find_all(htm,'//*[@id="mw-content-text"]')
write_html(tcontent,"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/steltzer_wiki-html_001.html")
all.div<-xml_find_all(tcontent,"//div")
t1<-xml_find_all(all.div,'//*[@id="mw-content-text"]/div[1]/div[2]')
t2<-unlist(t1[[1]])
#length(xml_children(t1[[1]]))
all.elements<-xml_children(t1[[1]])
# all.h<-xml_attr(all.elements,"style")
# # get all #level 1 (aufzug) headers
# m1<-grep("170%",all.h)
# # get all #level 2 (auftritt) headers
# m2<-grep("130%",all.h)
# xml_text(all.elements[m1])
# xml_text(all.elements[m2])
return(all.elements)
}
# wks.
all.elements<-get.all.elements()
assign.sp<-function(x,i){
  sub<-x
  # check passed element and return ezd adapted element
    # # get all #level 1 (aufzug) headers
  m1<-grepl("170%",xml_attr(sub,"style"))
  if(m1){
    h2.tx<-paste0("#",xml_text(sub))
    return(h2.tx)
  }  
  # # get all #level 2 (auftritt) headers
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
# all kursive stage directions
    if(xml_name(sub)=="i"){
    p.tx<-xml_text(sub)
    sp.ezd<-paste0("$",p.tx)
    sp.ezd<-gsub("\n"," ",sp.ezd)
    sp.ezd<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',sp.ezd)
    return(sp.ezd)
  }
# all bold speaker
    b<-xml_find_all(sub,"b")
  if(length(b)>0){
    p.tx<-xml_text(sub)
    b.tx<-xml_text(b)
    p.tx<-gsub(b.tx[1],"",p.tx)
    b.tx<-gsub("\\.",":",b.tx)
    sp.ezd<-paste0("@",b.tx,p.tx)
    sp.ezd<-gsub("\n"," ",sp.ezd)
    sp.ezd<-gsub(":[ ]{1,2}\t",":\t",sp.ezd)
    sp.ezd<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',sp.ezd)
    return(sp.ezd)
  }
# rest of stage directions after kursive speaker names
      b<-xml_find_all(sub,"i")
  if(length(b)>0){
    p.tx<-xml_text(sub)
    b.tx<-xml_text(b)
    sp.ezd<-paste0("$",p.tx)
    sp.ezd<-gsub("\n"," ",sp.ezd)
    sp.ezd<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',sp.ezd)
    return(sp.ezd)
  }
      # returns checked and adapted element
}
###
sp.lines<-lapply(seq_along(all.elements),function(i){
  assign.sp(all.elements[[i]],i)
})
### RUN
# clean up text
ezd.lines<-unlist(sp.lines)
# removes redundant lines
m<-ezd.lines=="$"|ezd.lines=="@ *"|ezd.lines==""|
  ezd.lines=="[ ]{1,10}"|ezd.lines=="[.]{1,2}"|ezd.lines=="\n"
sum(m)
ezd.lines<-gsub("\n"," ",ezd.lines)
ezd.lines<-ezd.lines[!m]
ezd.lines<-gsub("\t","\n",ezd.lines)
for (k in 1:3){
  ezd.lines<-gsub("(^@.+[:])[ \t]{1,2}","\\1\n",ezd.lines)
}
ezd.lines[1:120]
### wks.
# paste single line pagebreaks to preceding line
m1<-grepl("^<pb",ezd.lines)
sum(m1)
m2<-grep("^<pb",ezd.lines)
m3<-m2-1
ezd.lines[m3]<-paste0(ezd.lines[m3],ezd.lines[m2])
ezd.lines<-ezd.lines[!m1]
#ezd.lines[1:100]
ezd.nl<-gsub("  "," ",ezd.lines)
ezd.nl<-gsub("[^:]\n<pb","<pb",ezd.nl)
ezd.nl[1:100]
ezd.nl<-gsub("(:\n<pb.+/>.?)\n","\\1",ezd.nl)
ezd.nl<-gsub("\t","",ezd.nl)
head(ezd.nl,100)
### normalise speaker ids
m<-grep("^@.*[:.]",ezd.nl)
#strsplit()
sp.u<-unique(strsplit(ezd.nl[m],"\n"))
sp.u[1]
sp.u.df<-abind(lapply(sp.u,function(x){x[1]}),along = 0)
# unique speakers
sp.un<-unique(sp.u.df)
# library(readr)
# write.table(sp.cor,"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/speaker-ids.csv",row.names = F)
# read in dataframe with corrected speaker ids 
#########################################################################################
# !!> replaces speakernames in text which deviate from the standard. can be for editorial reasons this is TODO to keep them like in the transcript and normalise in ezd output !!
#########################################################################################
sp.cor<-read.csv("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/speaker-ids.csv",sep = " ")
replace.sp.ids<-function(x){
  if(!is.na(x[,2])){
  m<-grep(x[,1],ezd.nl)
  return(gsub(x[,1],x[,2],ezd.nl[m]))
  }
}
ezd.nl.sp<-ezd.nl
for (k in 1:length(sp.cor$speaker)){
  if(sp.cor$corrected[k]!=""){
    m<-grep(sp.cor$speaker[k],ezd.nl.sp)
    ezd.nl.sp[m]<-gsub(sp.cor$speaker[k],sp.cor$corrected[k],ezd.nl.sp[m])
  }
}
#wks.
### personal:
m<-grep("II",ezd.nl.sp)
# split personal list
p.head<-strsplit(ezd.nl.sp[m[1]+1],"\\.")
p.head<-gsub("^ ","",unlist(p.head))
p.head<-p.head[p.head!=""]
p.head<-strsplit(p.head,",")
p.head<-abind(lapply(p.head,function(x){x[1:4]}),along = 0)
p.person<-p.head[,1]
p.head[is.na(p.head)]<-""
p.desc<-apply(p.head[,2:length(p.head[1,])],c(1),FUN =function(x)paste0(x,collapse = ","))
p.desc<-gsub("([,]{1,3}$)","",p.desc)
p.desc<-gsub("^ ","",p.desc)
# this is redundant, castlist is done in the .xml, but we need p.desc for that
p.desc.m<-paste0(p.head[,1],"<roleDesc>",p.desc,"</roleDesc>")
ezd.nl.sp[m[1]]<-"^"
m.out<-c(m[1],(m[2]+1):length(ezd.nl.sp))
ezd.nl.sp.head<-ezd.nl.sp[m.out]
ezd.nl.sp.head<-append(ezd.nl.sp.head,p.desc.m,m[1])
# writeLines(unlist(ezd.lines),"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/ezd/steltzer_ezd.001.txt")
ezd_markup.ns<-"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/ezd/steltzer_ezd.001"
ezd_markup_text<-paste0(ezd_markup.ns,".txt")
##########################################
# write ezdrama marked up text
writeLines(ezd.nl.sp.head,ezd_markup_text)
##########################################
local<-T
path.local.home<-"~/Documents/GitHub/dybbuk-cor"
process.ezd<-function(){
  system(paste0("python3 ",ifelse(local,path.local.home,path.git.home),"/convert/actuel/","parser.git.py ",ezd_markup_text))
  print("finished python ezd")
} #end ezd process .txt
########################
# perform ezd processing
process.ezd()
########################
xml.ns<-paste0(ezd_markup.ns,".xml")
# read in ezd output .xml
xml.tx<-readLines(xml.ns)
# redo removed (htmldecoded) <pb> tags
m<-grep("&lt|&gt;",xml.tx)
xml.tx[m]<-gsub("&lt;","<",xml.tx[m])
xml.tx[m]<-gsub("&gt;",">",xml.tx[m])
xmltemp<-tempfile("xmltemp.xml")
dracor_head<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/dracor_header.xml")
# insert fixed header
xml.tx<-append(xml.tx,dracor_head[1:4],0)
m<-grepl("<TEI xml:id",xml.tx)
xml.tx<-xml.tx[!m]
head(xml.tx)
writeLines(xml.tx,xmltemp)
xml<-read_xml(xmltemp)
xmlns<-xml_attr(xml,"xmlns")
xmlid<-xml_attr(xml,"id")
xmllang<-xml_attr(xml,"lang")
xml%>%xml_ns_strip()
# create castlist from above p.desc
castnode<-xml_new_root("castList")
k<-1
for(k in 1:length(p.desc)){
  xml_add_child(castnode,"castItem",p.head[k,1])
  if(p.desc[k]!=""){
    xml_add_child(xml_find_all(castnode,"//castItem")[[k]],"roleDesc",p.desc[k])
  }
}
castnode
# replace ezd created castlist with castlist including role descriptions
xml_replace(xml_find_all(xml,"//castList"),castnode)#,xml_find_all(xml,"//castList"))
# include filedesc and standoff from fixed files
filedesc<-read_xml("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/dracor_filedesc.xml")
standoff<-read_xml("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/dracor_standoff.xml")
xml_replace(xml_find_all(xml,"//standOff"),standoff)#,xml_find_all(xml,"//castList"))
xml_replace(xml_find_all(xml,"//fileDesc"),filedesc)#,xml_find_all(xml,"//castList"))
# reset xml attributes to fixed from headerfile (removed stripns to modify xml)
xml_set_attr(xml,"xmlns",xmlns)
xml_set_attr(xml,"xml:id",xmlid)
xml_set_attr(xml,"xml:lang",xmllang)
#########################################
write.final.xml<-function(xml,xml.final){
  write_xml(xml,xml.final)
}
xml.final<-paste0("~/Documents/GitHub/ETCRA5_dd23/tei/","steltzer_montenegro.final.xml")
write.final.xml(xml,xml.final)


