# 20250118(07.14)
# 15041.bgltr.WS.TEI
# TEI refactoring of wikisource transcription
#############################################
source("/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/functions.R")
library(xml2)
library(abind)
library(purrr)
library(readr)
library(stringi)
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
log.ns<-"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/log.csv"
log.x<-function(what){
  write.table(what,log.ns,append = T,col.names = F,quote = F)
}
file.create(log.ns)
#########################
#r<-49
x<-all.elements[[49]]
x
assign.sp<-function(x,r){
  log.tx<-matrix("",ncol = 10)
  sub<-x
  dif.tx<-0
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
  #######################################
  ### TODO: increase +1
  pb<-xml_attr(sub,"class")=="PageNumber"
    if(!is.na(pb)){
    pb<-xml_text(sub)
    pb<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',xml_text(sub))
    return(pb)
    }
  #######################################
  # get speaker and p wo/ declaration
  # pattern <p><b><i> : p,speaker,stage
  # get p
  p.tx<-xml_text(sub)
  p.tx<-gsub("\n"," ",p.tx)
  if(xml_name(sub)=="p"){
    # chk if speaker
    p.ur.tx<-xml_text(sub)
    p.tx<-p.ur.tx
    l.tx<-length(unlist(strsplit(p.tx," ")))
    b<-xml_find_all(sub,"b")
    i<-xml_find_all(sub,"i")
    l.tb<-length(unlist(strsplit(xml_text(b)," ")))
    l.ti<-length(unlist(strsplit(xml_text(i)," ")))
    mkl<-stri_extract_all_regex(p.tx,"\\(",simplify = T)
    mkr<-stri_extract_all_regex(p.tx,"\\)",simplify = T)
    mkl<-mkl[!is.na(mkl)]
    mkr<-mkr[!is.na(mkr)]
    write.table(c("mkrl,letx",mkl,mkr,mkl==mkr,l.tx,l.ti,l.tb),log.ns,append = T,col.names = F,quote = F)
    if(length(mkl)!=length(mkr)){
      p.tx<-gsub("[(](.+)","(\\1)",p.tx)
    }
    p.tx<-gsub("\n"," ",p.tx)
    t.sub<-unlist(strsplit(p.tx," "))
    t.first<-t.sub[1]
    logx<-c(r,t.first,"<p>",t.sub)
    log.p<-matrix(logx,ncol = length(logx),nrow = 1)
    write.table(log.p,log.ns,append = T,col.names = F,quote = F)
    b<-xml_find_all(sub,"b")
    i<-xml_find_all(sub,"i")
    # if simple <p>
    if(length(b)==0&length(i)==0){
#      p.tx<-xml_text(sub)
      sp.ezd<-gsub("\n"," ",p.tx)
      sp.ezd<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',sp.ezd)
      print("R1")
      return(sp.ezd) 
    }
  
      
      if(length(b)>0){
        
        pb.tx<-xml_text(b)
        l.tx<-length(unlist(strsplit(p.tx," ")))
        log.x(c(t.first,pb.tx))
        if (t.first==pb.tx){
          log.x("tfirst==bold")
          p.tx<-gsub(t.first,"",p.tx)
          dif.tx<-length(p.tx)-l.tx
        logx<-c(r,"<sp>",pb.tx,"<p>",p.tx)
        print(logx)
        ma<-grep("allein",p.tx)
        if(length(ma)>0){
         lp<-0 
        }
        log.b<-matrix(logx,ncol = length(logx),nrow = 1)
        write.table(log.b,log.ns,append = T,col.names = F,quote = F)
        }
      i<-xml_find_all(sub,"i")
      pi.tx<-xml_text(i)
      print(pi.tx)
      l.bi<-length(unlist(strsplit(pb.tx," ")))+length(unlist(strsplit(pi.tx," ")))
      print(c(l.tx,l.bi))
      if(l.tx==l.bi){
        pi.tx<-paste0(pi.tx,collapse = " ")
       ezd<-paste0("@",pb.tx,":\t","(",pi.tx,")\n")
       l.tx<-100
       l.bi<-200
       print("R2")
       return(ezd)
      }
      gsub.bi<-c(pi.tx,pb.tx)
      write.table(c("gsub.bi",gsub.bi),log.ns,append = T,col.names = F,quote = F)
      
      p.test<-p.tx
      for (g in 1:length(gsub.bi)){
        gbi<-gsub("([)(])","\\\\\\1",gsub.bi[g])
        write.table(c("#R3gsub.bi.re",gbi),log.ns,append = T,col.names = F,quote = F)
        
        p.test<-gsub(gbi,"",p.test)
      }
      p.eval<-unlist(strsplit(p.test," "))
      dif.bi<-l.tx-length(p.eval)
      write.table(dif.bi,log.ns,append = T,col.names = F,quote = F)
      if(length(i)>0&dif.bi==0){
#        pi.tx<-paste0("(",pi.tx,")")
 #       sp.ezd<-paste0("@",pb.tx,":\t#(",pi.tx,")")
        sp.ezd<-paste0("@",pb.tx,":\t",p.tx)
       # sp.ezd<-gsub("\n"," ",sp.ezd)
      sp.ezd<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',sp.ezd)
      print("R3")
      return(sp.ezd)
      }
      if(length(i)>0&dif.tx!=0){
        logx<-c(i,"<i>",pi.tx)
        print(logx)
        
        log.i<-matrix(logx,ncol = length(logx),nrow = 1)
        write.table(log.i,log.ns,append = T,col.names = F,quote = F)
       # sp.tx
#        sp.ezd<-paste0("@#R4",pb.tx,":\n(",pi.tx,")",p.tx)
        sp.ezd<-paste0("@",pb.tx,":\t",p.tx)
  #      sp.ezd<-paste0("@#R4",pb.tx,":\n(",pi.tx,")",p.tx)
      sp.ezd<-paste0("@",pb.tx,":\t",p.tx)
        #sp.ezd<-gsub("\n"," ",sp.ezd)
        sp.ezd<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',sp.ezd)
        print("R4")
        return(sp.ezd)
      }
    }
  }
  # all kursive stage directions
  # r
  if(xml_name(sub)=="i"){
    sp.ezd<-paste0("$",p.tx)
    sp.ezd<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',sp.ezd)
    print("R5")
    return(sp.ezd)
  }
# all bold speaker
    b<-xml_find_all(sub,"b")
  if(length(b)>0){
#    p.tx<-xml_text(sub)
    b.tx<-xml_text(b)
    p.tx<-gsub(b.tx[1],"",p.tx)
    b.tx<-gsub("\\.",":",b.tx)
    sp.ezd<-paste0("@",b.tx,p.tx)
    sp.ezd<-gsub("\n"," ",sp.ezd)
    sp.ezd<-gsub(":[ ]{1,2}\t",":\t",sp.ezd)
    sp.ezd<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',sp.ezd)
    print("R6")
    return(sp.ezd)
  }
# rest of stage directions after kursive speaker names
    i<-xml_find_all(sub,"i")
    b<-xml_find_all(i,"b")
    p.tx<-xml_text(sub)
  i 
  b
    print(c(b,i))
  if(length(i)>0){
    #p.tx<-xml_text(sub)
    i.tx<-xml_text(i)
    b.tx<-xml_text(b)
    if(length(b.tx)>0){
    l.tb<-length(unlist(strsplit(b.tx," ")))
    l.ti<-length(unlist(strsplit(i.tx," ")))
    l.tx<-length(unlist(strsplit(p.tx," ")))
    cat(i.tx,b.tx,"\n")
    i.gs<-gsub(b.tx,"",p.tx)
      l.ti<-unlist(strsplit(i.gs," "))
      l.tix<-l.ti[l.ti!=""]
      l.tl<-length(l.tix)
      print(l.tix)
      if(print(l.tb+l.tl==l.tx)){
        l.tix<-gsub("\n","",l.tix)
        l.tix<-paste0(l.tix,collapse = " ")
        sp.ezd<-paste0("@",b.tx,":\n(",l.tix,")")
#      sp.ezd<-gsub("\n"," ",sp.ezd)
      sp.ezd<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',sp.ezd)
      print("R8")
      return(sp.ezd)
      }
    }
    l.ti<-length(unlist(strsplit(i.tx," ")))
    l.tx<-length(unlist(strsplit(p.tx," ")))
    # if <p> includes <i>
    if(l.tx>l.ti){
      sp.ezd<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',p.tx)
      sp.ezd<-gsub("\n"," ",sp.ezd)
      print("R9")
      return(sp.ezd)
    }
    # if <p> not includes more than <i> i.e. just stage line, brackets already? chk!
    sp.ezd<-gsub("\n"," ",p.tx)
    m.kl.first<-grep("^\\(",p.tx)
    if(length(m.kl.first)==0){
       sp.ezd<-paste0("(",sp.ezd,")","\n")
       sp.ezd<-gsub("(\\(\\()","(",sp.ezd)
       sp.ezd<-gsub("(\\)\\))",")",sp.ezd)
    }
    m.kl<-grep("[)(]",p.tx)
    cat("length lti,ltx,mfirst",l.tx,l.ti,m.kl.first)
    if(l.tx==l.ti&length(m.kl.first)==0){
    sp.ezd<-paste0("$",sp.ezd,"\n")
    sp.ezd<-gsub("[)(]|\n","",sp.ezd)
    sp.ezd<-paste0(sp.ezd,"\n")
    print("R10")
    }
    sp.ezd<-gsub("\\[([0-9]{1,100})\\]",'<pb n="\\1"/>',sp.ezd)
    #sp.ezd<-gsub("\n"," ",sp.ezd)
    ###################### 15066.critical.pg-78, stage
    #sp.ezd<-c(sp.ezd,"\n")
    ######################
    print(xml_name(sub))
    print(p.tx)
    print(i.tx)
    print(b.tx)
    print("R7")
    print(sp.ezd)
    return(sp.ezd)
  }
      # returns checked and adapted element
      
      
}
###
### test assign
test.dep<-function(){
m1<-grep("Huth",all.elements)
m1
# critical: 160
all.elements[m1]
i<-160 # critical, not wks.
i<-164 # reference, wks.
i<-161
i<-49
i<-40
i<-584
i<-m1
assign.sp(all.elements[[i]],i)
}
sp.lines<-lapply(seq_along(all.elements),function(i){
  assign.sp(all.elements[[i]],i)
})
### RUN
# clean up text
ezd.lines<-unlist(sp.lines)
# get speaker wo/ declaration
m<-grep("Anton",ezd.lines)
ezd.lines[m]
# critical: L54
ezd.lines[1:50]
### the 


# removes redundant lines
m<-ezd.lines=="$"|ezd.lines=="@ *"|ezd.lines==""|
  ezd.lines=="[ ]{1,10}"|ezd.lines=="[.]{1,2}"|ezd.lines=="\n"|ezd.lines=="\n\n"|ezd.lines=="("|ezd.lines==")"|
  ezd.lines==")\n"
sum(m)
# 15066
### preserve inserted linebreaks!
#ezd.lines<-gsub("\n"," ",ezd.lines)
###################################
ezd.lines<-ezd.lines[!m]
ezd.lines<-gsub("\t","\n",ezd.lines)
ezd.lines<-gsub(") )",")",ezd.lines)
for (k in 1:3){
  ezd.lines<-gsub("(^@.+[:])[ \t]{1,2}","\\1\n",ezd.lines)
}
ezd.lines[1:40]
### wks.
# paste single line pagebreaks to preceding line
m1<-grepl("^ ?<pb",ezd.lines)
sum(m1)
m2<-grep("^ ?<pb",ezd.lines)
m3<-m2-1
ezd.lines[m3]<-paste0(ezd.lines[m3],ezd.lines[m2])
ezd.lines<-ezd.lines[!m1]
#ezd.lines[1:100]
ezd.nl<-gsub("  "," ",ezd.lines)
repl.dep<-function(){
  tetext<-'##\n<pb n="10"/>'
  gsub("[^:]\n<pb","<pb",tetext)
  
}
ezd.nl<-gsub("[^:]\n<pb","<pb",ezd.nl)
ezd.nl[1:100]
ezd.nl<-gsub("(:\n<pb.+/>.?)\n","\\1",ezd.nl)
# ezd.nl<-gsub("\t","",ezd.nl)
head(ezd.nl,100)
### normalise speaker ids
ms<-grep("^@.*[:.]",ezd.nl)
#strsplit()
sp.u<-unique(strsplit(ezd.nl[ms],"\n"))
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
head(ezd.nl,10)

ezd.nl.sp<-ezd.nl
### personal:
head(ezd.nl,10)
head(ezd.nl.sp,15)
# split personal list
m<-grep("\\[I\\]|\\[II\\]|\\[III\\]|\\[IV\\]",ezd.nl.sp)
p.head<-strsplit(ezd.nl.sp[m[3]+1],"\\.")
p.head
p.head<-gsub("(^ )|\n","",unlist(p.head))
p.head<-p.head[p.head!=""]
p.head<-strsplit(p.head,",")
p.head<-abind(lapply(p.head,function(x){x[1:4]}),along = 0)
p.person<-p.head[,1]
p.head[is.na(p.head)]<-""
p.head
ezd.nl.l<-1:length(ezd.nl.sp)
# front replace
m2<-grep("#",ezd.nl.sp[m[1]:m[3]])
ezd.nl.sp[m2]<-gsub("#","",ezd.nl.sp[m2])
front<-paste0(ezd.nl.sp[m2],collapse = " ")
front<-unlist(strsplit(front,"\\."))
front.desc<-c("@title ","@subtitle ","")
front<-paste0(front.desc,front)
front<-front[1:2]
front
#m
ezd.nl.out<-ezd.nl.l[c(ezd.nl.l[m[4]+1]:ezd.nl.l[length(ezd.nl.l)])]
ezd.nl.sp.f<-c(front,ezd.nl.sp[ezd.nl.out])
### end personal
###########################################
ezd.nl.sp<-gsub("\t","\n",ezd.nl.sp.f)
ezd.temp<-tempfile("ezdramamarkup.txt")
writeLines(unlist(ezd.nl.sp),ezd.temp)
ezd.nl.sp<-readLines(ezd.temp)
head(ezd.nl.sp,10)
ms<-grep("^@.*[:.]",ezd.nl.sp)
ms<-grep("^@.*",ezd.nl.sp)
ezd.nl.sp[ms]

for (k in 1:length(sp.cor$speaker)){
  if(sp.cor$corrected[k]!=""){
    m<-grep(sp.cor$speaker[k],ezd.nl.sp)
    ezd.nl.sp[m]<-gsub(sp.cor$speaker[k],sp.cor$corrected[k],ezd.nl.sp[m])
  }
}
ezd.nl.sp[ms]<-gsub(":",".",ezd.nl.sp[ms])
#ezd.nl.sp<-gsub("\t","\n",ezd.nl.sp)
mna<-ezd.nl.sp==""
ezd.nl.sp<-ezd.nl.sp[!mna]
ezd.nl.sp<-gsub("^ ?|^\n","",ezd.nl.sp)
#wks.
writetx<-function(tx,outns){
outns<-"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/test/nltest.txt"
  writeLines(tx,outns)  

outns<-"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/test/nltest.txt"
tx<-c("drei","mal","schwarzer\n","kater")
#writetx(tx,outns)
#wks., \n linebreaks are preserved/inserted in output
}
save.lines<-function(ezd.lines){
  ezd_markup.ns<-"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/ezd/steltzer_ezd.001"
  ezd_markup_text<-paste0(ezd_markup.ns,".txt")
  writeLines(unlist(ezd.lines),ezd_markup_text)
 # writeLines(unlist(ezd.lines),ezd_markup_text)
#?writetext
    return(list(ezd_markup.ns,ezd_markup_text))
  }
### personal:
head(ezd.nl,10)
head(ezd.nl.sp,10)
# m<-grep("\\[I\\]|\\[II\\]|\\[III\\]|\\[IV\\]",ezd.nl.sp)
# # split personal list
# p.head<-strsplit(ezd.nl.sp[m[3]+1],"\\.")
# p.head
# p.head<-gsub("^ ","",unlist(p.head))
# p.head<-p.head[p.head!=""]
# p.head<-strsplit(p.head,",")
# p.head<-abind(lapply(p.head,function(x){x[1:4]}),along = 0)
# p.person<-p.head[,1]
# p.head[is.na(p.head)]<-""
# p.head
# ezd.nl.l<-1:length(ezd.nl.sp)
# # front replace
# m2<-grep("#",ezd.nl.sp[m[1]:m[3]])
# ezd.nl.sp[m2]<-gsub("#","",ezd.nl.sp[m2])
# front<-paste0(ezd.nl.sp[m2],collapse = " ")
# front<-unlist(strsplit(front,"\\."))
# front.desc<-c("@title ","@subtitle ","")
# front<-paste0(front.desc,front)
# front<-front[1:2]
# #m
# ezd.nl.out<-ezd.nl.l[c(ezd.nl.l[m[4]+1]:ezd.nl.l[length(ezd.nl.l)])]
# ezd.nl.sp.f<-c(front,ezd.nl.sp[ezd.nl.out])
### save ezd before fail
###################################
# removes redundant lines
ezd.lines<-ezd.nl.sp
m<-ezd.lines=="$"|ezd.lines=="@ *"|ezd.lines==""|
  ezd.lines=="[ ]{1,10}"|ezd.lines=="[.]{1,2}"|ezd.lines=="\n"|ezd.lines=="\n\n"|ezd.lines=="("|ezd.lines==")"|ezd.lines==")\n"
sum(m)
ezd.lines<-ezd.lines[!m]
##### TODO shift <pb>
shift.pb<-function(){
m<-grep("^<pb",ezd.lines)
m0<-grep("^<pb",ezd.lines)
m.1<-m-1
ezd.lines[m.1]
m.3<-grepl("^@",ezd.lines[m.1])
m.3<-(m.3*1-1)*-1
m.3<-m.3==1
ezd.lines[m.1][m.3]<-paste0(ezd.lines[m.1][m.3],ezd.lines[m][m.3])
ezd.lines[m]
m.3<-grep("^@",ezd.lines[m.1])
m.4<-m.3-1
ezd.lines[m.1[m.4]]
#m<-m[!m.3]
m.2<-m-2
m.3<-grepl("^@",ezd.lines[m.2])
m.3<-(m.3*1-1)*-1
m.3<-m.3==1
sum(m.3)
ezd.lines[m.2][m.3]<-paste0(ezd.lines[m.2][m.3],ezd.lines[m][m.3])
ezd.lines[m.2]
#ezd.lines[m.2]<-paste0(ezd.lines[m.2][m.3],ezd.lines[m][m.3])
m<-m[!m.3]
m.4<-m-3
m.3<-grepl("^@",ezd.lines[m.4])
m.3<-(m.3*1-1)*-1
m.3<-m.3==1
sum(m.3)
ezd.lines[m.4][m.3]<-paste0(ezd.lines[m.4][m.3],ezd.lines[m][m.3])
ezd.lines[m.4]
ezd.lines[m0-2]
ezd.lines.pb<-ezd.lines[!m0]
m<-grep("<pb",ezd.lines)
ezd.lines[m]
}
#ezd.lines[m.2]<-paste0(ezd.lines[m.2][m.3],ezd.lines[m][m.3])
# 15066
### preserve inserted linebreaks!
#ezd.lines<-gsub("\n"," ",ezd.lines)
###################################
###################################
ezd_markup<-save.lines(ezd.lines)
###############################################################################
ezd_markup_text<-ezd_markup[[2]]
ezd_markup.ns<-ezd_markup[[1]]
#####################
write.table("--- pdesc --- ",log.ns,append = T,col.names = F,quote = F)
log.x(p.head)
p.desc.re<-function(x){
log.x(x)
  paste0(x,collapse = ",")
  
}
p.head
p.desc<-apply(p.head[,2:length(p.head[1,])],c(1),FUN =p.desc.re)
p.desc<-gsub("([,]{1,3}$)","",p.desc)
p.desc<-gsub("^ ","",p.desc)
p.desc
# this is redundant, castlist is done in the .xml, but we need p.desc for that
#p.desc.m<-paste0(p.head[,1],"<roleDesc>",p.desc,"</roleDesc>")
#ezd.nl.sp[m[1]]<-"^"
#m.out<-c(m[1],(m[2]+1):length(ezd.nl.sp))
#ezd.nl.sp.head<-ezd.nl.sp[m.out]
#ezd.nl.sp.head<-append(ezd.nl.sp.head,p.desc.m,m[1])
#save.lines(ezd.lines)
##########################################
# write ezdrama marked up text
# 15063.out
#writeLines(ezd.nl.sp.head,ezd_markup_text)
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
xml.tx[m]<-gsub("p>pb","p><pb",xml.tx[m])
xmltemp<-tempfile("xmltemp.xml")
dracor_head<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/dracor_header.xml")
# insert fixed header
xml.tx<-append(xml.tx,dracor_head[1:4],0)
m<-grepl("<TEI xml:id",xml.tx)
xml.tx<-xml.tx[!m]
head(xml.tx)
writeLines(xml.tx,xmltemp)
writeLines(xml.tx,"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/tei/steltzer.temp.xml")
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
 # write_xml(xml,xml.final)
}
xml.final<-paste0("~/Documents/GitHub/ETCRA5_dd23/tei/","steltzer_montenegro.final.xml")
xml.final.dracor<-paste0("~/Documents/GitHub/fork/gerdracor/tei/","steltzer_franziska-montenegro.xml")
write.final.xml(xml,xml.final)
#write.final.xml(xml,xml.final.dracor)


