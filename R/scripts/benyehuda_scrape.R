#13373.TEI conversion essai
#20230911(12.00)
###########################
#documented essai for global appplication on html sources
###########################
#1.abstract:
#TEI declaration of benyehuda.org dramatext for further processing
getwd()
##########################
#library(stringi)
library(httr)
library(rvest)
library(xml2)
library(stringi)
library(stringr)
library(R.utils)
library(readr)
library(XML)
###################################
#this calls static txt from repository
#src<-"https://raw.githubusercontent.com/esteeschwarz/DH_essais/main/data/corpus/klemm_besuch/klemm(1765)_wiki_preprocessed.txt"
#api_call<-httr::GET(src)
#txt<-httr::content(api_call,"text")
###################################
# lisa scrape:
# src<-"https://de.wikisource.org/wiki/Der_Besuch_(Klemm)"
# dta1<-read_html(src)
# xpathkl<-'//*[@id="mw-content-text"]/div[1]/div[2]'
# #xpath copied from browser developer tools (safari)
# html_nodes(dta1,xpath = xpathkl)
# txt<-html_nodes(dta1,xpath = xpathkl) %>%html_text()
#gets plain text
#wks.
#now with epub formatted text:
# src<-"https://ws-export.wmcloud.org/?format=epub&lang=de&page=Der_Besuch_(Klemm)"
# dta2<-read_html(src) #no
# x<-GET(src) #no
# dta2<-content(x,"text")
getwd()
#lapsi
#setwd("~/Documents/GitHub/ETCRA5_dd23/R")
#ewa
#setwd("~/Documents/GIT/ETCRA5_dd23/R")
#mini
#setwd("gith/ETCRA5_dd23/R")
####################
#src<-"https://raw.githubusercontent.com/esteeschwarz/ETCRA5_dd23/main/R/data/c0_Der_Besuch_Klemm_wsource_epub.xml"
src<-"https://benyehuda.org/read/33373" #33373
#src<-"data/c0_Der_Besuch_Klemm_wsource_epub.xml"

#bensrc<-"~/boxHKW/21S/DH/local/EXC2020/DD23/data/benyehuda.drama.csv"
#bencsv<-read.csv(bensrc)
#ids<-bencsv$TextID

k<-2
#for (k in 2:length(ids)){

#id.p<-ids[k]
id.p<-33373
#2nd: 10777
src<-paste0("https://benyehuda.org/read/",id.p)

d2<-read_html(src)
d3<-d2
####all elements##
# all.e<-d2 %>% 
#   xml_find_all('//div/*') %>%
#   xml_path() #wks. finds all div elements, including p
# all_e
all.e<-d2 %>% xml_find_all('//*[@id="actualtext"]/*') #xpath of div copied from safari 
#cat(xml_text(all.e))
all.h2<-all.e%>%xml_find_all('//h2') #act
all.h3<-all.e%>%xml_find_all('//h3') #scene
all.strong<-all.e%>%xml_find_all('//strong') #speaker
#all.p<-xml_find_all(all.e,'.//p')
head(all.e)
#head(all.p) #not wks.
df.l<-length(all.e)
df.s<-data.frame(pid=1:df.l,h2=1:df.l,h3=1:df.l,strong=1:df.l,p=1:df.l)
df.s[1:df.l,2:length(df.s)]<-NA
df.s$cpt<-d2 %>% xml_find_all('//*[@id="actualtext"]/*')%>%xml_text()
m.h2<-grep("h2",all.e)
h2.temp<-xml_text(all.h2)
m.h3<-grep("h3",all.e)
df.s$h3[m.h3]<-xml_text(all.h3)
df.s$h2[m.h2]<-xml_text(all.h2)
m.strong<-grep("strong",all.e)
df.s$strong[m.strong]<-all.strong%>%xml_text()
#write_html(d2,"benyehuda.html")

#k<-1
for(k in 1:length(df.s$pid)){
  df.s$p[k]<-gsub(df.s$strong[k],"",df.s$cpt[k])
}
#wks. removes bold text of p
#now tags
#the starting row (30) has to be adapted for each play, TODO: find first speaker instance
#like: scrape cast list:
castlist.s<-grep("הנפשות",df.s$cpt)+1
castlist.e<-grep("\\* \\* \\*",df.s$cpt)
#manually:
######################
#start from here again
#for second play on list:
#castlist.e<-12
sp.l<-stri_extract_all_words(df.s$cpt[castlist.s:castlist.e],simplify = T)
sp.l
#grep first speaker instance
sp.l.m<-sp.l%in%df.s$strong
sp.l.m.a<-array()
#k<-1
for(k in 1:length(sp.l)){
  sp.l.m.a[k] <- min(grep(sp.l[k],df.s$strong))
}
sp.l.m.a
sp.l.m.a<-sp.l.m.a[sp.l.m.a>castlist.e]
sp.l.m.a<-sp.l.m.a[sp.l.m.a<=length(df.s$pid)]
sp.l.m.a<-sp.l.m.a[!is.na(sp.l.m.a)]
sp.first<-min(sp.l.m.a)
#manually:
#sp.first<-24
#min(sp.l.m.a,na.rm = T)
k<-30
df.s$t.1<-NA

# for (k in sp.first:length(df.s$pid)){
#   m<-grep(":",df.s$strong[k]) #only speaker declarations
#   if(m==T)
#     df.s$t.1[k]<-paste0('<sp who="#',df.s$strong[k],'"><speaker>',df.s$strong[k],'</speaker>','<p>',df.s$p[k],'</p></sp>')
# }
#m<-grep(":",df.s$strong) #only speaker declarations
#manually:
##########
#m<-grep("\\.",df.s$strong) #only speaker declarations
#m<-grep("\\.",df.s$cpt) #only speaker declarations

# m
# m.sp<-stri_split_regex(df.s$cpt[m],"\\.",simplify = T)
# df.s$p.1<-""
# df.s$p.1[m]<-m.sp[,1]
# df.s$p.2<-""
# k<-17
# for(k in 1:length(m)){
#   df.s$p.2[m[k]]<-paste0(m.sp[k,2:length(m.sp[1,])],collapse = " ")
# }
#############
m<-grep(":",df.s$strong) #only speaker declarations
m.2<-match(sp.first,m)
mna<-is.na(df.s$strong)
df.s$p.1[mna]<-NA
#starting 24
m.3<-sp.first
sp.first
m.2<-match(sp.first,m)
m<-m[m.2:length(m)]
m.speaker<-m
speaker<-df.s$strong
speaker<-gsub(":","",speaker)
speaker<-gsub('"',"''",speaker)
speaker.u<-speaker[m]
speaker.u<-unique(speaker.u)
#TODO: manually edit latin speaker.u.d for speaker id entry
speaker.u.d<-data.frame(sp.h=speaker.u,sp.d=NA)
#speaker.u.d.2<-fix(speaker.u.d) ## >>> save somewhere before reset!
#library(clipr)
#write_clip(speaker.u.d.2$var3)
# speaker.d.c<-c("dr_gri","ruben","hanna","mirkin","blink","miri",
# "berla","diamenet","rubik","diamenu","klara","dimenet","daimenet","all")
speaker.d.c<-c("dr_gri","ruben","hannah","mirkin","blink","miri",
               "berla","diamant","rubik","diamant","klara","diamant","diamant","voice")
# the lettering of <diamant> varies in the boldface speaker declaration,
# normalised this here; still due 
speaker.df<-data.frame(speaker.h=speaker.u.d$sp.h,speaker.d=speaker.d.c)
#13377.to complete...
df.s$speaker.d<-NA
k<-1
for(k in 1:length(speaker.df$speaker.h)){
  m.h<-grep(speaker.df$speaker.h[k],speaker)
  m.to.h<-speaker.df$speaker.d[k]
  df.s$speaker.d[m.h]<-speaker.df$speaker.d[k]
}
m<-is.na(df.s$speaker.d)
#df.s$t.1[m]<-paste0('<sp who="#',df.s$strong[m],'"><speaker>',df.s$strong[m],'</speaker>','<p>',df.s$p[m],'</p></sp>')
#df.s$t.1[m]<-paste0('<sp who="#',df.s$speaker.d[m],'"><speaker>',speaker[m],'</speaker>','<p>',df.s$p[m],'</p></sp>')
df.s$t.1<-paste0('<sp who="#',df.s$speaker.d,'"><speaker>',speaker,'</speaker>','<p>',df.s$p,'</p></sp>')
df.s$t.1[m]<-NA
#wks. 33373
############second
# df.s$t.1[m]<-paste0('<sp who="#',df.s$p.1[m],'"><speaker>',df.s$p.1[m],'</speaker>','<p>',df.s$p.2[m],'</p></sp>')
# df.s$t.1[mna]<-NA

#wks not for staged speakers
# m<-grep("\\(|\\)",df.s$p.1)
# sp.st<-stri_split_regex(df.s$p.1[m],"\\(",simplify = T)
# df.s$sp.s<-""
# df.s$sp.s<-df.s$p.1
# df.s$sp.s[m]<-sp.st
# for(k in 1:length(sp.st[,1])){
#   df.s$stage[m]<-paste0('<stage>',sp.st[k,2],'</stage>',collapse = "")
# }
# m.s<-array()
# df.s$sp.in.strong<-NA
# k<-1
# sp.l[1]<-"אוריאל" #some reason speakerlist orthographiy differs from speaker entries, manually adapted
# 
# for(k in 1:length(sp.l[,1])){
#   m.s<-grep(sp.l[k],df.s$strong)
#   df.s$sp.in.strong[m.s]<-df.s$strong[m.s]
# }
# m<-!is.na(df.s$sp.in.strong)
# df.s$sp.s[m]<-df.s$sp.in.strong[m]
# ######
# m<-m.speaker
# speaker<-df.s$sp.s[m]
# 
# #df.s$t.1[m]<-paste0('<sp who="#',df.s$sp.s[m],'"><speaker>',df.s$sp.s[m],'</speaker>','<p>',df.s$p.2[m],'</p></sp>')
# #recover 13375
# df.s$t.1[m]<-gsub(regx,'<stage> \\2 </stage>',df.s$t.1[m],perl=T)
# 
# df.s$t.1[mna]<-NA
# 
# m.s
# sp.l
###############################2nd
#stage directions:
m<-grep("\\(|\\)",df.s$cpt)
t<-"drei(malschwarzer)kater"
regx<-"(\\((.*)\\))"
a<-28
a<-m[1]
df.s$cpt[a]
gsub(regx,'<stage> \\2 </stage>',df.s$cpt[a],perl=T)

gsub(regx,'<stage>\\2</stage>',t,perl = T)
#df.s$t.1[m]<-gsub(regx,'<stage> \\2 </stage>',df.s$cpt[m],perl=T)
df.s$t.1[m]<-gsub(regx,'<stage> \\2 </stage>',df.s$t.1[m],perl=T)

# a<-28
# df.s$cpt[a]
# gsub(regx,'<stage> \\2 </stage>',df.s$cpt[a],perl=T)
# df.s$t.1[28] #wks. just not in view table!
#now for h3 divs
g.h3<-!is.na(df.s$h3)
if(sum(g.h3)>0){
  g.h3<-which(g.h3)
  k<-1
  k<-6
  g.h3[k]
  df.s$t.3<-NA
  #if(g.h3>0))
  for (k in 1:length(g.h3)){
    t.start<-g.h3[k]+1
    ifelse(k<length(g.h3),t.end<-g.h3[k+1]-1,t.end<-length(df.s$pid))
    m<-is.na(df.s$t.1)
    df.s$t.1[m]<-""
    df.s$t.3[g.h3[k]]<-paste0('<div type="scene"><head>',
                              df.s$h3[g.h3[k]],'</head>',paste0(df.s$t.1[t.start:t.end],collapse=""),'</div>',collapse = "")
  }
}
g.h2<-!is.na(df.s$h2)
if(sum(g.h2)>0){
  
  g.h2<-which(!is.na(df.s$h2))
  k<-1
  k<-6
  g.h2[k]
  df.s$t.2<-NA
  for (k in 1:length(g.h2)){
    t.start<-g.h2[k]+1
    ifelse(k<length(g.h2),t.end<-g.h2[k+1]-1,t.end<-length(df.s$pid))
    m<-is.na(df.s$t.3)
    df.s$t.3[m]<-""
    df.s$t.2[g.h2[k]]<-paste0('<div type="act"><head>',
                              df.s$h2[g.h2[k]],'</head>',paste0(df.s$t.3[t.start:t.end],collapse=""),'</div>',collapse = "")
  }
}
# df.s$t.2[1135]
# length(paste0('<div type="act"><head>ACT ',
#        k,'</head>',paste0(df.s$t.1[t.start:t.end],collapse=""),'</div>',collapse = ""))
#writeLines(df.s$t.2[24],"benyehuda-33373-1.html")
#wks. scenes/acts declaration
#now castlist:
#castlist.t<-stri_split_regex(df.s$cpt[castlist.s:castlist.e],"–",simplify = T)
castlist.t<-df.s$cpt[castlist.s:castlist.e]
castlist.t<-castlist.t[1:length(castlist.t)-1]
df.s$t.4<-NA
df.s$t.4[castlist.s-1]<-paste0('<div type="Dramatis Personae"><castList><head>',
                               df.s$cpt[castlist.s-1],'</head>',paste0('<castItem>',castlist.t,'</castItem>',collapse = ""),'</castList></div>')
#df.s$t.4
getwd()
#writeLines(df.s$t.2[24],"benyehuda-33373.html")
df.s$t.5<-NA
xp.author<-'//*[@id="header-general"]/div[3]/div[1]/div[2]/div/div[1]/div[2]/*'
author.h<-xml_find_all(d2,xp.author)
author.a<-xml_find_all(author.h,"a")
author.ns<-xml_text(author.a)
author.ns.d<-c("igal","mosinsohn")

xp.work<-'//*[@id="header-general"]/div[3]/div[1]/div[2]/div/div[1]/div[1]'
work.h<-xml_find_all(d2,xp.work)
#work.a<-xml_find_all(work.h,"a")
work.ns<-xml_text(work.h)
wrk.ns.d<-"bride-and-groom"

#wks.
# danielmeta:
#   <head>
#   <meta charset="utf-8"/>
#   <title>חתן וכלה</title>
#   <meta name="author" content="יצחק ליבוש פרץ"/>
#   </head>
frontline<-paste0('<front>',paste0(df.s$cpt[1:castlist.s-1],collapse=""),'</front>',collapse = "")
frontline
headerline<-paste0('<html><head><meta charset="utf-8"/><title>',work.ns,'</title><meta name="author" content="',author.ns,'"/></head><body>')
headerline
df.s$t.5[1]<-headerline
df.s$t.5[2]<-frontline
df.s$t.c<-NA
df.s$t.c[!is.na(df.s$t.5)]<-df.s$t.5[!is.na(df.s$t.5)]
df.s$t.c[!is.na(df.s$t.4)]<-df.s$t.4[!is.na(df.s$t.4)]
#df.s$t.c[!is.na(df.s$t.4)]<-df.s$t.5[!is.na(df.s$t.5)]
df.s$t.c[!is.na(df.s$t.2)]<-df.s$t.2[!is.na(df.s$t.2)]
df.s$t.c[2]<-paste0("<text>",df.s$t.c[2])
df.s$t.c[length(df.s$pid)]<-paste0(df.s$t.c[length(df.s$pid)],'</text></body></html>')

######
# <publisher xml:id="dracor">
#   DraCor
# </publisher>
#   <idno type="URL">
#   https://dracor.org
# </idno>
#   <availability>
#   <licence>
#   <ab>
#   CC0 1.0
# </ab>
#   <ref target="https://creativecommons.org/publicdomain/zero/1.0/">
#   Licence
# </ref>
#   </licence>
#   </availability>
  ######
src.tei<-paste("~/Documents/GitHub/hedracor/tei/",list.files("~/Documents/GitHub/hedracor/tei")[2],sep = "/")
tei.dracor<-read_xml(src.tei)
getwd()

doc<-xmlParseDoc(src.tei)
#x1<-xmlToList(doc)  
x1<-xmlRoot(doc)
x2<-xmlToList(x1)  
#x3<-xmlToDataFrame(x1)

#source<-"https://stackoverflow.com/questions/48120782/r-write-list-to-csv-line-by-line"
# crush to flat matrix
tei.mat <- do.call(rbind, x2) #x2=my_list
# add in list names as new column
tei.df <- data.frame(id = names(x2), tei.mat)
tei.df$fileDesc$teiHeader$titleStmt$title$text

write_csv(tei.df,"tei_dracor.csv")
#xml_text(xml_find_all(tei.dracor,"//*"))<-tei.df
tei.temp<-function(){
tei<-xml_new_root("TEI")
xml_add_child(tei,"teiHeader")
#xml_add_child(tei,"teiHeader")
xml_add_child(xml_children(tei),"fileDesc")
xml_add_child(xml_children(xml_children(tei)),"titleStmt")
xml_add_child(xml_children(xml_children(xml_children(tei))),"title")
xml_add_child(xml_children(xml_children(xml_children(tei))),"author")
xml_add_child(xml_children(xml_children(xml_children(tei))),"author")
xml_add_child(xml_children(xml_children(tei)),"publicationStmt",.where = 1)
xml_add_child(xml_find_first(tei,"//publicationStmt"),"publisher")
xml_add_child(xml_find_first(tei,"//publicationStmt"),"idno")
xml_add_child(xml_find_first(tei,"//publicationStmt"),"availability")
xml_add_child(xml_find_first(tei,"//availability"),"license")
xml_add_child(xml_find_first(tei,"//license"),"ref")
xml_add_child(xml_find_first(tei,"//license"),"ab")
xml_add_child(xml_children(xml_children(tei)),"sourceDesc")
xml_add_child(xml_find_first(tei,"//sourceDesc"),"bibl")
xml_add_child(xml_find_first(tei,"//bibl"),"name")
xml_add_child(xml_find_first(tei,"//bibl"),"idno")
xml_add_child(xml_find_first(tei,"//bibl"),"availability")
xml_add_child(xml_find_first(tei,"//bibl/availability"),"p")
xml_add_child(xml_children(tei),"profileDesc")
xml_add_child(xml_find_first(tei,"//profileDesc"),"particDesc")
return(tei)
}
tei.temp.df<-function(){
  tei<-xml_new_root("TEI")
  xml_add_child(tei,"teiHeader")
  #xml_add_child(tei,"teiHeader")
  xml_add_child(xml_children(tei),"fileDesc")
  xml_add_child(xml_children(xml_children(tei)),"titleStmt")
  xml_add_child(xml_children(xml_children(xml_children(tei))),"title")
  xml_add_child(xml_children(xml_children(xml_children(tei))),"author")
  xml_add_child(xml_children(xml_children(xml_children(tei))),"author")
  xml_add_child(xml_children(xml_children(tei)),"publicationStmt",.where = 1)
  xml_add_child(xml_find_first(tei,"//publicationStmt"),"publisher")
  xml_add_child(xml_find_first(tei,"//publicationStmt"),"idno")
  xml_add_child(xml_find_first(tei,"//publicationStmt"),"availability")
  xml_add_child(xml_find_first(tei,"//availability"),"license")
  xml_add_child(xml_find_first(tei,"//license"),"ref")
  xml_add_child(xml_find_first(tei,"//license"),"ab")
  xml_add_child(xml_children(xml_children(tei)),"sourceDesc")
  xml_add_child(xml_find_first(tei,"//sourceDesc"),"bibl")
  xml_add_child(xml_find_first(tei,"//bibl"),"name")
  xml_add_child(xml_find_first(tei,"//bibl"),"idno")
  xml_add_child(xml_find_first(tei,"//bibl"),"availability")
  xml_add_child(xml_find_first(tei,"//bibl/availability"),"p")
  xml_add_child(xml_children(tei),"profileDesc")
  xml_add_child(xml_find_first(tei,"//profileDesc"),"particDesc")
  return(tei)
}
### here insert castlist adapted
#xml_add_child(xml_find_first(tei,"//particDesc"),"")
castlist.t
speaker.u.d
speaker.df
# <listPerson>
#   <person xml:id="ֲkַu">
#   <persName>
#   אֲקַו
# </persName>
#   </person>
#   <person xml:id="bun">
#   <persName>
#   בּוּן
# </persName>
#   </person>
k<-1
tei<-tei.temp()
tei.33373.person<-xml_new_root("particDesc")
xml_add_child(xml_find_first(tei.33373.person,"//particDesc"),"listPerson")
for(k in 1:length(speaker.df$speaker.d)){
  xml_add_child(xml_find_first(tei.33373.person,"//listPerson"),"person")
  xml_add_child(xml_find_all(tei.33373.person,"//person")[k],"persName")
  xml_set_attr(xml_find_all(tei.33373.person,"//person")[k],"xml:id",speaker.df$speaker.d[k])
  xml_set_text(xml_find_all(tei.33373.person,"//persName")[k],speaker.df$speaker.h[k])
  
}
# x1<-xmlRoot(tei.33373.person)
# x2<-xmlToList(tei.33373.person)  
# #x3<-xmlToDataFrame(x1)
# 
# #source<-"https://stackoverflow.com/questions/48120782/r-write-list-to-csv-line-by-line"
# # crush to flat matrix
# tei.mat <- do.call(rbind, x2) #x2=my_list
# # add in list names as new column
# tei.df <- data.frame(id = names(x2), tei.mat)
tei.df$profileDesc$teiHeader$particDesc<-tei.33373.person #no
write_xml(as.list(tei.df),"tei_df.xml")
saveXML(as.list(tei.df),"tei_df.xml")
tei.df$profileDesc$teiHeader$particDesc
# xml_remove(xml_find_all(tei.dracor,"//particDesc"))
# xml_replace(xml_find_first(tei.dracor,"//profileDesc"),tei.33373.person)
# html_form_set(xml_child(xml_child(xml_child(tei.dracor, 1), 2), 1),tei.33373.person)
# xmlNode(xml_find_all(tei.dracor,"//particDesc"))<-tei.33373.person
# xml_find_all(tei.dracor,"//particDesc")<-tei.33373.person
# xml_add_child(xml_find_first(tei,"//particDesc"),"listPerson")
# #xml_add_child(xml_find_all(tei,"//person"),"persName")
# for(k in 1:length(speaker.df$speaker.d)){
#   xml_add_child(xml_find_first(tei,"//listPerson"),"person")
#   xml_add_child(xml_find_all(tei,"//person")[k],"persName")
#   xml_set_attr(xml_find_all(tei,"//person")[k],"xml:id",speaker.df$speaker.d[k])
#   xml_set_text(xml_find_all(tei,"//persName")[k],speaker.df$speaker.h[k])
#   
# }
### breakpoint
xml_add_child(tei,"standOff")
xml_add_child(xml_find_all(tei,"//standOff"),"listEvent")
xml_add_child(xml_find_all(tei,"//listEvent"),"event")
xml_add_child(xml_find_all(tei,"//event"),"desc")

#xml_set_attr(tei,"xmlns","http://www.tei-c.org/ns/1.0")
xml_set_attr(xml_find_all(tei,"//title"),"type","main")
xml_set_text(xml_find_first(tei,"//author"),author.ns)
xml_set_attr(xml_find_all(tei,"//author")[2],"xml:lang","eng")
author.ns.c<-paste0(capitalize(author.ns.d),collapse = " ")
xml_set_text(xml_find_all(tei,"//author")[2],author.ns.c)
xml_set_attr(xml_find_all(tei,"//publisher"),"xml:id","dracor")
xml_set_text(xml_find_all(tei,"//publisher"),"Dracor")
xml_set_attr(xml_find_all(tei,"//publicationStmt/idno"),"type","URL")
xml_set_text(xml_find_all(tei,"//publicationStmt/idno"),"https://dracor.org")
xml_set_attr(xml_find_all(tei,"//license/ref"),"target","https://creativecommons.org/publicdomain/zero/1.0/")
xml_set_text(xml_find_all(tei,"//license/ab"),"CC0 1.0")
xml_set_text(xml_find_all(tei,"//license/ref"),"License")
xml_set_attr(xml_find_all(tei,"//bibl"), "type","digitalSource")
xml_set_text(xml_find_all(tei,"//bibl/name"),"ENTER SOURCE NAME HERE")
xml_set_attr(xml_find_all(tei,"//bibl"), "type","URL")
xml_set_text(xml_find_all(tei,"//bibl/idno"),"ENTER SOURCE URL HERE")
xml_set_attr(xml_find_all(tei,"//bibl/availability"), "status","free")
xml_set_text(xml_find_all(tei,"//bibl/availability/p"),"In the public domain.")
m<-!is.na(df.s$t.c)
#ben.ns<-paste0("benyehuda-",id.p,"-text.xml")
hedracor.git<-"~/documents/github/hedracor/html_scraped-refact"
#ben.ns<-paste0(hedracor.git,"/benyehuda-",id.p,"-text.xml")
ben.ns<-paste0(hedracor.git,"/",author.ns.d[1],"_",author.ns.d[2],"_",id.p,".xml")
ben.ns
writeLines(df.s$t.c[m],ben.ns)
#save csv
write_csv(df.s,paste0(hedracor.git,"/benyehuda-",id.p,"-db.csv"))
xmlformat<-paste0("xmlformat -i ",ben.ns)
system(xmlformat)


#writeLines(df.s$t.c[m],"benyehuda-33373-text.html")

#}#end extract loop
