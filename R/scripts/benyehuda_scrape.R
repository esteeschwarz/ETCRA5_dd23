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

bensrc<-"~/boxHKW/21S/DH/local/EXC2020/DD23/data/benyehuda.drama.csv"
bencsv<-read.csv(bensrc)
ids<-bencsv$TextID

k<-2
#for (k in 2:length(ids)){
#save csv
write_csv(df.s,paste0("benyehuda-",id.p,"-text.csv"))

id.p<-ids[k]
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
castlist.e<-12
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
m<-grep(":",df.s$strong) #only speaker declarations
#manually:
##########
m<-grep("\\.",df.s$strong) #only speaker declarations
m<-grep("\\.",df.s$cpt) #only speaker declarations

m
m.sp<-stri_split_regex(df.s$cpt[m],"\\.",simplify = T)
df.s$p.1<-""
df.s$p.1[m]<-m.sp[,1]
df.s$p.2<-""
k<-17
for(k in 1:length(m)){
df.s$p.2[m[k]]<-paste0(m.sp[k,2:length(m.sp[1,])],collapse = " ")
}
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
speaker<-df.s$strong[m]
speaker<-gsub(":","",speaker)
speaker<-gsub('"',"''",speaker)
#df.s$t.1[m]<-paste0('<sp who="#',df.s$strong[m],'"><speaker>',df.s$strong[m],'</speaker>','<p>',df.s$p[m],'</p></sp>')
df.s$t.1[m]<-paste0('<sp who="#',speaker[m],'"><speaker>',speaker[m],'</speaker>','<p>',df.s$p[m],'</p></sp>')

#wks. 33373
############second
df.s$t.1[m]<-paste0('<sp who="#',df.s$p.1[m],'"><speaker>',df.s$p.1[m],'</speaker>','<p>',df.s$p.2[m],'</p></sp>')
df.s$t.1[mna]<-NA

#wks not for staged speakers
m<-grep("\\(|\\)",df.s$p.1)
sp.st<-stri_split_regex(df.s$p.1[m],"\\(",simplify = T)
df.s$sp.s<-""
df.s$sp.s<-df.s$p.1
df.s$sp.s[m]<-sp.st
for(k in 1:length(sp.st[,1])){
df.s$stage[m]<-paste0('<stage>',sp.st[k,2],'</stage>',collapse = "")
}
m.s<-array()
df.s$sp.in.strong<-NA
k<-1
sp.l[1]<-"אוריאל" #some reason speakerlist orthographiy differs from speaker entries, manually adapted

for(k in 1:length(sp.l[,1])){
m.s<-grep(sp.l[k],df.s$strong)
df.s$sp.in.strong[m.s]<-df.s$strong[m.s]
}
m<-!is.na(df.s$sp.in.strong)
df.s$sp.s[m]<-df.s$sp.in.strong[m]
######
m<-m.speaker
speaker<-df.s$sp.s[m]

df.s$t.1[m]<-paste0('<sp who="#',df.s$sp.s[m],'"><speaker>',df.s$sp.s[m],'</speaker>','<p>',df.s$p.2[m],'</p></sp>')
df.s$t.1[mna]<-NA

m.s
sp.l
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
df.s$t.1[m]<-gsub(regx,'<stage> \\2 </stage>',df.s$cpt[m],perl=T)
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

xp.work<-'//*[@id="header-general"]/div[3]/div[1]/div[2]/div/div[1]/div[1]'
work.h<-xml_find_all(d2,xp.work)
#work.a<-xml_find_all(work.h,"a")
work.ns<-xml_text(work.h)

#wks.
# danielmeta:
#   <head>
#   <meta charset="utf-8"/>
#   <title>חתן וכלה</title>
#   <meta name="author" content="יצחק ליבוש פרץ"/>
#   </head>
frontline<-paste0('<front>',paste0(df.s$cpt[1:castlist.s-1],collapse=""),'</front>',collapse = "")
frontline
headerline<-paste0('<head><meta charset="utf-8"/><title>',work.ns,'</title><meta name="author" content="',author.ns,'"/></head><body>')
headerline
df.s$t.5[1]<-headerline
df.s$t.5[2]<-frontline
df.s$t.c<-NA
df.s$t.c[!is.na(df.s$t.5)]<-df.s$t.5[!is.na(df.s$t.5)]
df.s$t.c[!is.na(df.s$t.4)]<-df.s$t.4[!is.na(df.s$t.4)]
#df.s$t.c[!is.na(df.s$t.4)]<-df.s$t.5[!is.na(df.s$t.5)]
df.s$t.c[!is.na(df.s$t.2)]<-df.s$t.2[!is.na(df.s$t.2)]
df.s$t.c[2]<-paste0("<text>",df.s$t.c[2])
df.s$t.c[length(df.s$pid)]<-paste0(df.s$t.c[length(df.s$pid)],'</text></body>')

m<-!is.na(df.s$t.c)
ben.ns<-paste0("benyehuda-",id.p,"-text.html")
writeLines(df.s$t.c[m],ben.ns)

#writeLines(df.s$t.c[m],"benyehuda-33373-text.html")

#}#end extract loop
