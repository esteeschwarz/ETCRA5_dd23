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


notrun<-function(){
#ids<-bencsv$TextID
#head(ids)

#now meta:
bensrc<-"https://github.com/dracor-org/hedracor/raw/main/preparatory_data/benyehuda.drama.29112022.tsv" #404,not public
bensrc<-"~/Documents/lade/benyehuda.drama.29112022.tsv"
# bensrc<-"~/boxHKW/21S/DH/local/EXC2020/DD23/data/benyehuda.drama.csv"
# bencsv<-read.csv(bensrc)
# ids<-bencsv$TextID



m<-match("33373",bencsv$TextID) #doesnt exist in db
temphtml<-tempfile()
writeLines(bencsv$html[m],temphtml)
d2<-read_html(temphtml)
writeLines(bencsv$html[m],"benyehuda-33373-temp.html")
getwd()
#paste0("act",1,2,3,"nr",paste0(1:4,collapse = ""),collapse = "")
################ this is archived >
# regx1<-"(Akt|Auftritt|Szene|Scene)"
# allscenes<-grep(regx1,d2%>%xml_find_all('//div/*') %>%
#                   xml_text())
#### 13131.
# find lines to change in divs:
#all_sc_b



r_b_head<-r_b[7:15]
firstlines<-r_b_head #all scene head lines
r_i_sp<-r_i[2:10] 
stageline<-r_i_sp #all scene speaker stage lines
r_p_txt<-r_p[3:length(r_p)]
textline<-r_p_txt
#t1<-array()
#k<-1
#chk length:
chkl<-length(r_b)+length(r_i)+length(r_p)
chkl<-length(firstlines)+length(stageline)+length(textline)
#########
k<-1

df_scenes<-data.frame(row=1:length(all_e))
df_scenes$cpt<-d2 %>% xml_find_all('//div/*') %>%xml_text()

regx<-"(^\n)"
repl<-""
rmn<-function(x) gsub(regx,repl,x)
df_scenes$cpt<-sapply(df_scenes$cpt,rmn)

d<-df_scenes

regx<-"(\n)" 
repl<-" "
rmn<-function(x) gsub(regx,repl,x)
#rmn<-gsub(regx,repl,d)
d$cpt<-sapply(d$cpt, rmn)
#### 2
 pba2<-d$cpt
# k<-1
##############
# move <pb> to preceding line
pba3<-array()
for (k in 2:length(pba2)){
  regx<-"(^\\[[0-9]{1,3}\\] ?)"
  p2<-grep(regx,pba2)
  regx<-"(^\\[[0-9]{1,3}\\] ?)"
  p1t<-stri_extract_all_regex(pba2[k],regx,simplify = T)
  pba3[k]<-pba2[k]
  
  if (!is.na(p1t)&pba2[k-1]!=""){
    pba3[k-1]<-paste0(pba2[k-1]," ",p1t)
  }
  if (!is.na(p1t)&pba2[k-1]==""){
    pba3[k-2]<-paste0(pba2[k-2]," ",p1t)
  }  
  pba3[k]<-gsub(regx,"",pba2[k])
  
}
####  
pba3
df_scenes$pb<-pba3
##################
d4<-data.frame(cpt=pba3)
 for(k in 1:length(d4[,1])){
  d4$cpt[k]<-gsub("\\[([0-9]{1,3})\\]",('<pb no="\\1"/>'),d4$cpt[k])
}
df_scenes$pb2<-d4$cpt
#####
#pba2
getwd()
#write.csv(tx1,"klemmDB001.csv")
############################################
#########
#declaration from DB source created above
#d<-read.csv("klemmDB001.csv")
#scheme of DB:
#column per scene: NA,scene,=scene,speaker,=speaker,speaker.text
#grep first content line
r_b_head
firstlines
stageline
textline

###
for (k in 1:length(r_b_head)){
  r0_head<-r_b_head[k]
  r0_stage<-r_i_sp[k]
  r0_text<-r0_stage+1
  r1_text<-r_b_head[k+1]-3
  c<-k+length(df_scenes)
  c<-length(df_scenes)+1
  df_scenes[r0_head,"pb2"]<-paste0('<div type="scene"><head>',df_scenes[r0_head,"pb2"],'</head>')
  
  df_scenes[r0_stage,"pb2"]<-paste0('<stage>',df_scenes[r0_stage,"pb2"],'</stage>')
}
################################
#### wks.
###########################
###########################
## THIS >>> ###############
# df of array
# pbdf<-matrix(pba2,ncol = 9)
# pbdf<-data.frame(pbdf)
pbdf<-df_scenes
a<-unlist(stri_split_boundaries(pbdf$cpt[stageline],type="word"))
a
b<-grep("[A-Z]",a)
c<-unique(a[b])
d<-c(c[c(1,2,3,4,5,7,9)])
speakerarray<-d
  #############
  #THIS to declare individually in header by piece, 
  #intends to grep speaker definitions which differ (characters) from
  #declaration in scene personae or are not introduced at all according to
  #the scheme.
  #
  sp1<-d
  sp1[length(sp1)+1]<-"Bediente"  
  #############
  sp2<-paste(sp1,collapse = "|") #regex speaker array
  #find evtl. stage dir after speaker:
  #chk if char between sp2 (speaker) and \.
  sp3<-paste0("^((",sp2,"))(.+\\.)")
  sp3
  speakerregex<-sp2
  m<-grep(sp3,pbdf$pb2,perl = T)
  #pbdf$pb2[m] #next: isolate speaker / stage before 1st \.
  #chk for characters between speaker and \.
  #regexr: ^(?=(Celimene|Finette|Chlorinde|Cydalise|Damis|Erast|Bedienter|Bediente)([^\.])(.+?\.))(.+?\.)(.*)
  r1<-paste0("^(?=(",sp2,")([^\\.])(.+?\\.))(.+?\\.)(.*)") ##paste speaker array with regex for staged speakers
  r1
  temp_a<-array(pbdf$pb2)
  temp_d<-1:length(temp_a)
  temp_d[textline]<-TRUE
  temp_d[temp_d!=T]<-F
  mode(temp_d)<-"logical"
  m<-grep(r1,pbdf$pb2,perl = T)
  m2<-textline%in%m
  pbdf$pb2[textline[m2]]
  m<-textline[m2]
  pbdf$pb2[m]<-gsub(r1,'<sp who="#\\1"><speaker>\\1</speaker><stage>\\3</stage><p>\\5</p></sp>',pbdf$pb2[m],perl = T)
  sp3<-paste0("^(",sp2,")(\\. ?)(.*)") #paste speaker array with regex for standard speakers followed by [. ]
  repl<-'<sp who="#\\1"><speaker>\\1</speaker><p>\\3</p></sp>'
  ###########
  pbdf$pb2[textline]<-gsub(sp3,repl,pbdf$pb2[textline])
  
  
  # r1<-"(</stage>)(.*)"
  # m<-grep(r1,pbdf$pb2,perl = T)
  # #pbdf$pb2[m]
  # pbdf$pb2[m]<-gsub(r1,"\\1<p>\\2</p>",pbdf$pb2[m],perl = T)
  # r1<-"(</speaker>)(.*)[^stage]"
  # m<-grep(r1,pbdf$pb2,perl = T)
  # pbdf$pb2[m]
  # pbdf$pb2[m]<-gsub(r1,"\\1<p>\\2</p></sp>",pbdf$pb2[m],perl = T)
  # r1<-"^(<speaker>)"
  # m<-grep(r1,pbdf$pb2)
  # pbdf$pb2[m]<-paste0("<sp>",pbdf$pb2[m],"</sp>")
  # r1<-"(<p></p>)" #rm obsolete tagging after stage directions (scene personae)
  # m<-grep(r1,pbdf$pb2)
  # pbdf$pb2[m]<-gsub(r1,"",pbdf$pb2[m])
  # # r1<-'(#[A-Z].+?)(?=")'
  # m1<-grep(r1,pbdf$pb2)
  # ex1<-stri_split_regex(stri_extract_all_regex(pbdf$pb2,pattern=r1,simplify = T),"#",simplify = T)
  # ex2<-decapitalize(ex1[,2])
  # ex3<-unique(ex2)
  # ex3<-ex3[ex3!=""]
  # ex3
  sp2
  spsmall<-decapitalize(speakerarray)
  ex3<-spsmall
  
  k<-1
  for (k in 1:length(speakerarray)){
    r1<-paste0('<sp who="#(',speakerarray[k],')">')
    m<-grep(r1,pbdf$pb2)
    repl<-paste0('<sp who="#',ex3[k],'">')
    pbdf$pb2[m]<-gsub(r1,repl,pbdf$pb2[m])        
  }
  #ex2
##############################################
##############################################
#close wrap scene <div
m<-grep("<",pbdf$pb2)
pbdf$tei1<-NA
pbdf$tei1[m]<-pbdf$pb2[m]
mna<-is.na(pbdf$tei1)
#mna
mna1<-mna-1
#mna1<-mna1*-1
#mna2<-!is.na(pbdf$tei1)
#m2<-grepl("<sp>",pbdf$tei1)
#mna3<-mna2-1
#mna3
#mna4<-mna3!=0
#mna4
#mna5<-mna2==m2
#mna5
#pbdf$chk<-mna5
#exp: if <sp> line is followed by NA line, close </div> at the end after </sp>
#or: assemble scenes
#m3<-grep("<sp>",pbdf$tei1[mna-1])
#m3
m4<-grep(0,mna1)
m4
m5<-grep("<sp",pbdf$tei1[m4-1])
pbdf$tei1[m4-1][m5]<-paste0(pbdf$tei1[m4-1][m5],"</div>")
# m6<-grep(-1,mna1) #last <sp>text
# pbdf$tei1[m6[length(m6)]]<-paste0(pbdf$tei1[m6[length(m6)]],"</div>")
###########
#TODO: book head, front, last lines

getwd()
wdir<-"/Users/guhl/Documents/GitHub/ETCRA5_dd23/R"
write.csv(pbdf,paste(wdir,"data","klemmDB002b.csv",sep = "/"))
writeLines(pbdf$tei1[m6],paste(wdir,"data","klemmDB002b.html",sep = "/"))

}
