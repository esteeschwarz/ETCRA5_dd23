#12231.TEI conversion essai
#20220604(10.25)
###########################
#20220619(14.49)
#finish this script. source: https://github.com/esteeschwarz/DH_essais/R/klemm_TEI_conversion_wks.R
#ongoing process: https://github.com/esteeschwarz/DH_essais/R/klemm_TEI_conversion_wks_process.R
#20220619(19.50)
#aktualisiert, status working bis auf schema integration, body complete.
#20230204(11.07)
#documented essai for global appplication on sources
###########################
#1.abstract:
#TEI declaration of wikisource dramatext for further processing
getwd()
setwd("/Users/lion/boxHKW/21S/DH")
#src<-"~/boxHKW/21S/DH/gith/DH_essais/data/corpus/klemm_besuch/klemm_TEI.xml"
#src<-"~/boxHKW/21S/DH/gith/DH_essais/data/corpus/klemm_besuch/klemm_TEI_wikiraw.xml"
#src<-"https://raw.githubusercontent.com/esteeschwarz/DH_essais/main/data/corpus/klemm_besuch/klemm(1765)_clean.txt"
##### modify for raw text:
##########################
#txt<-scan(src,"")
#library(stringi)
library(httr)
library(rvest)
library(xml2)
library(stringi)
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
setwd("~/Documents/GitHub/ETCRA5_dd23/R")
#ewa
setwd("~/Documents/GIT/ETCRA5_dd23/R")
#mini
setwd("gith/ETCRA5_dd23/R")

#dta2<-read_xml("local/EXC2020/DD23/data/c0_Der_Besuch__Klemm_.xhtml") #xml extracted from epub
dta2<-read_xml("data/c0_Der_Besuch_Klemm_wsource_epub.xml") #xml extracted from epub
#this presumes a preformatted wikisource text with speaker and scene formatting in plain html,

#not run
#runreplace<-function(){

# xpathkl<-'/html/body/section/div/'
# xml_child(xml_child(xml_child(xml_child(xml_child(dta2, 2), 1), 3), 12), 1)
# #xpath copied from browser developer tools (safari)
# html_nodes(dta2,xpath = xpathkl)

data<-dta2
#?xml_name
# xml_name(data)
# xml_name(xml_parent(data)) #keine Eltern - Wurzelelement
# xml_name(xml_children(data))
# data
# Namespace anzeigen
# xml_attr(data, "xmlns")

# Namespace entfernen

data %>% xml_ns()
data%>% xml_ns_strip()
data_sf<-data
#revert
#data<-data_sf

# Beispiel 1: xpath-Pfade anzeigen
# Alle xpath-Pfade anzeigen
# data %>% 
#   xml_find_all('//*') %>%
#   xml_path()
# #?xml_path

# Alle xpath-Pfade zu einem head-Element anzeigen
all_heads <- data %>% 
  xml_find_all('//div') %>%
  xml_path()
all_heads

all_divs <- data %>% 
  xml_find_all('//div/div')
xml_text(all_divs[1])
xml_attr(all_divs[6],"style")
xml_text(all_divs)
#<front>
#1:3 frontispiz, <bibl><title>../>
#4:7 <front><div type front><head>4:5</head><head>6:7</head></div>
#<div type "dramatis_personae">
#<castlist>
#8:<head>...</head>
#9 personen: seperate <castItem><role>...</role></castItem>
#</div></front>
#10 <div type="scene" <head> </head>
#11 <sp who="#fromspeaker id"><stage></><speaker>all_b</speaker><p>all_p</p></sp>(<sp... for each speechact)</div (scene)
#12 scene
#13 speaker ...
#28 ende
#29 signe K.

#<div <b = <div type scene <head>
#<div <i = <stage>
#<p <b = <speaker
#<p = <p
#</div> (scene)
# >>> concenate 2 div + p til next div
# : find div which has style <b>, then include following til next <div><b>
########



#all speakerlist in scene 
all_i <- data %>% 
  xml_find_all('//div/div/i')
xml_text(all_i[2])
xml_text(all_i)

#all speaker declaration of scene paragraph
all_b <- data %>% 
  xml_find_all('//div/p/b')
txtall_b<-xml_text(all_b)
#sp1<-paste0("<speaker>",txtall_b,"</speaker>")
#sp1
#xml_text(all_b)<-sp1

all_p<- data %>%
  xml_find_all('//div/p')
txtall_p<-xml_text(all_p)
txtall_p[4] #includes speaker declaration, i.e. first wrap speaker, then p
regx2<-"\n"
x2<-gsub(regx2," ",txtall_p,perl = T)
x2

regx1<-"((?<=</speaker>)(.*))"
repl1<-"<p>\\2</p>"
grep(regx1,txtall_p[4],perl = T,value = T)
x<-gsub(regx1,repl1,txtall_p[4],perl = T)
x
x<-gsub(regx1,repl1,x2,perl = T)
x[4]
x3<-paste0("<sp>",x,"</sp>")
x3[4] #all speechacts wrapped


#wks. now back insert into xml
all_p_mod<-x3
all_p2<-list(all_p_mod[1:length(all_p_mod)])
all_p_sf<-all_p
all_p[1:length(all_p)]<-all_p_mod #no
all_p<-all_p_sf
xml_replace(all_p,x3)
#chk
all_p_chk<- data %>%
  xml_find_all('//div/p')
txtall_p<-xml_text(all_p_chk)
txtall_p[4] #includes speaker declaration, i.e. first wrap speaker, then p
#bullshit to insert, have to create new xml...

all_div_b <- data %>% 
  xml_find_all('//div/div/b',flatten=T)
xml_text(all_div_b[2])
txt_all_div_b<-xml_text(all_div_b)
m<-grep("(Akt|Auftritt|Szene|Scene)",txt_all_div_b)
#get global position of each <b> to set start / end of scene
l1<-xml_path(all_div_b)
data%>%xml_find_all(l1[11])
xml_add_child(all_div_b[m[1]])
#head b divs (scene heading)
l2<-unlist(stri_extract_all(l1,regex="[0-9]{1,3}"))
xml_text(all_divs[9])
l3<-3
k<-m
data%>%xml_find_all(sprintf('//div/div[%i]',k))%>%xml_text()
scenearray<-list()
for (k in m){
  scenearray[k]<-xml_text(all_div_b[k])
}
x3 #all sp wrapped, to be put into corresponding scene
###
data%>%xml_find_all('//div/p')%>%xml_text()

all_b <- data %>% 
  xml_find_all('//div/p/b')
txtall_b<-xml_text(all_b)
sp1<-paste0("<speaker>",txtall_b,"</speaker>")
sp1
xml_text(all_b)<-sp1

####################
#reset xml
src<-"data/c0_Der_Besuch_Klemm_wsource_epub.xml"
dta2<-read_xml("data/c0_Der_Besuch_Klemm_wsource_epub.xml") #xml extracted from epub
data<-dta2
data %>% xml_ns()
data%>% xml_ns_strip()
data_sf<-data



all_p<- data %>%
  xml_find_all('//div/p')
xml_text(all_p)
txtall_p<-xml_text(all_p)
txtall_p[4] #includes speaker declaration, i.e. first wrap speaker, then p
regx2<-"\n"
all_p_n<-gsub(regx2," ",data %>%
       xml_find_all('//div/p'),perl = T)
x2<-(all_p_n)
xml_text(all_p)<-x2
###
data %>%
  xml_find_all('//div[14]')%>%xml_text()
d2<-read_html(src)
d2 %>%
  xml_find_all('//body/section/div')%>%xml_text()
all_e<-d2 %>% 
  xml_find_all('//div/*') %>%
  xml_path() #wks. finds all div elements, including p
d2 %>% 
  xml_find_all(all_e[201]) %>%
  xml_text() #wks. finds single elements text, no matter what element
#order: 22,23scene div[10]div[10]/b / 24,25 div[11]div[11]/i / 26text div/p[3] 
#i.e.: 
# 1. find first b which is act head
# 2. find next b which is next act head
regx1<-"(Akt|Auftritt|Szene|Scene)"
allscenes<-grep(regx1,d2%>%xml_find_all('//div/*') %>%
         xml_text())
t1<-array()
k<-1
for (k in 1:length(allscenes)){
  ifelse (allscenes[k+1]-allscenes[k]==1,
    t1<-append(t1,allscenes[k],after = length(t1)),f<-1)
  #levels(allscenes[k])<-2,levels(allscenes[k])<-1)
#t1[k]<-2,t1[k]<-1)
  }
t1<-t1[2:length(t1)] #excluded instances where the regex appears in text
t1
k<-1
d2 %>% 
  xml_find_all(all_e[t1[1]]) %>%
  xml_text() #wks. all scene headings
tx4<-data.frame()

for(k in 1:length(t1)){
  row0<-t1[k]
  row<-t1[k]+1
  ifelse (k<length(t1),lastrow<-t1[k+1]-1,lastrow<-length(all_e))
  rows<-row0:lastrow
  for(t in rows){
    
  #if (allscenes[k] < allscenes[k+1])
  tx2<-d2 %>% 
    xml_find_all(all_e[t]) %>%
    xml_text()
  tx3<-paste0("<div>",tx2,"</div>")
  tx4[t,k]<-tx3
  }} #wks. dataframe of text along section/divs
getwd()
write.csv(tx1,"klemmDB001.csv")
# #levels(allscenes)
# t2<-(drop(array(allscenes,dim=t1)))
# t2
allscenes

regx1<-"((?<=</speaker>)(.*))"
repl1<-"<p>\\2</p>"
txtall_p<-xml_text(all_p)
grep(regx1,txtall_p[4],perl = T,value = T)
x<-gsub(regx1,repl1,txtall_p[4],perl = T)
x
x<-gsub(regx1,repl1,x2,perl = T)
x[4]
x3<-paste0("<sp>",x,"</sp>")
x3[4] #all speechacts wrapped

############################################
# 1.wrap speaker, scene, scenespeaker
# del \n

#txt<-html_nodes(dta1,xpath = xpathkl) %>%html_text()
xml_find_all(dta2,xpathkl)
#library(stringi)
library(clipr)
#library(xml2)
library(stringr)
#txt
#set<-txt
#########
# declaration from DB source created above
d<-read.csv("klemmDB001.csv")
#scheme of DB:
#column per scene: NA,scene,=scene,speaker,=speaker,speaker.text
#grep first content line
firstlines<-array()
#k<-2
for (k in 1:length(d)){
m<-which(!is.na(d[,k]))
firstlines[k]<-m[1]
}
firstlines<-firstlines[2:length(firstlines)]
sceneline<-firstlines
#t_scene<-d$V1[sceneline[1]]

stageline<-sceneline+2
#t_speaker<-d$V1[speakerline[1]]
#textline<-speakerline+4
#w_scene<-function(k){paste0('<div type="scene"><head>',d[sceneline[k]],'<head>')}
# d$V1[sceneline[1]]<-w_scene(1)
for (k in 1:length(firstlines)){
  d[sceneline[k],k+1]<-paste0('<div type="scene"><head>',d[sceneline[k],k+1],'<head>')
  d[sceneline[k]+1,k+1]<-NA
}
for (k in 1:length(firstlines)){
  d[stageline[k],k+1]<-paste0('<stage>',d[stageline[k],k+1],'</stage>')
  d[stageline[k]+1,k+1]<-NA
  }
# all_e<-d2 %>% 
#   xml_find_all('//div/*')
# all_e_p<-d2 %>% 
#   xml_find_all('//div/*') %>%
#   xml_path() #wks. finds all div elements, including p
# d2 %>% 
#   xml_find_all(all_e[201]) %>%
#   xml_text() #wks. finds single elements text, no matter what element
# #order: 22,23scene div[10]div[10]/b / 24,25 div[11]div[11]/i / 26text div/p[3] 
# library(rvest)
# xml_tag(all_e[sceneline[1]])->"head"
# html_name(all_e[sceneline[1]])<-"head"
# xml_attr(all_e[sceneline[1]],"style")<-""
#attributes(all_e[sceneline[1]])
################################
#13122.from db
#pb1<-stri_extract_all_regex(d[,2:10],"(\\[[0-9]{1,3}\\])")
#remove \n
#restore
#d<-dsf
regx<-"(^\n)"
repl<-""
#d2<-gsub(regx,repl,d)
rmn<-function(x) gsub(regx,repl,x)
#rmn<-gsub(regx,repl,d)
d2<-sapply(d[,2:10], rmn)
#dsf<-d
d<-d2
pbarray<-list()
for(k in 1:length(d[,2])){
  for (c in 1:length(d[1,])){
pb2<-grep("(\\[[0-9]{1,3}\\])",d[k,c],value = T)
if (length(pb2)!=0)
  pbarray[k]<-pb2
  }
}
pbarray
d<-data.frame(d2)
regx<-"(\n)"
repl<-" "
rmn<-function(x) gsub(regx,repl,x)
#rmn<-gsub(regx,repl,d)
d2<-sapply(d[,1:9], rmn)
dsf<-d
#d<-d2
d<-data.frame(d2)
pbarray<-matrix(nrow = length(d[,2]), ncol = length(d[1,]))
for(k in 1:length(d[,2])){
  for (c in 1:length(d[1,])){
    pb2<-grep("(\\[[0-9]{1,3}\\])",d[k,c],value = T)
    if (length(pb2)!=0)
      pbarray[k,c]<-pb2
  }
}
# d<-data.frame(d2)
# pbarray<-list()
# for(k in 1:length(d[,2])){
#   for (c in 1:length(d[1,])){
#     pb2<-grep("(\\[[0-9]{1,3}\\])",d[k,c],value = T)
#     if (length(pb2)!=0)
#       pbarray[[c]]<-pb2
#   }
# }

pbarray #all pb rows extracted. now if pb at linestart, insert (mv) into preceding (empty, new) line
library(R.utils)
p1<-grep("(^\\[[0-9]{1,3}\\])",pbarray)
#p1t<-grep("(^\\[[0-9]{1,3}\\])",pbarray,value = T)
p1t<-stri_extract_all_regex(pbarray,"(^\\[[0-9]{1,3}\\])")

pba2<-as.vector(d2)
k<-1
for (k in 1:length(p1)){
  regx<-"(^\\[[0-9]{1,3}\\] ?)"
  p2<-grep(regx,pba2)
  p2<-p1+k-1
  
    #p1t<-grep("(^\\[[0-9]{1,3}\\])",pb2,value = T)
#  p1t<-stri_extract_all_regex(pba2,"(^\\[[0-9]{1,3}\\])")
  p3<-p2+1
  p1t2<-gsub("\\[|\\]","",p1t[[p1[k]]])
  pb4<-paste0('<pb no="',p1t2,'"/>')
  pba2<-insert(pba2,p2[k],pb4)
  
  pba2[p3]<-gsub(regx,"",pba2[p3])
}
pba2[p1[4]]
p1
p1t
pba2
#### wks.