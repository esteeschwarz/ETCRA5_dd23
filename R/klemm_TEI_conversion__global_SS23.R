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
#20230320(09.37)
#database of complete text
#wrapped elements in db
###########################
#1.abstract:
#TEI declaration of wikisource dramatext for further processing
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
src<-"https://raw.githubusercontent.com/esteeschwarz/ETCRA5_dd23/main/R/data/c0_Der_Besuch_Klemm_wsource_epub.xml"
#src<-"data/c0_Der_Besuch_Klemm_wsource_epub.xml"
d2<-read_html(src)
####all elements##
all_e<-d2 %>% 
  xml_find_all('//div/*') %>%
  xml_path() #wks. finds all div elements, including p
regx1<-"(Akt|Auftritt|Szene|Scene)"
allscenes<-grep(regx1,d2%>%xml_find_all('//div/*') %>%
         xml_text())
t1<-array()
#k<-1
for (k in 1:length(allscenes)){
  ifelse (allscenes[k+1]-allscenes[k]==1,
    t1<-append(t1,allscenes[k],after = length(t1)),f<-1)
  }
t1<-t1[2:length(t1)] #1st is NA
t1
k<-1
####
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
  tx3<-paste0("<div>",tx2,"</div>") #NO
  tx3<-tx2
   tx4[t,k]<-tx3
  }} #wks. dataframe of text along section/divs
getwd()
#write.csv(tx1,"klemmDB001.csv")
allscenes
############################################
#########
#declaration from DB source created above
#d<-read.csv("klemmDB001.csv")
s<-2 #2nd column = first scene, 
#from tx4 db created above
d<-tx4
s<-1 #first column 1 with tx4 db
#scheme of DB:
#column per scene: NA,scene,=scene,speaker,=speaker,speaker.text
#grep first content line
firstlines<-array()
#k<-2
for (k in 1:length(d)){
m<-which(!is.na(d[,k]))
firstlines[k]<-m[1]
}
firstlines<-firstlines[s:length(firstlines)]
sceneline<-firstlines
stageline<-sceneline+2
textline<-firstlines+4

for (k in s:length(firstlines)){
  d[sceneline[k],k]<-paste0('<div type="scene"><head>',d[sceneline[k],k],'<head>')
  d[sceneline[k]+1,k]<-NA
}
for (k in s:length(firstlines)){
  d[stageline[k],k]<-paste0('<stage>',d[stageline[k],k],'</stage>')
  d[stageline[k]+1,k]<-NA
}
################################
#13122.from db
#pb1<-stri_extract_all_regex(d[,2:10],"(\\[[0-9]{1,3}\\])")
#remove \n at linestart
#d<-dsf
regx<-"(^\n)"
repl<-""
#d2<-gsub(regx,repl,d)
rmn<-function(x) gsub(regx,repl,x)
#rmn<-gsub(regx,repl,d)
d2<-sapply(d[,s:length(d)], rmn)
#dsf<-d
d<-d2
#pagebreaks array
pbarray<-list()
for(k in 1:length(d[,2])){
  for (c in 1:length(d[1,])){
pb2<-grep("(\\[[0-9]{1,3}\\])",d[k,c],value = T)
if (length(pb2)!=0)
  pbarray[k]<-pb2
  }
}
#pbarray
d<-data.frame(d2)
regx<-"(\n)" 
repl<-" "
rmn<-function(x) gsub(regx,repl,x)
#rmn<-gsub(regx,repl,d)
d2<-sapply(d[,s:length(d)], rmn)
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
#pbarray #all pb rows extracted. now if pb at linestart, insert (mv) into preceding (empty, new) line
p1<-grep("(^\\[[0-9]{1,3}\\])",pbarray) #grep all [123]
#p1t<-grep("(^\\[[0-9]{1,3}\\])",pbarray,value = T)
p1t<-stri_extract_all_regex(pbarray,"(^\\[[0-9]{1,3}\\])")

pba2<-as.vector(d2)
k<-1
for (k in 1:length(p1)){
  regx<-"(^\\[[0-9]{1,3}\\] ?)"
  p2<-grep(regx,pba2)
  p2<-p1+k-1
  p3<-p2+1
  p1t2<-gsub("\\[|\\]","",p1t[[p1[k]]])
  pb4<-paste0('<pb no="',p1t2,'"/>')
  pba2<-insert(pba2,p2[k],pb4)
  
  pba2[p3]<-gsub(regx,"",pba2[p3])
}
#### wks.
###########################
###########################
## THIS >>> ###############
# df of array
pbdf<-matrix(pba2,ncol = 9)
pbdf<-data.frame(pbdf)

for (c in 1:length(pbdf[1,])){
  regx<-"^([A-Za-z]{1,10})\\. ?" #speakers (words) at linestart followed by \.
  sp1<-stri_extract_all_regex(pbdf[,c],regx)
  sp1<-unlist(unique(sp1)) #unique speakers per scene (db column)
  sp1<-sp1[!is.na(sp1)] #exclude NA
  sp1<-unlist(stri_split(sp1,regex="\\."))
  m<-grep("[A-Za-z]",sp1)
  sp1<-sp1[m]
  #############
  #THIS to declare individually in header by piece, 
  #intends to grep speaker definitions which differ (characters) from
  #declaration in scene personae or are not introduced at all according to
  #the scheme.
  #
    sp1[length(sp1)+1]<-"Bediente"  
  #############
  sp2<-paste(sp1,collapse = "|") #regex speaker array
  #find evtl. stage dir after speaker:
  #chk if char between sp2 (speaker) and \.
  sp3<-paste0("^((",sp2,"))(.+\\.)")
  m<-grep(sp3,pbdf[,c],perl = T)
  pbdf[m,c] #next: isolate speaker / stage before 1st \.
  #chk for characters between speaker and \.
  #r1<-"^((?=(Cydalise|Celimene)[^\\.]))(.+?\\.)" #YES.
  #r1<-"^((?=(Cydalise|Celimene)[^\\.]))(.+?\\.)" #groups:
  #r1<-"^(?=(Cydalise|Celimene)([^\\.])(.+?\\.))(.+?\\.)"   
  r1<-paste0("^(?=(",sp2,")([^\\.])(.+?\\.))(.+?\\.)") ##paste speaker array with regex for staged speakers
  m<-grep(r1,pbdf[,c],perl = T)
  pbdf[m,c]
  pbdf[m,c]<-gsub(r1,"<speaker>\\1</speaker><stage>\\3</stage>",pbdf[m,c],perl = T)
  sp3<-paste0("^(",sp2,")\\. ?") #paste speaker array with regex for standard speakers followed by [. ]
  repl<-"<speaker>\\1</speaker>"
  pbdf[,c]<-gsub(sp3,repl,pbdf[,c])
  r1<-"(</speaker>|</stage>)(.*)"
  m<-grep(r1,pbdf[,c],perl = T)
  pbdf[m,c]
  pbdf[m,c]<-gsub(r1,"\\1<p>\\2</p>",pbdf[m,c],perl = T)
  r1<-"^(<speaker>)"
  m<-grep(r1,pbdf[,c])
  pbdf[m,c]<-paste0("<sp>",pbdf[m,c],"</sp>")
  r1<-"(<p></p>)" #rm obsolete tagging after stage directions (scene personae)
  m<-grep(r1,pbdf[,c])
  pbdf[m,c]<-gsub(r1,"",pbdf[m,c])
  
}

getwd()
#write.csv(pbdf,"data/klemmDB002.csv")
##############################################
##############################################
