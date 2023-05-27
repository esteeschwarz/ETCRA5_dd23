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
#lapsi
dev<-"/Users/guhl/Documents/GitHub/"
#ewa
dev<-"/Users/guhl/Documents/GIT/"
#mini
#dev<-"gith"

####################
#src<-"https://raw.githubusercontent.com/esteeschwarz/ETCRA5_dd23/main/R/data/c0_Der_Besuch_Klemm_wsource_epub.xml"
src<-"https://raw.githubusercontent.com/esteeschwarz/ETCRA5_dd23/main/R/data/c0_Blind_geladen_wsource_epub.xml"
src<-"https://raw.githubusercontent.com/esteeschwarz/ETCRA5_dd23/main/R/data/c0_Polytimet.xml"
srcdr<-"https://raw.githubusercontent.com/esteeschwarz/ETCRA5_dd23/main/R/data/gerXXX-kotzebue-blindgeladen.tei_onlyheader.xml"
#srcdr<-"https://raw.githubusercontent.com/esteeschwarz/ETCRA5_dd23/main/R/data/gerXXX-kotzebue-blindgeladen.tei_onlyheader.xml"

#src<-"data/c0_Der_Besuch_Klemm_wsource_epub.xml"
d2<-read_html(src)
d3<-d2
dramans<-"klemm"
dramans<-"blindgeladen"
dramans<-"polytimet"
####all elements##
all_e<-d2 %>% 
  xml_find_all('//div/*') %>%
  xml_path() #wks. finds all div elements, including p
# regx1<-"(Akt|Auftritt|Szene|Scene)"
# allscenes<-grep(regx1,d2%>%xml_find_all('//div/*') %>%
#                   xml_text())
#### 13131.
# find lines to change in divs:
#all_sc_b
all_e
d2 %>% 
  xml_find_all(all_e[23]) %>%
  xml_text() #zweyter auftritt
regx<-"\\]/b"
r_b<-d2 %>% 
  xml_find_all('//div/*') %>% xml_path()%>% 
  grep(pattern=regx)
regx<-"\\]/i"
r_i<-d2 %>% 
  xml_find_all('//div/*') %>% xml_path()%>% 
  grep(pattern=regx) #all scene speaker personae
r_i_stage<-d2 %>% 
  xml_find_all('//div/p/i') %>% xml_path() #
#/html/body/section/div/p[98]/i
d2 %>% 
  xml_find_all(r_i_stage[2]) %>% xml_text()
regx<-"/p"
r_p<-d2 %>% 
  xml_find_all('//div/*') %>% xml_path()%>% 
  grep(pattern=regx)
r_t<-d2%>%xml_find_all('//div/*')%>%xml_attr("style")
r_t_s<-grep(pattern="text-align",r_t)

###13191.kotzebue scene declaration:
regx<-"\\]/span/b"
regx<-"\\]/b"
#13215.polytimet
regx<-"/div/div\\[[0-9]{1,3}\\]" #NO. greps too much, maybe not grep over xmlpath but text in df.
r_b<-d2 %>% 
  xml_find_all('//div/*') %>% xml_path()%>% 
  grep(pattern=regx)

r_b<-d2 %>% 
  xml_find_all('//div/*') %>% xml_path()%>% 
  grep(pattern=regx)


all_e
#xml_text(all_e)
#xml_find_all('//div/*')
all_e[r_b]
# d2 %>% 
#   xml_find_all('//div/*')[r_b]
# d2 %>% 
#   xml_find_all('//div/*')
 d2 %>% 
   xml_find_all(all_e[r_b[26]]) %>% xml_text()
# d2 %>% 
#   xml_find_all(all_e[r_b_head[10]]) %>% xml_text()
 d2 %>%
   xml_find_all('//i') %>% xml_text()
 r_i2<-d2 %>%
   xml_find_all('//i')
 d2 %>%
  xml_find_all(all_e[r_i[24]]) %>% xml_text()
# d2 %>% 
#   xml_find_all(all_e[r_p[161]]) %>% xml_text()
# d2 %>% 
#   xml_find_all(all_e[r_t_s[7]]) %>% xml_text()
# 
# firstlines_2
# r_b[6]
# r_b
#adapt:
 r_b_head<-r_b[4:26]
 firstlines<-r_b_head #all scene head lines
 r_i_sp<-r_i[2:24]
 ##all stage speaker
 #m<-grep("(",)
 r_i
# r_b_head<-r_b[7:15]
# firstlines<-r_b_head #all scene head lines
# r_i_sp<-r_i[2:10]

stageline<-r_i_sp #all scene speaker stage lines
r_p_txt<-r_p[6:length(r_p)]
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
regx<-"\\[Ξ\\]"
repl<-""
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
dfsafe<-df_scenes
############################################
#########
#declaration from DB source created above
#d<-read.csv("klemmDB001.csv")
#scheme of DB:
#column per scene: NA,scene,=scene,speaker,=speaker,speaker.text
#grep first content line
r_b_head
#polytimet:
firstlines<-grep("Auftritt",d4$cpt)
r_b_head<-firstlines
#stageline<-c(13,23,stageline)
stageline<-c(13,18,25,40,61,82,119,136)
r_i_sp<-c(23,38,59,80,117,134)
r_i_sp<-stageline
textline
k
###
length(r_i_sp)
for (k in 1:length(r_b_head)){
  r0_head<-r_b_head[k]
  r0_stage<-r_i[k]
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
#klemm
# a<-unlist(stri_split_boundaries(pbdf$cpt[stageline],type="word"))
# a
# b<-grep("[A-Z]",a)
# c<-unique(a[b])
# # d<-c(c[c(1,2,3,4,5,7,9)]) #klemm
# d<-c(c[c(2,3,4,5,7)]) #kotzebue
a<-pbdf$cpt[stageline]
a
b<-grep("[A-Z]",a)
c<-unique(a[b])
# d<-c(c[c(1,2,3,4,5,7,9)]) #klemm
d<-c(c[c(2,3,4,5,7)]) #kotzebue
d<-c("Baron","Blum","Krips","Zauser","Thal","Wilhelmine","Michel")
d<-c("Polytimet","Aridäus","Polemon","Aristodem","Strato","Parmenio","Heerold")

speakerarray<-d
d
  #############
  #THIS to declare individually in header by piece, 
  #intends to grep speaker definitions which differ (characters) from
  #declaration in scene personae or are not introduced at all according to
  #the scheme.
  #
  sp1<-d
### 13191.kotzebue 
# sp1[length(sp1)+1]<-"Bediente"  
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
  sp2
  #manual corrections, declared in head
  # for(k in 1:length(coray$gsub)){
  #   
  # }
  #missing speaker declaration in transcript
 # pbdf$pb2[219]<-paste0("Krips. ",pbdf$pb2[219])
  #speaker thal+blum, L77 <beide>
  #sp1[8]<-"Beide"
  sp2<-paste(sp1,collapse = "|") #regex speaker array
  r1<-paste0("^(?=(",sp2,")([^\\.])(.+?\\.))(.+?\\.)(.*)") ##paste speaker array with regex for staged speakers
  r1
  sp2
  # #front pages
  # #3,8,12,14,17,20
  # pbdf$man<-""
  # pbdf$man[8]<-paste0('<front><div type="front"><head>',
  #                     pbdf$pb[3],'</head><head>',pbdf$pb[8],'</head></div>')
  # #castlist scheme
  # # <div type="Dramatis_Personae">
  # #   <castList>
  # #   <head>Personen.</head>
  # #   <castItem>
  # #   <role>Baron Eschenholz</role>
  # #   <desc>, ein Landedelmann.</desc>
  # #   </castItem>
  # drperson<-stri_split_regex(pbdf$pb[14],pattern="\\.",simplify = T)
  # drperson_d<-stri_split_regex(drperson,pattern = ",",simplify = T)
  # drperson_d[,1]<-gsub("^ ","",drperson_d[,1])
  # drperson_d[,2]<-gsub("^ ","",drperson_d[,2])
  # drperson_d<-drperson_d[1:7,]
  # drperson_d[,1]<-paste0('<role>',drperson_d[,1],'</role>')
  # drperson_d[,2]<-paste0('<desc>',drperson_d[,2],'</desc>')
  # drperson_c<-paste0('<castItem>',drperson_d[,1],drperson_d[,2],'</castItem')
  # drperson_e<-stri_join(drperson_c,collapse = "")
  #   pbdf$man[12]<-paste0('<div type="Dramatis Personae"><castList><head>',
  #                     pbdf$pb[12],'</head>',drperson_e,'</div>')
  # #stage
  #   pbdf$man[17]<-paste0('<div type="stage">',pbdf$cpt[17],'</div>')
  #   pbdf$man[20]<-paste0('<div type="anmerkung">',pbdf$cpt[20],'</div>')
  # #close <front>
  #   pbdf$man[22]<-"</front><body>"
  ##############################################
  temp_a<-array(pbdf$pb2)
  temp_d<-1:length(temp_a)
  temp_d[textline]<-TRUE
  temp_d[temp_d!=T]<-F
  mode(temp_d)<-"logical"
  m<-grep(r1,pbdf$pb2,perl = T)
  m2<-textline%in%m
  pbdf$pb2[textline[m2]]
  m<-textline[m2]
  #critical, NO
 # pbdf$pb2[m]<-gsub(r1,'<sp who="#\\1"><speaker>\\1</speaker><stage>\\3</stage><p>\\5</p></sp>',pbdf$pb2[m],perl = T)
  sp3<-paste0("^(",sp2,")(\\. ?)(.*)") #paste speaker array with regex for standard speakers followed by [. ]
  repl<-'<sp who="#\\1"><speaker>\\1</speaker><p>\\3</p></sp>'
  ###########
 # pbdf$pb2[textline]<-gsub(sp3,repl,pbdf$pb2[textline])
#### polytimet: speaker line is extra line, not within <p>
  r1
  sp2
  k<-1
  sp4<-paste0(d,".")
  pbdf$sp4<-NA
  for (k in 1:length(sp4)){
    sp<-sp4[k]
   # m<-k==pbdf$pb2
    k
    
    m<-which(sp==pbdf$pb2)
    
    ssm<-decapitalize(d[k])
    
    #mt<-m+1
    tx<-pbdf$pb2[m+1]
    pbdf[m,"sp4"]<-paste0('<sp who="#',ssm,'"><speaker>',sp,'</speaker><p>',tx,'</p></sp>')
  }
  stinst<-r_b_head[1]+2
  pbdf$sp4[1:stinst]<-NA
  m
  k
  #m<-match
  #front pages
  #3,8,12,14,17,20
  pbdf$man<-""
  pbdf$man[5]<-paste0('<front><div type="front"><head>',
                      pbdf$pb2[3],'</head><head>',pbdf$pb2[5],'</head></div>')
  #castlist scheme
  # <div type="Dramatis_Personae">
  #   <castList>
  #   <head>Personen.</head>
  #   <castItem>
  #   <role>Baron Eschenholz</role>
  #   <desc>, ein Landedelmann.</desc>
  #   </castItem>
  drperson<-stri_split_regex(pbdf$pb2[12],pattern="\\.",simplify = T)
  drperson_d<-stri_split_regex(drperson,pattern = ",",simplify = T)
  drperson_d[,1]<-gsub("^ ","",drperson_d[,1])
  drperson_d[,2]<-gsub("^ ","",drperson_d[,2])
  drperson_d[3,2]<-paste(drperson_d[3,2],drperson_d[3,3],sep = ",")
  drperson_d<-drperson_d[1:6,]
  drperson_d[,1]<-paste0('<role>',drperson_d[,1],'</role>')
  drperson_d[,2]<-paste0('<roleDesc>',drperson_d[,2],'</roleDesc>')
  drperson_c<-paste0('<castItem>',drperson_d[,1],drperson_d[,2],'</castItem>')
  drperson_e<-stri_join(drperson_c,collapse = "")
  pbdf$man[10]<-paste0('<div type="Dramatis Personae"><castList><head>',
                       pbdf$pb2[10],'</head>',drperson_e,'</castList></div>')
  #stage
  # pbdf$man[17]<-paste0('<div type="stage">',pbdf$pb2[17],'</div>')
  # pbdf$man[20]<-paste0('<div type="anmerkung">',pbdf$pb2[20],'</div>')
  # #close <front>
  pbdf$man[14]<-"</front><body>"
  pb1<-stri_extract(pbdf$cpt[1],regex="[0-9]{1,3}")
  pbdf$man[1]<-paste0('<pb no="',pb1,'"/>')
  #m<-grep("Censur",pbdf$pb2)
  #pbdf$man[m]<-paste0("<trailer>",pbdf$pb2[m],"</trailer></body>")
  #pbdf$man[538]<-paste0("<stage>",pbdf$pb2[538],"</stage>")
  #pbdf$man[146]<-paste0(stri_extract(pbdf$pb2[146],regex="<.*"),"</div>")
  #pbdf$man[153]<-paste0('<sp who="#zauser"><speaker>Zauser</speaker><p>',pbdf$pb2[153],'</p></sp>')
  #pbdf$man[499]<-paste0('<stage>',pbdf$pb2[499],'</stage>')
  #pbdf$man[528]<-paste0('<stage>',pbdf$pb2[528],'</stage>')
  
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
  # for (k in 1:length(speakerarray)){
  #   r1<-paste0('<sp who="#(',speakerarray[k],')">')
  #   m<-grep(r1,pbdf$pb2)
  #   repl<-paste0('<sp who="#',ex3[k],'">')
  #   pbdf$pb2[m]<-gsub(r1,repl,pbdf$pb2[m])        
  # }
  # #ex2
##############################################
##############################################
#close wrap scene <div
m<-grep("^<",pbdf$pb2)
pbdf$tei1<-NA
pbdf$tei1[m]<-pbdf$pb2[m]
m<-grep("^<",pbdf$sp4)
#pbdf$tei1<-NA
pbdf$tei1[m]<-pbdf$sp4[m]

#TODO: book head, front, last lines
m<-grep("<",pbdf$man)
pbdf$tei1[m]<-pbdf$man[m]

m<-grep("<div",pbdf$tei1)
m
m2<-m>r_b_head[1]
pbdf$tei1[m[m2]]<-paste0("</div>",pbdf$tei1[m[m2]])
m<-grep("^<",pbdf$tei1)
m<-m[length(m)]
pbdf$tei1[m]<-paste0(pbdf$tei1[m],"</div></body>")

 
mna<-is.na(pbdf$tei1)
mna
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
# m4<-grep(0,mna1)
# mna1
# m4
# m5<-grep("<sp",pbdf$tei1[m4-1])
# m5 #23 scenes
# m41<-m4-1
# pbdf$tei1[m41][m5]
#CRITICAL
# pbdf$tei1[m4-1][m5]<-paste0(pbdf$tei1[m4-1][m5],"</div>")
 m6<-grep(-1,mna1) #valid lines
 m6
 m6<-!is.na(pbdf$tei1)
# pbdf$tei1[m6[length(m6)]]<-paste0(pbdf$tei1[m6[length(m6)]],"</div>")
###########
getwd()
wdir<-"/Users/guhl/Documents/GitHub/ETCRA5_dd23/R"
wdir<-paste(dev,"ETCRA5_dd23/R",sep = "/")
write.csv(pbdf,paste(wdir,"data",paste0(dramans,"DB002b.csv"),sep = "/"))
dramawrite<-paste0(dramans,"DB002b.html")
outns<-paste(wdir,"data",dramawrite,sep = "/")
dramafinal<-paste0(dramans,"_staged_TEI_final.xml")
finaloutns<-paste(wdir,"data",dramafinal,sep = "/")
writeLines(pbdf$tei1[m6],outns)

## stray <div>: Baron... erleichtern</div> (Beide. wodurch)
########wks.
#integrate into dracor scheme:
getwd()
#src<-"~/Documents/GitHub/ETCRA5_dd23/R/data/gerXXX-kotzebue-blindgeladen.tei_onlyheader.xml"
# d5<-read_xml("~/Documents/GitHub/ETCRA5_dd23/R/data/gerXXX-kotzebue-blindgeladen.tei_onlyheader.xml")
# d5%>% xml_ns_strip()
# xml_add_child(d5,"text")
# d5txt<-as.list(pbdf$tei1[m6])
# d5["text"]<-d5txt
# xml_add_child(d5,d5txt)
# d5t<-d5%>%xml_find_all("//text")
# d5addtxt<-function(what)xml_add_child(d5t,what)
# lapply(d5t, d5addtxt)
# codef<-function(x) stri_extract_all_regex(x,"(#[A-Z]{3})")
# ms7<-lapply(sent1, codef)
# for(k in d5txt){
#   xml_add_child(d5t,k)
# }
# d6txt<-c(d5$doc,d5txt)
# d5text<-unlist(d5$node)
d6<-readLines(srcdr)
m<-grep("</standOff>",d6)
library(R.utils)
d7<-insert(d6,m+1,"<text>")
d8<-insert(d7,m+2,pbdf$tei1[m6])
m<-length(d8)-1
d8[m]<-"</text>"
outns
#writeLines(d8,paste(wdir,"data","gerXXX-kotzebue-blindgeladen.tei_Rcombined.xml",sep = "/"))
writeLines(d8,paste(wdir,"data","gerXXX-polytimet.tei_Rcombined.xml",sep = "/"))

#systemprep<-paste0('xmlformat --config-file=/Users/guhl/Documents/GitHub/ezdrama/format.conf "',outns,'" > "',finaloutns,'"')
system('xmlformat --config-file=/Users/guhl/Documents/GitHub/ezdrama/format.conf "/Users/guhl/Documents/GitHub/ETCRA5_dd23/R/data/gerXXX-polytimet.tei_Rcombined.xml" > "/Users/guhl/Documents/GitHub/ETCRA5_dd23/R/data/gerXXX-polytimet_staged_tei_final.xml"')
#writeLines(d8,"gerXXX-kotzebue-blindgeladen.tei_Rcombined.xml")

getwd()
# use DF, tag according to ezdrama notebook
write.csv(dfsafe,paste(wdir,"data",paste0(dramans,"DB001b.csv"),sep = "/"))
ezd<-read.csv("data/polytimetDB001b_ezdrama.csv",sep = ";")
ezd$ezcpt<-paste0(ezd$ezd,ezd$pb2)
m<-!is.na(ezd$ezcpt)
writeLines(ezd$ezcpt[m],paste(wdir,"data","polytimet_ezd_.txt",sep = "/"))

