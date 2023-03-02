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
src<-"https://de.wikisource.org/wiki/Der_Besuch_(Klemm)"
dta1<-read_html(src)
xpathkl<-'//*[@id="mw-content-text"]/div[1]/div[2]'
#xpath copied from browser developer tools (safari)
html_nodes(dta1,xpath = xpathkl)
txt<-html_nodes(dta1,xpath = xpathkl) %>%html_text()
#gets plain text
#wks.
#now with epub formatted text:
# src<-"https://ws-export.wmcloud.org/?format=epub&lang=de&page=Der_Besuch_(Klemm)"
# dta2<-read_html(src) #no
# x<-GET(src) #no
# dta2<-content(x,"text")
getwd()
setwd("~/Documents/GIT/ETCRA5_dd23/R")
#dta2<-read_xml("local/EXC2020/DD23/data/c0_Der_Besuch__Klemm_.xhtml") #xml extracted from epub
dta2<-read_xml("data/c0_Der_Besuch_Klemm_wsource_epub.xml") #xml extracted from epub
#this presumes a preformatted wikisource text with speaker and scene formatting in plain html,

runreplace<-function(){

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
#11 <sp who="#fromspeaker id"><stage></><speaker>all_b</speaker><p>all_p</p></sp>
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
sp1<-paste0("<speaker>",txtall_b,"</speaker>")
sp1
xml_text(all_b)<-sp1

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
x3[3] #all speechacts wrapped
}

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
data%>%xml_find_all(l1[10])
xml_add_child(all_div_b[m[1]])
l2<-unlist(stri_extract_all(l1,regex="[0-9]{1,3}"))

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
