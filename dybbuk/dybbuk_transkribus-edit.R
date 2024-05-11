#20240511(08.31)
#14201.ocr-correction
#####################
# essai automated transkribus output correction of yiddish text
###############################################################
library(xml2)
flist<-list.files("~/boxHKW/21S/DH/local/EXC2020/dybbuk/Yudale_der_blinder,_Emkroyt1908-xp001/Yudale_der_blinder,_Emkroyt1908-xp001/page")
fns<-"~/boxHKW/21S/DH/local/EXC2020/dybbuk/Yudale_der_blinder,_Emkroyt1908-xp001/Yudale_der_blinder,_Emkroyt1908-xp001/page/"
tlist<-list()
tlines<-list()
for (k in 1:length(flist)){
  file<-paste0(fns,flist[k])
  f1<-read_xml(file)
  tlist[[k]]<-f1
  f1%>% xml_ns_strip()
  alltext<-xml_find_all(f1,".//TextLine")
  tlines[[k]]<-xml_text(alltext)
  
}
library(collostructions)
library(quanteda)
get.tok<-function(x)tokenize_word(x)
tok.list<-lapply(tlines, get.tok)
f1<-read_xml(paste0(fns,flist[3]))
tok.freq<-freq.list(unlist(tok.list))
tok.freq
m<-grep("דיר",tok.freq)
m<-grep("דיִר",tok.freq)
tok.sort<-tok.freq[order(tok.freq$WORD),]
tok.sort
write.csv(tok.sort,"~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_tok_freq.csv")
save(tok.sort,file = "~/Documents/GitHub/ETCRA5_dd23/dybbuk/tok.sort.RData")
f1%>% xml_ns_strip()
alltext<-xml_find_all(f1,".//TextLine")
xml_text(alltext)
#################
m<-grep("ביז",tok.sort$WORD)
tok.sort[m,] 
m<-grep("בִּיז",tok.sort$WORD)
tok.sort[m,] 
### wks., grep differenciates between tokens with/wo vocalisation
### goal: to replace not vocalised tokens in xml with manually corrected tokens. pages 1-16 are not vocalise corrected,
### pages 17-23 are corrected.
##############################


