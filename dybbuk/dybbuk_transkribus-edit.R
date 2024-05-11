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
### manually assign corrected versions in .csv
library(stringi)
library(utils)
tok.ed<-read.csv("~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_tok_freq/tok.freq.list.edited-yudale_tok_freq.csv")
fns.edit<-"~/boxHKW/21S/DH/local/EXC2020/dybbuk/Yudale_der_blinder,_Emkroyt1908-xpedit001/Yudale_der_blinder,_Emkroyt1908-xp001/page/"
#sort regexes after gefräszigkeit:
tok.ed$bytes<-unlist(lapply(tok.ed$WORD,object.size))
tok.ed.s<-tok.ed[order(tok.ed$bytes,decreasing = T),]
#tok.ed.repl<-tok.ed$cor.tok[order(tok.ed$cor.tok)]
### > TODO: replace order, first lagest 
#format(object.size("a"),units="B",digits=3,standard = "SI")
#sort1<-stri_count(tok.ed$WORD,regex = ".")

for (k in 1:length(flist)){
  file<-paste0(fns.edit,flist[k])
  t1<-readLines(file)
  
  for (r in 1:length(tok.ed.s$WORD)){
    reg<-tok.ed.s$WORD[r]
    repl<-tok.ed.s$cor.tok[r]
    if(!is.na(repl)&repl!="")
       t1<-gsub(reg,repl,t1)
    cat(k,":",r,"\n")
    
    
  }
  writeLines(t1,file)
  
}
file



