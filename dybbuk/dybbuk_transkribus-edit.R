#20240511(08.31)
#14201.ocr-correction
#####################
# essai automated transkribus output correction of yiddish text
###############################################################
#flist<-list.files("~/boxHKW/21S/DH/local/EXC2020/dybbuk/Yudale_der_blinder,_Emkroyt1908-xp001/Yudale_der_blinder,_Emkroyt1908-xp001/page")
mini<-"/Volumes/EXT"
lapsi<-"/Users/guhl/Documents"
fns.base.1<-"/boxHKW/21S/DH/local/EXC2020/dybbuk/"
minichk<-list.files(mini)
#fns.base<-paste0(lapsi,fns.base)
#fns<-"/boxHKW/21S/DH/local/EXC2020/dybbuk/Yudale_der_blinder,_Emkroyt1908-xp001/Yudale_der_blinder,_Emkroyt1908-xp001/page/"
if(length(minichk)>0)
  fns.base.2<-paste0(mini,fns.base.1)
if(length(minichk)<1)
  fns.base.2<-paste0("~/boxHKW/21S/DH/local/EXC2020/dybbuk/")
fns.base.2
fns.this<-"yudale_xml-edited003/"
fns.this<-"yudale_xml-edited003/yudale_xml-edited003-17-27/yudale_xml-edited003-17-27/"
fns.this<-"yudale_xml-edited003/yudale_xml-edited006-17-28/yudale_xml-edited006-17-28/"
fns<-paste0(fns.base.2,fns.this,"page/")
fns
flist<-list.files(fns)
preproc.fun<-function(pages.cor){ # not called function to get token list of half corrected transcription
  library(xml2)
  library(collostructions)
  library(quanteda)
  library(purrr)
  tlist<-list()
tlines<-list()
k<-1
pages.cor<-17:28
for (k in pages.cor){
  
#for (k in 1:length(flist)){
  file<-paste0(fns,flist[k])
  f1<-read_xml(file)
  f1
  tlist[[k]]<-f1
  f1%>% xml_ns_strip()
  alltext<-xml_find_all(f1,".//TextLine")
  tlines[[k]]<-xml_text(alltext)
  
}
3+4%>%sum()
#tokeniz
get.tok<-function(x)tokenize_word1(x)
tok.list<-lapply(tlines, get.tok)
f1<-read_xml(paste0(fns,flist[3]))
tok.freq<-freq.list(unlist(tok.list))
tok.freq
m<-grep("דיר",tok.freq)
m<-grep("דיִר",tok.freq)
tok.sort<-tok.freq[order(tok.freq$WORD),]
tok.sort
#write.csv(tok.sort,"~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_tok_freq.csv")
#save(tok.sort,file = "~/Documents/GitHub/ETCRA5_dd23/dybbuk/tok.sort.RData")
#f1%>% xml_ns_strip()
alltext<-xml_find_all(f1,".//TextLine")
xml_text(alltext)
#################
m<-grep("ביז",tok.sort$WORD)
tok.sort[m,] 
m<-grep("בִּיז",tok.sort$WORD)
tok.sort[m,]
return(tok.sort)
}
tok.sort<-preproc.fun()
tok.sort.f<-tok.sort[order(tok.sort$FREQ,decreasing = T),]
write.csv(tok.sort,"~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_tok_freq_17-28.csv")
### wks., grep differenciates between tokens with/wo vocalisation
### goal: to replace not vocalised tokens in xml with manually corrected tokens. pages 1-16 are not vocalise corrected,
### pages 17-23 are corrected.
##############################
### manually assign corrected versions in .csv
library(stringi)
library(utils)
### run after modifying .csv >
edit.xml.top<-function(){
tok.ed<-read.csv("~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_tok_freq/tok.freq.list.edited-yudale_tok_freq.csv")
tok.ed.1<-read.csv("~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_tok_freq.14256/tok.freq.list.edited-corrected.csv")

### merge corrected tables
tok.ed.1<-read.csv("~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_tok_freq.14256/tok.freq.list.edited-corrected.csv")
tok.ed.2<-read.csv("~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_tok_freq.14256/14256.17-27cor-corrected.csv")
colnames(tok.ed.2)[1]<-"id"
tok.ed.m<-rbind(tok.ed.1[55:length(tok.ed.1$id),c(1,2,6)],tok.ed.2[55:length(tok.ed.2$id),c(1,2,5)])
#write.csv(tok.ed.m,"~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_tok_freq.csv")
### pattern replacement
# regx.array<-c("בע","גע","פע","דע","ווע","בּע")
# repl.array<-c("בֶּע","גֶע","פֶע","דֶע","ווֶע","בֶּע")
# repl.array
# reg.p.1<-"קע"
# repl.p.1<-"קֶע"
# repl.df<-data.frame(regx=regx.array,repl=repl.array)
# #write.csv(repl.df,"~/Documents/GitHub/ETCRA5_dd23/dybbuk/replace.df.csv")
# regx.array<-c(regx.array,reg.p.1)
# repl.array<-c(repl.array,repl.p.1)
# regx.array
# repl.array
# tok.ed.m$global<-tok.ed.m$WORD
# for(k in 1:length(regx.array)){
#   tok.ed.m$global<-gsub(regx.array[k],repl.array[k], tok.ed.m$global)
# }
#tok.ed.m<-tok.ed.m[55:length(tok.ed.m$id),]
tok.ed<-tok.ed.m

edit.xml<-function(tok.ed){
repl.df<-read.csv("~/Documents/GitHub/ETCRA5_dd23/dybbuk/replace.df-m.csv")

fns.edit<-"~/boxHKW/21S/DH/local/EXC2020/dybbuk/Yudale_der_blinder,_Emkroyt1908-xpedit001/Yudale_der_blinder,_Emkroyt1908-xp001/page/"
fns.edit<-"~/boxHKW/21S/DH/local/EXC2020/dybbuk/yudale_xml-edited003/yudale_xml-edited003-17-27/edited004/page/"
#sort regexes after gefräszigkeit:
tok.ed$bytes<-unlist(lapply(tok.ed$WORD,object.size))
tok.ed.s<-tok.ed[order(tok.ed$bytes,decreasing = T),]
#tok.ed.repl<-tok.ed$cor.tok[order(tok.ed$cor.tok)]
### > TODO: replace order, first lagest 
#format(object.size("a"),units="B",digits=3,standard = "SI")
#sort1<-stri_count(tok.ed$WORD,regex = ".")
### array of pages not resp. corrected w/o vokalisation:
p.array<-c(5:16,28:70)
# for (k in 1:length(flist)){
p<-28
m.p<-grep(28,p.array)
p<-m.p
r<-5680
for (p in 1:length(p.array)){
  k<-p.array[p]
  file.xp<-paste0(fns,flist[k])
  t1<-readLines(file.xp)
  t1
  file.ed<-paste0(fns.edit,flist[k])
  t1[57]
  for (r in 55:length(tok.ed.s$WORD)){
    reg<-tok.ed.s$WORD[r]
    repl<-tok.ed.s$cor.tok[r]
    #repl.pre<-tok.ed.s$global[r]
    #ifelse(!is.na(repl)&repl!="",t1<-gsub(reg,repl,t1),t1<-gsub(reg,repl.pre,t1))
    if(!is.na(repl)&repl!="")
      t1<-gsub(reg,repl,t1)
    
    cat("editing page:",p,"token:",r,"of:",length(p.array),"pages","\n")
    
    
  }
  # global replace pattern
  for (g in 1:length(repl.df$regx)){
    reg.g<-repl.df$regx[g]
    repl.g<-repl.df$repl[g]
    t1<-gsub(reg.g,repl.g,t1)
  }
  
  writeLines(t1,file.ed)
  
}
}
edit.xml(tok.ed = tok.ed.m)
} # end editxmltop
edit.xml.top()
# tok.ed.s[86,]
# gsub(reg,repl.pre,t1)
### merge ngram tables
ng2<-read.csv("~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_2grams.csv",col.names = c("id","ngram","freq"))
ng3<-read.csv("~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_3grams.csv",col.names = c("id","ngram","freq"))
ng4<-read.csv("~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_4grams.csv",col.names = c("id","ngram","freq"))
ng.n<-rbind(ng2,ng3,ng4)
ng.n.s<-ng.n[order(ng.n$ngram),]
#write.csv(ng.n.s,"~/Documents/GitHub/ETCRA5_dd23/dybbuk/ngrams_sorted.csv")

##########
# train: one run (pg. 1-16, 24-70 corrected, imported to transkribus, manually edited p.24, exported, 
# run preproc.func() on new xml data
# tokens: original: 3301, corrected: 3107
#########################################
# perform fuzzy match:
library(fuzzyjoin)
#jw
a1<-tok.ed
a2<-tok.sort
m1<-a2$WORD%in%a1$cor.tok
sum(m1)
a2$cor.tok<-NA
a2.cor<-as.character(a2$WORD[m1])
a2$cor.tok[m1]<-a2.cor
write.csv(a2,"~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_tok_freq_17-27.csv")
a2.uncor<-a2[!m1,]
a2.uncor$cor.tok<-""
write.csv(a2.uncor,"~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_tok_freq_17-27-uncor.csv")

library(quanteda)

chars<-char_segment(a2.cor,".",valuetype = "regex",remove_pattern = F)
chars[100:550]
ngrams.2<-char_ngrams(chars,2,concatenator = "")
#ngrams[1:100]
ngrams.2[1:100]
ng.t<-table(ngrams.2)
ng.t
write.csv(ng.t,"~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_2grams.csv")
ngrams.3<-char_ngrams(chars,3,concatenator = "")
ng.t3<-table(ngrams.3)
ng.t
write.csv(ng.t3,"~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_3grams.csv")
ngrams.4<-char_ngrams(chars,4,concatenator = "")
ng.t4<-table(ngrams.4)
write.csv(ng.t4,"~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_4grams.csv")
m<-grepl("[א-ת]",chars)
sum(m)
chars[m]
###########################
j1<-stringdist_join(a1,a2,by="WORD",max_dist = 99,mode = "left",method = "jw",distance_col = "dist")
j3<-stringdist_join(a1,a2,by="WORD",max_dist = 1,method = "jw",distance_col = "dist")
j1[j1$dist>=1&j1$dist<=2,]
m<-j1$dist<0.8&j1$dist>0.74
j2<-j1[m,]

m<-j3$dist<0.8&j3$dist>0.7
j4<-j3[m,]
library(stringdist)
t1<-"ביז"
#tok.sort[m,] 
t2<-"בִּיז"
t1<-"arbeit"
t2<-"erbeit"
stringsim(t1,t2,method = "lv",p=0.1)
tok.ed$tok.2<-NA
tok.ed$tok.2[1:length(tok.sort$WORD)]<-levels(tok.sort$WORD)

