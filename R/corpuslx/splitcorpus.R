#13326.dd23.annis corpus essai
#20230810(20.55)
#make dd23 pieces annis available
#################################
library(xml2)
library(readr)
library(stringi)
library(R.utils)
getwd()
src<-"dd23/dd23.tt" #original: dd23.vert (sketch engine download compiled corpus vertical format of xml import of plays)
src<-"dd23_17_1.vert" #original: dd23.vert (sketch engine download compiled corpus vertical format of xml import of plays)
list.files(getwd())
d1<-read_html(src,encoding = "UTF-8")
alltext<-xml_find_all(d1,"//text")
alldesc<-xml_find_all(d1,"//title")
alltitle<-xml_attr(alldesc,"type")
alldocs<-xml_find_all(d1,"//doc")
allnames<-xml_attr(alldocs,"filename")
#  regx0a<-"((?<=<body>).*?(?=</body>))"

allnames.x<-stri_extract_all_regex(allnames,"((?<=-).*?(?=\\.))",simplify = T)
# mt<-alltitle=="main"
# mt[is.na(mt)]<-F
# sum(mt)
# m.mt<-which(mt)
# pn<-6
# gettitle<-function(pn){
#   
# 
# m.mt.tx<-stri_split_boundaries(xml_text(alldesc[m.mt[pn]]),simplify = T)
# m.mt.tx
# m<-m.mt.tx!="\r\n"
# m.x<-m.mt.tx=="x\r\n"
# m.mt.tx<-m.mt.tx[m]
# #m.x<-c(T,m.x)
# m.mt.tx.i<-insert(m.mt.tx,which(m.x),"dum")
# m.mt.tx.i
# m.l<-length(m.mt.tx)
# m.l<-m.l-3 # last token
# #m.l.1<-m.l/3 # gesamt token
# m.l.s<-seq(1,m.l,4) # from 2nd item to last token, each a 3 item token separated by one item
# t.x<-paste0(m.mt.tx[m.l.s],collapse = " ")
# t.x<-gsub("[^A-Za-zäöüß?!,\\. \\-]","",t.x)
# }
# t1<-gettitle(6)
# xml_attr(alldesc)
# allx<-xml_find_all(d1,"//doctitle")
# cat(xml_text(alldesc[1]))
# write.table(xml_text(allx),"doctitle.csv")
# 
# xml_text(alltext)
k<-1
getwd()
corpusdir<-"dd23_17.1"
dir.create(corpusdir)
for (k in 1:length(alltext)){
  
  writeLines  (xml_text(alltext[k]),paste0(corpusdir,"/",allnames.x[k],".tt"))
}
xml_text(alltext[1])
