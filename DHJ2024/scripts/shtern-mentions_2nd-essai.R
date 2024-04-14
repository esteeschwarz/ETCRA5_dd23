#20240413(09.21)
#14161.shtern build corpus
##########################
d<-list.files("~/boxHKW/21S/DH/local/EXC2020/DHJ2024/merged2")
srcdir<-"~/boxHKW/21S/DH/local/EXC2020/DHJ2024/merged2/"
texts<-list()
#d
k<-1
for(k in 1:length(d)){
  tx<-readLines(paste0(srcdir,d[k]))
  texts[[k]]<-tx
}
###
library(tokenizers)
library(stringi)

##############################
get.freq<-function(tokenlist,k,t){

#  tokenlist<-tokenlist.x
  matr.t<-matrix(unlist(tokenlist[[t]]))
  # matr.g<-matrix(unlist(y.x))
  #grep.y.l<-function(x)grepl("[0-9]{4}",x)
  #y.x.l<-lapply(tokenlist, grep.y.l)
  # length(unlist(y.x.l))
  # length(unlist(tokenlist))
   y.x.l<-grep("[0-9]{4}",matr.t)
 #  matr.g<-matrix(unlist(y.x.l))
   
  #matr.t[y.x.l]
  matr.g<-y.x.l
  matr.g.true<-as.double(matr.t[y.x.l])>1900&as.double(matr.t[y.x.l])<1940
  mna<-is.na(matr.g.true)
  matr.g.true[mna]<-F
  f.y.1<-matr.t[matr.g[matr.g.true]]
  #matr.t[matr.g]
fr.table<-table(f.y.1)
}
get.lines<-function(tokenlist,k,t){
  grep.y.l<-function(x)grepl("[0-9]{4}",x)
  y.x.l<-lapply(tokenlist[[t]], grep.y.l)
  grep.true<-function(x)x%in%TRUE
#  y.x.l[[1]]%in%TRUE
  y.x.l.true<-lapply(y.x.l, grep.true)
y.x.l.true
matr.l<-matrix(texts[[k]])
y.x.l.m<-unlist(y.x.l.true[[k]])
y.lines<-matr.l[y.x.l.m]
  }
# y.lines<-get.lines(tokenlist.x,1)
#######################
y.split<-stri_split_regex(d,"_",simplify = T)
tns.y<-y.split[,1]
tns.t<-y.split[,2]
tns.id<-paste0(y.split[,1],"_",y.split[,2])
y.un<-unique(tns.y)
y.un
y.un<-y.un[y.un!=2020]
k<-1
#for (k in 1:length(y.un)){
tokenlist<-list()

  for (k in 1:2){
    y<-y.un[k]
m<-grep(y,d)
tokenlist.x<-list()
t<-1
for(t in m){
  cat(t,"\n")
tns<-tns.id[t]
t1<-tokenize_words(texts[[t]])
#tokenlist[k]<-tns[k]
tokenlist.x[[t]]<-t1
y.lines<-get.lines(tokenlist.x,k,t)
fr.table<-get.freq(tokenlist = tokenlist.x,k,t)
tokenlist[[y]][[1]]<-fr.table
tokenlist[[y]][[2]]<-tokenlist.x
}
}
### wks.
tokenlist[[1]][[]]
### ? how to display/arrange RTL text order into vertical format?
### > the character order RTL is correct, but in the vertical, word order would follow LTR direction
#----
  # perform frequency analysis for years
k<-1
t<-1
y.lines<-get.lines(tokenlist.x,k,t)
tokenlist[[1]][[2]][[1]]

fr.table<-get.freq(tokenlist = tokenlist[[1]][[2]][[1]],1,1)
fr.table
# grep.y<-function(x)grep("[0-9]{4}",x)
# y.x<-grep("[0-9]{4}",tokenlist)
# y.x<-lapply(tokenlist, grep.y)
# y.x
# grep.tx<-function(x)print(x[lapply(y.x,print)])
# lapply(tokenlist, grep.tx(lapply(y.x,print)))
# lapply(tokenlist,print(lapply(y.x,print))
# lapply(tokenlist, grep.tx)
# tokenlist[[2]][y.x[[2]]]
#tokenlist[[]][y.x[[]]]
# get.freq<-function(df){
# matr.t<-matrix(unlist(tokenlist))
# # matr.g<-matrix(unlist(y.x))
# grep.y.l<-function(x)grepl("[0-9]{4}",x)
# y.x.l<-lapply(tokenlist, grep.y.l)
# matr.g<-matrix(unlist(y.x.l))
# # length(unlist(y.x.l))
# # length(unlist(tokenlist))
# # y.x.l<-grep("[0-9]{4}",matr.t)
# #matr.t[y.x.l]
# matr.g<-y.x.l
# matr.g.true<-as.double(matr.t[y.x.l])>1900&as.double(matr.t[y.x.l])<1940
# mna<-is.na(matr.g.true)
# matr.g.true[mna]<-F
# f.y.1<-matr.t[matr.g[matr.g.true]]
# #matr.t[matr.g]
# table(f.y.1)
# }