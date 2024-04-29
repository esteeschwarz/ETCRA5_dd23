#20240413(09.21)
#14161.shtern build corpus
##########################
d<-list.files("~/boxHKW/21S/DH/local/EXC2020/DHJ2024/merged2")
srcdir<-"~/boxHKW/21S/DH/local/EXC2020/DHJ2024/merged2/"
#texts<-list()
#d
k<-1
# for(k in 1:length(d)){
#   tx<-readLines(paste0(srcdir,d[k]))
#   texts[[k]]<-tx
# }

#save(texts,file = "~/boxHKW/21S/DH/local/EXC2020/DHJ2024/shterntexts.RData")
load("~/boxHKW/21S/DH/local/EXC2020/DHJ2024/shterntexts.RData")
###
library(tokenizers)
library(stringi)

##############################
get.freq<-function(t1,k,t){
  grep.y.l<-function(x)grepl("[0-9]{4}",x)
  #y.x.l<-lapply(tokenlist, grep.y.l)
  y.x.l<-lapply(t1, grep.y.l) # which token == regex
  #y.x.l[[1]]
  matr.t<-matrix(unlist(t1)) # token matrix
  y.x.l.u<-unlist(y.x.l) # match token==regex array
  sum(y.x.l.u)
  #as.double(matr.t[y.x.l.u][1:20])
  matr.g.true<-which(as.double(matr.t[y.x.l.u])>=1900&as.double(matr.t[y.x.l.u])<=1940)
  #matr.g.true[[1]]
  mna<-is.na(matr.g.true)
  matr.g.true[mna]<-F
  matr.t[y.x.l.u][matr.g.true]
  grep.true<-function(x)TRUE%in%x
  y.x.l.true<-lapply(y.x.l, grep.true)
  length(matr.t)
  length(t1)
  matr.l<-matrix(t1)
  y.x.l.m<-unlist(y.x.l.true)
  y.lines<-matr.l[y.x.l.m]
  y.lines
  y.lines
  fr.table<-table(matr.t[y.x.l.u][matr.g.true])
  fr.table
  returnlist<-list(frequency=fr.table,lines=y.lines)
  return(returnlist)
}
get.lines<-function(tokenlist,t){
  grep.y.l<-function(x)grepl("[0-9]{4}",x)
  y.x.l<-lapply(tokenlist, grep.y.l)
  grep.true<-function(x)TRUE%in%x
  #TRUE%in%y.x.l[[110]]
  y.x.l.true<-lapply(y.x.l, grep.true)
#y.x.l.true[[2]]
matr.l<-matrix(texts[[t]])
y.x.l.m<-unlist(y.x.l.true)
y.lines<-matr.l[y.x.l.m]
#y.lines
  }
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
tokenlist.x<-list()

  for (k in 1:length(y.un)){
    y<-y.un[k]
m<-grep(y,d)
t<-1
t
m
for(t in 1:length(m)){
  tn<-m[t]
  cat(k,"/",length(y.un),"-",t,"/",length(m),"\n")
tns<-tns.id[tn]
t
tn
tns
t1<-tokenize_words(texts[[tn]])
#tokenlist[k]<-tns[k]
#y.lines<-get.lines(t1,t)
#y.lines<-get.freq(t1,k,t)
#y.lines
#y.lines<-get.lines(tokenlist.x,k,t)
#fr.table<-get.freq(tokenlist = tokenlist.x,k,t)
fr.table<-get.freq(t1,k,tn)
fr.table$frequency
fr.table
tokenlist.x[[y]][[tns]]<-fr.table
#tokenlist.x[["lines"]][[t]]<-y.lines
#tokenlist[[y]][[1]][[t]]<-fr.table
#tokenlist[[y]]<-tokenlist.x
}
  }
#tokenlist.x[["1927"]][[1]][[1]]
### wks.
#save(tokenlist.x,file = "~/boxHKW/21S/DH/local/EXC2020/DHJ2024/tokenlistx.RData")
### now sum up mentions in year
year.df<-data.frame(year=y.un)
y.q<-c(1900:1940)
#sum(tokenlist.x[["1927"]][[1]][["frequency"]]["1904"])
for (k in 1:length(y.q)){
  for(r in 1:length(tokenlist.x)){
  y<-as.character(y.q[k])
sumyear<-function(x)sum(x[["frequency"]][y])
#library(abind)
s1<-lapply(tokenlist.x[[r]], sumyear)
s1
s1<-sum(unlist(s1),na.rm = T)
year.df[r,y]<-s1
}
}
jsum<-array()
for(k in 1:length(y.un)){
  m<-grepl(y.un[k],y.split[,1])
  
  jsum[k]<-sum(m)
}
jsum
year.df$periods<-jsum
year.df.norm<-year.df
evalyear.df.1<-year.df
m<-grep("[0-9]{4}",colnames(year.df))
year.df.norm[,m]<-year.df.norm[,m]/year.df.norm$periods
for(k in m){
  ### plotting  
  #scatter.smooth(evalyear.df.1[1:14,k]~evalyear.df.1$year[1:14])
  #par(las=1)
  plot(evalyear.df.1[1:14,k]~evalyear.df.1$year[1:14],type="l", xlab="jahrgang", ylab = "number of year mentions", main=paste0('year -',colnames(evalyear.df.1)[k], '- mentions in "Shtern" - journal'))

  }
for(k in m){
  ### plotting  
  #scatter.smooth(evalyear.df.1[1:14,k]~evalyear.df.1$year[1:14])
  #par(las=1)
  plot(year.df.norm[1:14,k]~year.df.norm$year[1:14],type="l", xlab="jahrgang", ylab = "number of year mentions (normalised)", 
       main=paste0('year -',colnames(evalyear.df.1)[k], '- mentions in "Shtern" - journal'))
  
}
#save(evalyear.df.1,file = "~/Documents/GitHub/ETCRA5_dd23/DHJ2024/data/yearmention_DF_2nd.RData")
#write.csv(evalyear.df.1,"~/Documents/GitHub/ETCRA5_dd23/DHJ2024/data/yearmention_DF_2nd.csv")

### keyword analysis
load("~/boxHKW/21S/DH/local/EXC2020/DHJ2024/tokenlistx.RData")

### to install collostructions (not on CRAN) call function
inst.coll<-function(){
  # dtemp<-tempfile()
  # download.file("https://sfla.ch/wp-content/uploads/2021/02/collostructions_0.2.0.tar.gz",dtemp)
   library(devtools)
   # devtools::install_local(dtemp)
### the package is now on github:
    devtools::install_github("skeptikantin/collostructions")
}
t<-require("collostructions")
if (!t)
  inst.coll()
library(collostructions)

#freq.list(tokenlist.x[[1]][[1]][[2]][[1]])
#key.list<-list()
#k<-1
# for (k in 1:length(tokenlist.x)){
#   get.fr.list<-function(x)freq.list(unlist(x[[k]]))
#   token.fr<-lapply(tokenlist.x, get.fr.list)
# key.list[[k]]<-token.fr  
# }
# key.list[[1]]
get.fr.list<-function(x)freq.list(unlist(x))
token.fr<-lapply(tokenlist.x, get.fr.list)
key.list<-token.fr
tokenfreqlist<-key.list
#save(key.list,file = "~/boxHKW/21S/DH/local/EXC2020/DHJ2024/keywordlist.RData")
#save(key.list,file = "~/Documents/GitHub/ETCRA5_dd23/DHJ2024/data/keywordlist.RData")
#token.fr[[2]]
#length(unlist(tokenlist.x[[1]]))
load("~/boxHKW/21S/DH/local/EXC2020/DHJ2024/keywordlist.RData")
key.list[[1]]
#############
length(key.list[[1]][["WORD"]])
length(unique(key.list[[1]][["WORD"]]))
m<-duplicated(key.list[[1]][["WORD"]])
key.list[[1]][[1]][m]
# sort(table(key.list[[1]][[1]]))
# head(table(factor(key.list[[1]][[1]])))
# table(factor(key.list[[1]][[1]]))
head(key.list[[1]],10)
m<-key.list[[1]][[1]]==1926
key.list[[1]][m,1:2]
sum(key.list[[1]][m,2]) # conforms with 2nd essai result
m<-key.list[[1]][[1]]==1925
key.list[[1]][m,1:2]
sum(key.list[[1]][m,2]) # conforms with 2nd essai result
#####
library(quanteda.textstats)
coll<-textstat_collocations(tokenlist.x[[1]],size = 5, min_count = 2)
library(quanteda)
library(tokenizers)
t1<-tokenize_words(texts[[1]])
t1<-tokens(texts[[1]])
coll<-textstat_collocations(t1,size = 3, min_count = 2)
library(topicmodels)
ndfmat<-dfm(t1)
m<-is.na(ndfmat)
m<-ndfmat==""
sum(m)
t1.tp<-CTM(data.frame(key.list[[1]]),3)
library(udpipe)
t2<-data.frame(term=unlist(t1),year=1926.1)
t3<-rbind(t2,data.frame(term=unlist(tokenize_words(texts[[2]])),year=1926.2))
t1.kw<-keywords_rake(t3,"term","year")
f1<-freq.list(t2$term)
f1$WORD
### TODO: make stopwordlist
f1.stops<-data.frame(word=f1$WORD,stop=1)
write.csv(f1.stops,"~/boxHKW/21S/DH/local/EXC2020/DHJ2024/share/stopwords_df.csv")
#s1<-data.frame(unlist(s1))
# data.frame(tokenlist.x[["1927"]][[2]][["frequency"]])
# y="1904"
# tokenlist.x[["1927"]][[1]][["frequency"]][y]

# y.lines<-get.lines(tokenlist[[1]][[2]],1,1)
# fr.table<-get.freq(tokenlist[[1]][[2]],1,1)
# fr.table2<-get.freq(tokenlist[[2]][[2]],2,2)
# fr.table
# ### wks.
# tokenlist[[1]][[]]
# ### ? how to display/arrange RTL text order into vertical format?
# ### > the character order RTL is correct, but in the vertical, word order would follow LTR direction
# #----
#   # perform frequency analysis for years
# k<-1
# t<-1
# y.lines<-get.lines(tokenlist.x,k,t)
# tokenlist[[1]][[2]][[1]]
# 
# fr.table<-get.freq(tokenlist = tokenlist[[1]][[2]][[1]],1,1)
# fr.table
# # grep.y<-function(x)grep("[0-9]{4}",x)
# # y.x<-grep("[0-9]{4}",tokenlist)
# # y.x<-lapply(tokenlist, grep.y)
# # y.x
# # grep.tx<-function(x)print(x[lapply(y.x,print)])
# # lapply(tokenlist, grep.tx(lapply(y.x,print)))
# # lapply(tokenlist,print(lapply(y.x,print))
# # lapply(tokenlist, grep.tx)
# # tokenlist[[2]][y.x[[2]]]
# #tokenlist[[]][y.x[[]]]
# # get.freq<-function(df){
# # matr.t<-matrix(unlist(tokenlist))
# # # matr.g<-matrix(unlist(y.x))
# # grep.y.l<-function(x)grepl("[0-9]{4}",x)
# # y.x.l<-lapply(tokenlist, grep.y.l)
# # matr.g<-matrix(unlist(y.x.l))
# # # length(unlist(y.x.l))
# # # length(unlist(tokenlist))
# # # y.x.l<-grep("[0-9]{4}",matr.t)
# # #matr.t[y.x.l]
# # matr.g<-y.x.l
# # matr.g.true<-as.double(matr.t[y.x.l])>1900&as.double(matr.t[y.x.l])<1940
# # mna<-is.na(matr.g.true)
# # matr.g.true[mna]<-F
# # f.y.1<-matr.t[matr.g[matr.g.true]]
# # #matr.t[matr.g]
# # table(f.y.1)
# # }