#20240410(16.02)
#14155.hackathon newspapers Ilja
#extract years mentions, corpus: shtern
#######################################
# read in texts locally:
d<-list.files("~/boxHKW/21S/DH/local/EXC2020/hackathon 10 apr 24/merged2")
texts<-list()
#d
k<-1
for(k in 1:length(d)){
  tx<-readLines(d[k])
  texts[[k]]<-tx
}
k<-1
library(readtext)
setwd("~/boxHKW/21S/DH/local/EXC2020/hackathon 10 apr 24/merged2")
for(k in 1:length(d)){
  tx<-readtext(d[k])$text
  texts[[k]]<-tx
}
for(k in 1:length(d)){
  tx<-readLines(d[k])
  texts[[k]]<-tx
}
# y<-1937
 k<-1
 library(stringi)
sumyear.a<-array()
exlist<-list()
exlist2<-list()
exyear<-list()
sumyear.m<-matrix(nrow = 2839,ncol = 40)
sumyear.df<-data.frame(sumyear.m)
y<-1
y
k<-1
d[1]
rm(exlist)
tx1<-exlist[[1]]
isnotna<-function(x)sum(!is.na(x))
mna<-lapply(tx1,isnotna)
tx1[mna]
for(k in 1:dim(sumyear.m)[1]){
  cat(k,"\n")
    for (y in 21:40){
      year<-1900+y
  #    year<-1926
   #   m<-grepl(as.character(paste0("\b",year,"\b")),texts[[k]])
ex<-stri_extract_all_regex(texts[[k]],paste0("(\\b",year,"\\b)"))
 #ex<-stri_extract_all_regex(texts[[k]],paste0("(.{20})(1926)(.{20})"))
 #ex<-stri_extract_all_regex(texts[[k]],paste0("\\b(1926)\\b"))
 # ex<-stri_extract_all_regex(texts[[k]],paste0("(.{20})(\n)(.{20})"))
# ex<-stri_extract_all_regex(texts[[k]],paste0("(.{30}19{30})"))
# ex<-grep("1926",texts[[k]])
#ex
#textx<-texts[[k]]
#textx[ex]
mna<-!is.na(ex)
lm<-sum(mna)
lm
#if(lm>0)
#if(mna==T){

#   exyear[[y]]<-lm
#lyear<-length(exyear[[y]])
sumyear.df[k,y]<-lm
if(lm>0)
  exyear[[y]]<-ex
}
  #      sumyear.a[y]<-sum(m,na.rm = T)  
#  sumyear.a<-sum(m,na.rm = T)  
#mna<-!is.na(exyear[[y]])
      #  sumyear.df[k,y]<-ex

  if(length(exyear)>0)
    exlist2[[k]]<-exyear
  }
colnames(sumyear.df)<-1901:1940
rownames(sumyear.df)
save(exlist2,file = "../exlist.grep1_1921-1940.Rdata")

save(sumyear.df,file="../sumyeardf.grep1_1901-1940.RData")
# aggregate text/years
library(stringi)
y.split<-stri_split_regex(d,"_",simplify = T)
y.split
sumyear.df$year<-y.split[,1]
sumyear.df$ref_txt<-y.split[,2]# sumyear.df$text<-d
sumyear.df[1,42]
sumyear.df3<-sumyear.df[,c(41,42,1:40)]
m<-grepl("2020",sumyear.df3$year)
sumyear.df3<-sumyear.df3[!m,]
tail(sumyear.df3)
#colnames(sumyear.df3)[3:42]<-1901:1940
y.unique<-unique(sumyear.df3$year)
evalyear.m<-matrix(nrow = length(y.unique),ncol = 40)
evalyear.df.1<-data.frame(evalyear.m)
rownames(evalyear.df.1)<-y.unique
colnames(evalyear.df.1)<-1901:1940
k<-1
y.unique
for(k in 1:length(y.unique)){
  yearx<-y.unique[k]
  m<-sumyear.df3$year==yearx
  sumyearsub<-subset(sumyear.df3,sumyear.df3$year==yearx)
  sum(m)
  msum<-colSums(sumyearsub[3:42])
  evalyear.df.1[k,]<-msum

}
#evalyear.df.1$year<-y.unique

# get number of periodicals/jahrgang
#y.split
jsum<-array()
m<-grepl("2020",y.split)
sum(m)
y.split<-y.split[!m]

for(k in 1:length(y.unique)){
  m<-grepl(y.unique[k],y.split)
  
  jsum[k]<-sum(m)
}
jsum
evalyear.df.1$periods<-jsum
save(evalyear.df.1,file = "../evalyear.df.1.grep2b.RData")
save(evalyear.df.1,file = "../yearmention_DF.RData")
write.csv(evalyear.df.1,"../shtern.yearsmention.df.csv")

evalyear.df.2<-evalyear.df.1
evalyear.df.1$year<-y.unique
# plot jahrgang vs. year mention, (jahrgang 2020 excluded)
# !!!> 50 plots, one/year mention
k<-1
for(k in 1:40){
  
#scatter.smooth(evalyear.df.1[1:14,k]~evalyear.df.1$year[1:14])
#par(las=1)
plot(evalyear.df.1[1:14,k]~evalyear.df.1$year[1:14],type="l", xlab="jahrgang", ylab = "number of year mentions", main=paste0('year -',colnames(evalyear.df.1)[k], '- mentions in "Shtern" - journal'))
}
rm(exlist2)
