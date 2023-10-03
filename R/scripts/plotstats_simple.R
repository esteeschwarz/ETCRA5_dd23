#14404.plot textstats
#20231003(14.36)
################
# segments text and plots type/token ratio & wordlength over segments
##########
# run: plot.stats(<sourceurl of text>)

library(httr)
library(stringi)
#library(syuzhet)

src.1<-GET("https://dracor.org/api/corpora/ger/play/schlegel-die-stumme-schoenheit/spoken-text")
src.2<-GET("https://dracor.org/api/corpora/ger/play/lessing-emilia-galotti/spoken-text")

#source("~/Documents/GitHub/DYN_ss22/wriddle_getstopwords.R")

plot.stats<-function(src,out){
x<-GET(src)
  re<-content(x,"text")
dta<-re
get_types<-function(set,opt){
  
  set$contentp<-gsub("[^A-Za-z0-9äöüß \n]","",set$content) #get clean text
  set$contentp<-gsub("(\n)"," ",set$contentp) #get clean text
  set$contentp<-gsub("(   )"," ",set$contentp) #get clean text
  set$contentp<-gsub("(  )"," ",set$contentp) #get clean text
  set$contentp<-gsub("^( )","",set$contentp) #get clean text
  set$contentp<-gsub("(   )"," ",set$contentp) #get clean text
  set$contentp<-gsub("(  )"," ",set$contentp) #get clean text
  set$contentp<-gsub("^( )","",set$contentp) #get clean text
  set$tokens<-stri_count_boundaries(set$contentp) # IMPORTANT: with type=word far too much!

  wolftypes<-stri_split_boundaries(set$contentp)
  types<-lapply(wolftypes,unique)
  ltypes<-lapply(types,length)
  wolfchars<-function(x) stri_count_boundaries(x,"character")
  chars<-lapply(wolftypes, wolfchars)
  chars.avg<-function(x)mean(x)
  set$chars.avg<-lapply(chars,chars.avg)
  set$types<-unlist(ltypes)
  set$ttr<-set$types/set$tokens
  return(set)
}
#########
#re[1:20] # dracor text one dim, to split into segments
tx.split<-function(text,l){
re.s<-stri_split_boundaries(text,simplify = T)
#re.s<-stri_split_regex(re,"\n",simplify = T) # split into sentences
re.s.start<-seq(1,length(re.s),l)
re.s.end<-seq(0,length(re.s),l)
content_df<-data.frame(id=1:length(re.s.start),content=NA)
for(k in 1:length(content_df$id)){
  if (k<length(content_df$id)) {
  segment<-re.s[re.s.start[k]:re.s.end[k+1]]
  segment<-paste0(segment,collapse = " ")
  content_df$content[k]<-segment
  print(k)
  }  
  if(k==length(re.s.start)){
  segment.l<-re.s[re.s.start[k]:length(re.s)]
  segment.l<-paste0(segment.l,collapse = " ")
  content_df$content[k]<-segment.l
  }
}
return(content_df)
}
######
l.seg<-100 # #segment length in words
dta<-tx.split(re,l.seg)
dta_t<-get_types(dta,1)
# scatter.smooth(1:length(dta_t$ttr),dta_t$ttr,.1,.1,type="h",
#                family = "gaussian",ylab="segment type/token ratio",xlab = paste0("segments of ",l.seg," words"),
#                col=2)
# scatter.smooth(1:length(dta_t$chars.avg),dta_t$chars.avg,.1,.1,type="h",
#                family = "gaussian",ylab="segment average word length / chars",xlab = paste0("segments of ",l.seg," words"),
#                col=2)
if(out=="ttr")
   scatter.smooth(1:length(dta_t$ttr),dta_t$ttr,.1,.1,type="h",
               family = "gaussian",ylab="segment type/token ratio",main="type/token ratio",xlab = paste0("segments of ",l.seg," words"),
               col=2)
if(out=="char")
   scatter.smooth(1:length(dta_t$chars.avg),dta_t$chars.avg,.1,.1,type="h",
               family = "gaussian",ylab="segment average word length / chars",main="word length",xlab = paste0("segments of ",l.seg," words"),
               col=2)

# ttr.n<-get_transformed_values(unlist(dta_t$ttr))
# a2m<-max(dta_t$ttr)+.001
# a2im<-max(ttr.n)
# y<-max(a2im)/max(a2m)
# ttr.n<-ttr.n/y
# # scatter.smooth(1:length(ttr.n),ttr.n,.1,.1,type="h",
# #                family = "gaussian",ylab="segment type/token ratio (normalised)",xlab = paste0("segments of ",l.seg," words"),
# #                col=2)
# chars.n<-get_transformed_values(unlist(dta_t$chars.avg))
# a2m<-max(unlist(dta_t$chars.avg))+.001
# a2im<-max(chars.n)
# y<-max(a2im)/max(a2m)
# chars.n<-chars.n/y
# scatter.smooth(1:length(chars.n),chars.n,.1,.1,type="h",
#                family = "gaussian",ylab="segment average word length / chars (normalised)",xlab = paste0("segments of ",l.seg," words"),
#                col=2)

}#end plotfunction
#plot.stats(src.2)

temp.fun.matrix<-function(){
dta_t$contentp[11]
wolfmatrix<-matrix(stri_split_boundaries(dta_t$contentp,simplify = T),nrow = length(dta_t[,1]))
#wolfmatrix<-wolfmatrix[2:nrow(wolfmatrix),]
wolfmatrix<-gsub("[^A-Za-z0-9äöüß]","",wolfmatrix)
wolfmatrix[which(is.na(wolfmatrix))]<-""

#wolfmatrix[11,12]
#library(DramaAnalysis)
#keyn<-keyness(wolfmatrix) #first frequency table of matrix
length(wolfmatrix)
wf<-wolfmatrix
w.df<-as.data.frame(wolfmatrix)
k<-1
for (k in 1:length(wolfmatrix)){
  t<-wolfmatrix[k]
  
s1<-wolfmatrix[k]==wolfmatrix[1:length(wolfmatrix)]
s1<-sum(s1,na.rm = T)

#  s<-sum(grepl(wolfmatrix[k],wolfmatrix))
wf[k]<-s1

print(k)
}
c<-1
r<-11
c<-12
w.f.q<-w.df
wf.2<-wf
for(r in 1:length(w.df[,1])){
  #s2<-array()
  w.a<-w.df[r,][w.df[r,]!=""]
  for (c in 1:length(w.a)){
    s2<-sum(w.a[c]==w.a[1:length(w.a)])
    wf.2[r,c]<-s2
    
    
  }
#  wolfmatrix[11,12]
  print(r)
  
}
wf.md<-median(as.double(wf.2))
wf.max<-max(as.double(wf.2))

wf.2[wf.2==wf.max]<-1
wf.3<-as.double(wf.2)/as.double(wf) #relative frequency: sum over text/sum over corpus
wf.1<-wf.3==1
wolfmatrix[which(is.na(wf.3==1))]
wf.4<-wf.3
wf.4[wf.1]<-wf.3[wf.1]+as.double(wf.2)[wf.1]-1 # mfw for text
wf.4.df<-matrix(wf.4,ncol = length(w.df),byrow = T)
getwd()
githdir<-"~/documents/github/DYN_ss22"
#write_csv(data.frame(wf.4.df),paste(githdir,"data/wolf_freq.csv",sep = "/"))
w.df[1,][which.max(wf.4.df[1,])] # mfw top
#head(x2[order(x2,decreasing = T)])
w.df[1,][order(wf.4.df[1,],decreasing = T)][1:10] #10 mfw
n.mfw<-10
n.df<-length(w.df[,1])
#w.mfw<-matrix(ncol = n.mfw,nrow = n.df)
w.mfw<-data.frame(mfw=1:n.df)
r<-2
n.mfw.1<-n.mfw+40
typeof(w.df)
for (r in 1:n.df){
  row<-unlist(w.df[r,])
mfw.a<-unique(row[order(wf.4.df[r,],decreasing = T)]) #10 mfw
mfw.st<-mfw.a%in%wstop
mfw.b<-mfw.a[!mfw.st]
mfw.b<-mfw.b[mfw.b!=""]
mfw.b<-mfw.b[1:n.mfw]
#[1:n.mfw]
w.mfw[r,2:(n.mfw+1)]<-mfw.b
}
#write_csv(w.mfw,paste(githdir,"data/wolf_keywords.csv",sep = "/"))
}