library("rdracor")
p<-get_dracor(corpus = "ger")
m<-grep("Klemm",p$firstAuthorName)
n<-p$playName[m]
nw<-get_net_cooccur_igraph(play=n[2],corpus = "ger")


summary(nw)
x<-nw
plot(
  x,
  layout = igraph::layout_with_kk,
  vertex.label = label_cooccur_igraph(x),
  gender_colors = c(MALE = "#0073C2", FEMALE = "#EFC000", UNKNOWN = "#99979D"),
  vertex_size_metric = c("numOfWords", "numOfScenes", "numOfSpeechActs", "degree",
                         "weightedDegree", "closeness", "betweenness", "eigenvector"),
  vertex_size_scale = c(5, 20),
  edge_size_scale = c(0.5, 4),
  vertex_label_adjust = TRUE,
  vertex.label.color = "#03070f",
  vertex.label.family = "sans",
  vertex.label.font = 2L,
  vertex.frame.color = "white",
)
#### 13173.
getwd()
library(DramaAnalysis)
#lm<-function(x) paste0(x,"drama")
#qdmeta<-lapply(qd,lm)
lapply(qd,lm)
l<-list.files("data/corpus")
#loadMeta("data/corpus/st/csv/_geladen")
setDataDirectory(dataDirectory = "data/corpus")
#qdm<-matrix(qdmeta)
# library(xml2)
# attr(qdmeta[[1]],"language")
# qdmeta[[1]]
#f<-loadDrama("test:rjmw.0")
f<-loadDrama("st:_geladen")
h<-hamming(f)
plot(h,f)
ff<-frequencytable(f)
presence(f)
#report(f)
plot(ff)
ff
ut<-utteranceStatistics(f)
#ff<-t(ff)
plot(ut,f)

ft <- frequencytable(f, byCharacter=TRUE)
g <- 1:7
f.cor <- correlationAnalysis(ft, g)

ft <- frequencytable(f, byCharacter=TRUE)
ft <- ft[,colSums(ft) > 5]
correlationAnalysis(ft, g)
#installData("tg") #test, qd
#installData("kt") #test, qd
#qddf<-loadAllInstalledIds(asDataFrame = T)
#lm<-function(x) loadMeta(x)
#dmeta<-loadMeta(qddf$X3)
#nr<-511
dpres<-function(nr){
#  f<-loadDrama(paste0(dmeta$corpus[nr],":",dmeta$drama[nr]))
  m<-loadMeta(paste0(qddf$X1[nr],":",qddf$X3[nr]))
  f<-loadDrama(paste0(qddf$X1[nr],":",qddf$X3[nr]))
  print(m[,c(1:5,9)])
  print(presence(f))
  return(f)
}
#presence("test:rjmw.0")
#m<-grep("test",dmeta$corpus)
#d1<-dpres(1)
#nr<-1
#report(paste0(qddf$X1[nr],":",qddf$X3[nr]))
#ff<-frequencytable(d1)
# ff<-frequencytable(f)
# presence(f)
# report(f)
# ut<-utteranceStatistics(f)
# ff<-t(ff)
# plot(ut,f)
# fdf<-data.frame(token=rownames(ff),count=ff[,1])
# ff2<-fdf[order(ff[,],decreasing = T),]
# library(quanteda)
#dtx<-loadText(paste0(qddf$X1[nr],":",qddf$X3[nr])) #NO funct()
#fieldnames = c("Teufel")
ds<-dictionaryStatistics(
  f,
#  fields = DramaAnalysis::base_dictionary[fieldnames],
  segment = c("Drama", "Act", "Scene"),
  normalizeByCharacter = TRUE,
  normalizeByField = FALSE,
  byCharacter = TRUE,
  column = "Token.lemma",
  ci = TRUE
)
#data(st)
# fnames <- c("Krieg", "Liebe", "Familie", "Ratio","Religion")
# ds <- dictionaryStatistics(rksp.0, normalizeByField=TRUE, 
#                            fieldnames=fnames)
# plotSpiderWebs(ds)