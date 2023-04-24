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
library(DramaAnalysis)
#lm<-function(x) paste0(x,"drama")
qdmeta<-lapply(qd,lm)
lapply(qd,lm)
loadMeta(list.files("data/qd/tg/csv"))
loadMeta(qddf$X3[1])
setDataDirectory(dataDirectory = "data/corpus")
qdm<-matrix(qdmeta)
library(xml2)
attr(qdmeta[[1]],"language")
qdmeta[[1]]
f<-loadDrama("test:rjmw.0")
installData("tg") #test, qd
qddf<-loadAllInstalledIds(asDataFrame = T)
lm<-function(x) loadMeta(x)
dmeta<-loadMeta(qddf$X3)
nr<-511
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
d1<-dpres(1)
nr<-1
#report(paste0(qddf$X1[nr],":",qddf$X3[nr]))
ff<-frequencytable(d1)
ff<-t(ff)
fdf<-data.frame(token=rownames(ff),count=ff[,1])
ff2<-fdf[order(ff[,],decreasing = T),]
