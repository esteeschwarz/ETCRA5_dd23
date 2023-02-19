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
