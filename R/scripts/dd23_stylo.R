#13451.klemm semantic stylometry
#20221105(08.04)
#aftermath klemm_HA
###################
library(httr)
library(jsonlite)
library(purrr)
library(stylo)
library(igraph)
library(network)


src<-"https://dracor.org/api/corpora/ger/play/klemm-der-besuch/spoken-text-by-character.json"
#dtajson<-GET(src)
src<-"https://dracor.org/api/corpora/ger/play/lessing-emilia-galotti/spoken-text-by-character.json"
#13302.api unavailable
src<-"~/Documents/GitHub/ETCRA5_dd23/R/data/ger000577-klemm-der-besuch-spoken.json"
dtatxt<-fromJSON(src)
dtatxt<-fromJSON(dtajson)

dtatxt$label
###
#write textfiles
root<-"~/boxHKW/21S/DH"
getwd()
k<-2
typeof(dtatxt$text[[k]])
speakerdir<-paste0(root,"/gith/DH_essais/sections/DD/klemm_HA/stylo/corpus/")
dir.create(speakerdir)
for (k in 1:length(dtatxt$label)){
  speakerfile<-paste0(speakerdir,"klemm_",dtatxt$id[k],".txt")
  writeLines(dtatxt$text[[k]],speakerfile)
#  writeLines(dtatxt$text[[k]],paste0(speakerdir,dtatxt$label[k],"test.txt"))
  
  }
#stilometrie, get MFW / edges list
setwd(paste0(root,"/gith/DH_essais/sections/DD/klemm_HA/stylo"))
################################################################
################################################################
#13247.block ada
################
getwd()
corpusdir<-"data/corpus/stylo/lauer-jannidis-autorschaft"
corpusdir<-"stylo/in-out/corpus"
corpusdir<-"corpus"
speakerdir<-corpusdir
stylo(gui=T,corpus.dir = corpusdir)
stylo
#network according to MFW
#dtanet<-read.csv("stylo_Consensus_100-100_MFWs_Culled_0__Classic Delta_C_0.5_EDGES.csv")
edges <- read.csv("in-out/in-out_Consensus_100-100_MFWs_Culled_0__Classic Delta_C_0.5_EDGES.csv")
net <- graph_from_data_frame(d=edges, 
                             directed=FALSE) 
label<-list.files("in-out/corpus")
dtatxt<-data.frame(label=1:length(label))
corpusdir
getwd()
dtatxt$label<-label
n <- 25 # für die Knotengröße
col <- "orange" # für die Knotenfarbe
V(net)$name<-dtatxt$label

# Knotengröße ändern
# Gekrümmte Kanten (edge.curved=.4) :
plot(net, edge.arrow.size=.4,
     vertex.size=n, vertex.color=col,
     vertex.label=V(net)$name,
     vertex.label.cex=.8, edge.curved=.5)
#net
x<-1
x<-stylo(mfw.min = 100,mfw.max = 100,analysis.type = "CA",corpus.dir<-speakerdir,output="screen")
install.packages("networkD3")
library(networkD3)
#stylo(corpus.dir = path2, analysis.type = "CA", mfw.min = 100, mfw.max = 3000, custom.graph.title = "Klemm vs. Others", write.png.file = TRUE, gui = FALSE)
# BCT or CA
#stylo(corpus.dir = path2, gui = T)
x<-doe
nodes<-x$list.of.nodes
edges<-x$list.of.edges
net2 <- graph_from_data_frame(d=edges) 
net2<-graph_from_data_frame(d=Weight)
edges<- edges[,c("Source", "Target", "Weight")]
 edges # überprüfen
# # neue Kantenliste speichern
# write.csv(edges_df, "ger000577-klemm-der-besuch_edges.csv", quote=FALSE, row.names=FALSE) # alte Kantenliste überschreiben
# 
# # Kantenliste neu einlesen: 
# edges <- read.csv("ger000577-klemm-der-besuch_edges.csv", header=T, as.is=T) 
#
 
network(x,vertices=x$list.of.nodes)
is.network(edges_df$Weight)

# simple networks with vertices =======================================================
simple_edge_df <- data.frame(
  from = c("b", "c", "c", "d", "a"),
  to = c("a", "b", "a", "a", "b"),
  weight = c(1, 1, 2, 2, 3),
  stringsAsFactors = FALSE
)
simple_edge_df
simple_vertex_df <- data.frame(
  name = letters[1:5],
  residence = c("urban", "rural", "suburban", "suburban", "rural")
)
simple_vertex_df
simple_edge_df<-doe
net3<-(as.network(simple_edge_df, vertices = simple_vertex_df))
plot(net3, edge.arrow.size=.4,label=simple_vertex_df$name, 
     vertex.size=n)
simple_vertex_df$residence
  plot(net4,label=nodes$id)
###########################
  #13302.
  #src: https://emitanaka.org/blog/2021-02-03-current-state-of-experimental-design-r-packages/current-state-of-experimental-design-r-packages.html
 src<-"data/ger000577-klemm-der-besuch.csv"
  doe_imports<-read.csv(src)
  library(ggraph)
  doe<-doe_imports
   graph_from_data_frame(doe_imports) %>% 
    ggraph(layout = 'fr') +
    geom_edge_link(aes(start_cap = label_rect(doe$source),
                       end_cap = label_rect(doe$target)), 
                   arrow = arrow(length = unit(2, 'mm')),
                   color = "#79003e") + 
    geom_node_text(aes(label = name),
                   color = "#79003e") +
    theme(panel.background = element_rect(fill = "#f6e5ee",
                                          color = "#79003e"),
          plot.margin = margin(20, 20, 20, 20))
  
  a1<-data.frame(
    from = edges$Source,
    to = edges$Target,
    weight = edges$Weight)
  net4<-as.network(a1)
  speaker<-paste0("klemm_",nodes$id)
  nodes$speaker<-speaker  
nodes

library(syuzhet)
a2<-get_sentiment(dtatxt$text[5[1:4]],method = "syuzhet",language = "german")
a2
dtatxt$text[[5]][1:4]
