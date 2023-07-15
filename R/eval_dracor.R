# src<-"~/boxHKW/UNI/21S/DH/local/EXC2020/DD23/data/data.csv"
# d1<-read.csv(src)
# einakter<-d1
# hist(einakter$normalizedYear, breaks = 20, xlab="Jahrfünft", ylab="Anzahl Stücke im Korpus")
# plot(einakter$normalizedYear, einakter$numberOfScenes, xlab="Jahr", ylab="Anzahl Szenen pro Stück")
# plot(einakter$normalizedYear, einakter$numberOfCharacters, xlab="Jahr", ylab="Anzahl Figuren pro Stück")
library(jsonlite)
einakter <- fromJSON("https://einakter.dracor.org/data.json")
#einakter$cast[1]

# m<-grepl("Apoll",einakter$cast)
# sum(m)
# m<-1
m<-einakter$printed=="NULL"
sum(m)
einakter$printed[m]<-NA
set<-einakter

m
m<-grep("Apoll",einakter$cast)
spitcast<-function(set,cast){
  ndf<-data.frame()
  m<-grepl(cast,set$cast)
  print(sum(m))
  m<-grep(cast,set$cast)
  s<-data.frame(author=set$author$name[m],year=unlist(set$printed[m]),title=set$title[m])
  print(s)
  return(s)
}
#typeof(set$prin)
ndf<-spitcast(einakter,"Lisette")
# u<-unlist(unique(einakter$cast))
# head(u)
# u2<-unique(einakter$cast)
# head(u2)
# head(u2)
# u3<-data.frame(einakter$cast)
# u3<-einakter$cast
# head(u3[[2]][1])
# un<-function(x)x[[]][1]
# u4<-lapply(u3,un(u3)) 
