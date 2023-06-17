#13217.potsdam block 
#lessing, galotti PoS & lemmatization for further research
#
library(stringi)
#d1<-read_table("data/lessing-galotti-sketchenginePoS_Lemma.vert",col_names = c("token","cat","lemma"),skip = 1) #read sketchenegine export table (vertical) of corpus
d1<-read_table("data/philotas.vert",col_names = c("token","tag","lemma"),skip = 1) #read sketchenegine export table (vertical) of corpus


               
get_pos<-function(set0){
  ann<-data.frame(stri_split_fixed(set0$tag,".",simplify = T))
  ns_g<-list()
  
  ns_g[[1]]<-unique(ann$X1)
  ns_g
  ns_g[[2]]<-unique(ann$X2)
  ns_g[[3]]<-unique(ann$X3)
  ns_g[[4]]<-unique(ann$X4)
  ns_g[[5]]<-unique(ann$X5)
  ns_g[[6]]<-unique(ann$X6)
  ns_g[[7]]<-unique(ann$X7)
  ns_g2<-list()
  ns_g2[["db"]]<-ns_g
  ns_g2[["cor"]][["pos"]]<-ns_g[[1]]
  ns_g2[["cor"]][["cat"]]<-c("Name","Inter","Full","Pers","Pun","Reg","Aux","Dem","Def","Neg","Pos",
                             "Indef","Ans","Paren","Coord","Sup","SubFin","Poss","Rel","Mod","Comp",
                             "Verb","Refl","Zu","Deg","Other","SubInf","Gen","Adj","Noun")
  ns_g2[["cor"]][["funct"]]<-c("Comma","Slash","Hyph","Aster","Subst","Sent","Left","Right","Psp","Attr","XY","Auth")
  ns_g2[["cor"]][["case"]]<-c("Nom","Gen","Dat","Acc")
  ns_g2[["cor"]][["pers"]]<-c(1,2,3)
  ns_g2[["cor"]][["num"]]<-c("Sg","Pl")
  ns_g2[["cor"]][["gender"]]<-c("Neut","Fem","Masc")
  ns_g2[["cor"]][["tense"]]<-c("Pres","Past","Cont")
  ns_g2[["cor"]][["mode"]]<-c("Ind","Subj") #"Subj" == conditional
  return(ns_g2)
  # its 9 fields! not 8
  # TODO: c("zu","Auth","Sent","Psp","Cont")
} # end get_pos

##################

######################
getarray<-function(set,r){
  d4<-set
  s1<-d4$tag[r]
  s1
  s2<-stri_split_regex(s1,"\\.",simplify = T)
  a<-c(s2)
  s2<-a
  rstar<-match(s2,"*")
  s2[rstar==T]<-"-"
  print(s2)
  return(s2)
}
#d6<-d1
#set<-d3
#############################
# get codes cpt, grep value of useable values in code, output to useable value standard position
#############################
##########################
colnames(d1)
set<-d1
performsplit<-function(set){
d2<-cbind(set,x1=0,x2=0,x3=0,x4=0,x5=0,x6=0,x7=0,x8=0,x9=0)
mxcolumns<-grep("x",colnames(d2))
###########
# this is a 1 minute loop!
###########
r<-3
for (r in 1:length(d2$tag)){
  s2<-getarray(d2,r)
  for (top in 1:length(ns_g2$cor)){
    for (l in 1:length(ns_g2$cor[[top]])){
      m1<-match(ns_g2$cor[[top]][[l]],s2)
      #s2[m1]
      
      pos<-mxcolumns[1]-1+top
      ifelse (m1!=0,d2[r,pos]<-s2[m1],d2[r,pos]<-"-")
      #cat("check token: [",r,"], tag = ",d6[r,],"\n")
      #print(d6[r,])
      print(r)
    }
  }
} # end POS position correction
###############################
d6<-d2
head(d6)


d6safe<-d6
###### finalise
colnames(d6)
#here stepback and run

dns_x<-c("pos","category","funct","case","pers","num","gender","tense","mode")
mxcolumns<-grep("x",colnames(d6))
dns_o<-colnames(d6)
dns_1<-length(colnames(d6))-length(mxcolumns)
dns_n<-c(dns_o[1:dns_1],dns_x)
colnames(d6)<-dns_n
#clear lemma definition
clean_lemma<-function(x) gsub("-.*","",x)
c1<-lapply(d6$lemma, clean_lemma)
na<-is.na(c1)
c1[na]<-""
c1<-unlist(c1)
d6$lemma<-c1
n0<-d6==0
d6[n0]<-NA
return(d6)
}
getwd()
###wks.
# 2nd run with RFTagger:
tx1<-readLines("~/boxHKW/UNI/21S/DH/local/DD23/GerDraCor_SpokenText/lessing-emilia-galotti_spoken-text.txt")
tx1<-readLines("data/corpus/philotas/gleim_Philotas.txt")
tx1<-readLines("data/corpus/philotas/bodmer_Polytimet.txt")

txdf<-stri_split_boundaries(tx1,type="word",simplify = T)
txl<-stri_split_boundaries(tx1,type="word")
tx2<-unlist(txl)
tx3<-tx2[tx2!=" "]
getwd()
#writeLines(tx3,"data/corpus/philotas/philotas-tokens.txt")
writeLines(tx3,"data/corpus/philotas/polytimet-tokens.txt")

callrft<-"~/pro/RFTagger/src/rft-annotate ~/pro/RFTagger/lib/german.par data/lessing-galotti-tokens.txt data/lessing-galotti-rftagged_DF.csv"
callrft<-"~/pro/RFTagger/src/rft-annotate ~/pro/RFTagger/lib/german.par data/corpus/philotas/philotas-tokens.txt data/philotas_DF.csv"
callrft<-"~/pro/RFTagger/src/rft-annotate ~/pro/RFTagger/lib/german.par data/corpus/philotas/polytimet-tokens.txt data/polytimet_DF.csv"

system(callrft)
#d3<-read_table("data/lessing-galotti-rftagged_DF.csv",col_names = c("token","tag")) #read sketchenegine export table (vertical) of corpus
d3<-read_table("data/philotas_DF.csv",col_names = c("tok","tag")) #read sketchenegine export table (vertical) of corpus
d3<-read_table("data/polytimet_DF.csv",col_names = c("tok","tag")) #read sketchenegine export table (vertical) of corpus

set<-d3

ns_g2<-get_pos(set)
d7<-performsplit(d3)
#d6<-performsplit(d1)

datadir<-"data"
datestamp<-"13221"
dbname1<-paste0("lessing-galotti_PoStagged-lemmatized.csv")
dbname1<-paste0("philotas_PoStagged-lemmatized.csv")
write.csv(d6,paste(datadir,dbname1,sep="/"))
dbname2<-paste0("philotas_PoStagged.csv")
dbname2<-paste0("philotas_PoStagged.csv")
dbname2<-paste0("philotas")
dbname2<-paste0("polytimet")
pepperdirdd<-paste("corpus/philotas/philotas_corpus",sep = "/")
dir.create(pepperdir)
write.csv(d7,paste(datadir,dbname2,sep="/"))
writexl::write_xlsx(d7,paste(datadir,pepperdirdd,paste0(dbname2,".xlsx"),sep="/"))

###wks.
#check
d8<-read.csv(paste0("data/",dbname1))
#d9<-read.csv(paste0("data/",dbname2))
#remove metatokens
m<-grep("<g/>|<s>|</s>",d8$token,invert = T)
d8b<-d8[m,]
write.csv(d8b,paste(datadir,dbname1,sep="/"))



