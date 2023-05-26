library(stringi)
d1<-read_table("data/gerdracor_01.vert",col_names = c("token","cat","lemma"),skip = 2) #read sketchenegine vertical export of corpus


               
get_pos<-function(set,set0){
  
  d4<-set
  #  dns[7:14]
  ann<-data.frame(stri_split_fixed(set0$cat,".",simplify = T))
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

ns_g2<-get_pos(d1,d1)
##################

######################
getarray<-function(set,r){
  d5<-set
  #s1<-d5$pos_cpt[r]
  s1<-d4$cat[r]
  #s1<-d4$cat[119]
  
  s1
  #mxcolumns<-grep("x",colnames(d4))
  s2<-stri_split_regex(s1,"\\.",simplify = T)
  a<-c(s2)
  s2<-a
  rstar<-match(s2,"*")
  s2[rstar==T]<-"-"
  print(s2)
  return(s2)
}
#############################
# NEW: ######################
# get codes cpt, grep value of useable values in code, output to useable value standard position
#############################
#d5<-getdata()
#d5<-d4
#r<-11
#top<-5
#d6<-d5
#ma<-array()
#s2
#d6<-matrix(nrow = length(d5$id),ncol = 8)
#top<-1
#l<-2
##########################
colnames(d1)
d2<-cbind(d1,x1=0,x2=0,x3=0,x4=0,x5=0,x6=0,x7=0,x8=0,x9=0)
d4<-d2
d5<-d4
d6<-d4
mxcolumns<-grep("x",colnames(d4))
###########
# this is a 5 minute loop!
###########
for (r in 1:length(d5$cat)){
  #s2<- d5$pos_cpt[r]
  s2<-getarray(d4,r)
  for (top in 1:length(ns_g2$cor)){
    for (l in 1:length(ns_g2$cor[[top]])){
      m1<-match(ns_g2$cor[[top]][[l]],s2)
      #s2[m1]
      
      pos<-mxcolumns[1]-1+top
      ifelse (m1!=0,d6[r,pos]<-s2[m1],d6[r,pos]<-"-")
      #cat("check token: [",r,"], tag = ",d6[r,],"\n")
      #print(d6[r,])
      print(r)
    }
  }
  #d6[pos,1:8]<-ma
} # end POS position correction
###############################
head(d6)


d6safe<-d6
getwd()
### DEFINITELY SAFE AFTER RUN! ####
#dbwritedir<-datadir
datadir<-"data"
###### finalise
colnames(d6)
#here stepback and run

dns_x<-c("pos","category","funct","case","pers","num","gender","tense","mode")
mxcolumns<-grep("x",colnames(d6))
dns_o<-colnames(d6)
dns_n<-c(dns_o[1:3],dns_x)
colnames(d6)<-dns_n
datestamp<-"13217"
dbname<-paste0("galotti_postagged_",datestamp,".csv")
write.csv(d6,paste(datadir,dbname,sep="/"))
