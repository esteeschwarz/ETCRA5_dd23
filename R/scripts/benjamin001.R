getwd()
library(readr)
library(stringi)
library(writexl)
d1<-read_table("local/R/corpus/benjaminfeldkraft.vert",col_names = c("token","cat","lemma"))
datetime<-"13172.2"
#"corpus"
#d2<-cleandb(d1)
#d2<-preprocess_temp(d1)
#### sentences
m1<-grep("<s>",d1$token)
m2<-grep("</s>",d1$token)

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
ns_g2[4]

dns<-c("token","cat","lemma","x1","x2","x3","x4","x5","x6","x7","x8","x9")
d2<-cbind(d1[,1:3],x1=0,x2=0,x3=0,x4=0,x5=0,x6=0,x7=0,x8=0,x9=0)

# getarray<-function(set,r){
#   d5<-set
#   #s1<-d5$pos_cpt[r]
#   s1<-d4$cat[r]
#   #s1<-d4$cat[119]
#   
#   s1
#   #mxcolumns<-grep("x",colnames(d4))
#   s2<-stri_split_regex(s1,"\\.",simplify = T)
#   a<-c(s2)
#   s2<-a
#   rstar<-match(s2,"*")
#   s2[rstar==T]<-"-"
#   print(s2)
#   return(s2)
# }
#############################
# NEW: ######################
# get codes cpt, grep value of useable values in code, output to useable value standard position
#############################
#d5<-getdata()
#d3<-d2
#r<-11
#top<-5
#d6<-d2
#ma<-array()
#s2
#d6<-matrix(nrow = length(d5$id),ncol = 8)
#top<-1
#l<-2
##########################
# this is a 5 minute loop!
mxcolumns<-grep("x",colnames(d2))
#r<-10
for (r in 1:length(d2$cat)){
  #s2<- d5$pos_cpt[r]
  #s2<-getarray(d4,r)
  s1<-d2$cat[r]
  #s1<-d4$cat[119]
  
#  s1
  #mxcolumns<-grep("x",colnames(d4))
  s2<-stri_split_regex(s1,"\\.",simplify = T)
  a<-c(s2)
  s2<-a
  rstar<-match(s2,"*")
  s2[rstar==T]<-"-"
  print(s2)
  
  for (top in 1:length(ns_g2$cor)){
    for (l in 1:length(ns_g2$cor[[top]])){
      m1<-match(ns_g2$cor[[top]][[l]],s2)
      #s2[m1]
      
      pos<-mxcolumns[1]-1+top
      ifelse (m1!=0,d3[r,pos]<-s2[m1],d3[r,pos]<-"-")
      #cat("check token: [",r,"], tag = ",d6[r,],"\n")
      #print(d6[r,])
      print(r)
    }
  }
  #d6[pos,1:8]<-ma
} # end POS position correction
###############################
head(d3)
#d5[1,2]
#write.csv(d3,"corpus/benjamin_db001.csv")
library(readxl)
dx<-read_xlsx("local/R/corpus/benjamin_db001.xlsx")
d3<-dx
colnames(d3)[1]<-"tok"
x<-stri_split_regex(d3$lemma,"-",simplify = T)
length(x[,2])
x[10,1]
d3$lemma<-x[,1]
m1<-grep("filename",d3$lemma)
d3$lemma[m1[2]]
m1
d3$cat[m1[1]:m1[2]-1]<-"buch1"
d3$cat[m1[2]:m1[3]-1]<-"buch2"
d3$cat[m1[3]:m1[5]-1]<-"buch3"
d3$cat[m1[5]:m1[6]-1]<-"buch4"
d3$cat[m1[6]:length(d3$cat)]<-"buch5"
m2<-grep("<g/>",d3$tok,invert = T)
d4<-d3[m2,]
m2<-grep("benjaminbuch[0-9].txt",d4$lemma,invert = T)
d4<-d4[m2,]
bookns<-colnames(d4)
cns<-c("tok","book","lemma","tag","cat","funct","case","pers","num","gender","tense","mode")
colnames(d4)<-cns
m<-d4[,1:12]==0
d4[m]<-NA
head(m)
sum(m)
length(m)
k<-1
bookns<-unique(d4$book)
setwd("~/boxHKW/UNI/21S/DH")
d6<-data.frame()
for (k in 1:length(bookns)){
  #a<-c(1,2,3,4,5)
  d5<-subset(d4,d4$book==bookns[k])
  m<-grep("(<s>|</s>)",d5$tok,invert = T)
  d5<-d5[m,]
  d5["tok_position"]<-1:length(d5$tok)
  #d13<-d12[a]
#  d13<-d12
  #getwd()
  ns<-paste0("local/R/corpus/xl6/",bookns[k],".xlsx")
#  write_xlsx(d5,ns)
d6<-rbind(d6,d5)
  }
getwd()
write.csv(d6,"benjamin_db_d6.csv")
write_xlsx(d3,"corpus/xl6/benjamin_db001.xlsx")
library(readxl)
dx<-read_xlsx("local/R/corpus/benjamin_db001.xlsx")
peppercall<-function(){
#  peppercon1<-"/Users/guhl/boxHKW/UNI/21S/DH/local/HU-LX/pepper/r-conxl1.pepper"
#  peppercon2<-"/Users/guhl/boxHKW/UNI/21S/DH/local/HU-LX/pepper/r-conxl2.pepper"
  
  peppercon11<-"~/boxHKW/UNI/21S/DH/local/R/corpus/r-conxl1.pepper"
  peppercon22<-"~/boxHKW/UNI/21S/DH/local/R/corpus/r-conxl2.pepper"
  #peppercon3<-"/Users/guhl/boxHKW/UNI/21S/DH/local/HU-LX/pepper/r-conxl3_cpt.pepper"
  pepperpath<-"/Users/guhl/pro/Pepper_2023/"
  #peppercon1<-"../r-conxl1.pepper"
  callpepper1<-paste0(pepperpath,"pepperStart.sh ",peppercon11)
  callpepper2<-paste0(pepperpath,"pepperStart.sh ",peppercon22)
  #lapsi
  callpepper1<-paste0("./pepperStart.sh ",peppercon11)
  callpepper2<-paste0("./pepperStart.sh ",peppercon22)
  
  #callpepper3<-paste0("./pepperStart.sh ",peppercon3)
  setwd(pepperpath)
  system(callpepper1) #cannot process 1+2 in one workflow file
  system(callpepper2)
  # system(callpepper3) #if directly converted .xls to annis theres no html display of text in ANNIS, but just a tokenized line, rest similar of annotation
  
  library(utils)
  annispath<-"/Users/guhl/boxHKW/UNI/21S/DH/local/R/corpus//"
  annisfiles<-list.files(annispath)
  zippath<-"/Users/guhl/boxHKW/UNI/21S/DH/local/HU-LX/pepper/anniszip"
  dir.create(zippath)
  nszip<-paste0(datetime,"_SES_annis_tagged_corpus.zip")
  zipfile<-paste(zippath,nszip,sep = "/")
  zip(zipfile = zipfile,paste(annispath,annisfiles,sep = "/"))
}

library(xml2)
p1<-read_xml(peppercon1)
p2<-read_xml(peppercon2)
getwd()
xlpath<-"xl6"
ttpath<-"ben_tt"
annisxp<-"ben_annis"
pimp<-xml_find_all(p1,"//importer")
pout<-xml_find_all(p1,"//exporter")
pa1<-xml_attr(pimp,"path")
pa2<-xml_attr(pout,"path")
xml_attr(pimp,"path")<-paste0("./",xlpath)
xml_attr(pout,"path")<-paste0("./",ttpath)
write_xml(p1,"corpus/r-conxl1.pepper")
pimp<-xml_find_all(p2,"//importer")
pout<-xml_find_all(p2,"//exporter")
pa1<-xml_attr(pimp,"path")
pa2<-xml_attr(pout,"path")
xml_attr(pimp,"path")<-paste0("./",ttpath)
xml_attr(pout,"path")<-paste0("./",annisxp)
write_xml(p2,"corpus/r-conxl2.pepper")

annispath<-paste("/Users/guhl/boxHKW/UNI/21S/DH/local/R/corpus/",annisxp,sep = "/")
annisfiles<-list.files(annispath)
zippath<-"/Users/guhl/boxHKW/UNI/21S/DH/local/R/corpus/anniszip"
dir.create(zippath)
nszip<-paste0(datetime,"_benjamin_tagged_corpus.zip")
zipfile<-paste(zippath,nszip,sep = "/")
zip(zipfile = zipfile,paste(annispath,annisfiles,sep = "/"))

###
getwd()
b1<-read_table("annis-export_Zeichen.csv")
mean(b1$`2_anno_default_ns::tok_position`)
plot(b1$`2_anno_default_ns::tok_position`,type = "h")
m<-which(d6$lemma=="Zeichen")
mean(d6$tok_position[m])
d6$tok[m[6]]
box1<-data.frame()
boxplot(d6$tok_position[m])
tlemma="Zeichen"
boxout<-function(tlemma){
box1<-data.frame()
d61<-subset(d6,d6$book=="buch1"&d6$lemma==tlemma)$tok_position
d62<-subset(d6,d6$book=="buch2"&d6$lemma==tlemma)$tok_position
d63<-subset(d6,d6$book=="buch3"&d6$lemma==tlemma)$tok_position
d64<-subset(d6,d6$book=="buch4"&d6$lemma==tlemma)$tok_position
d65<-subset(d6,d6$book=="buch5"&d6$lemma==tlemma)$tok_position
#box1<-boxplot(d61,d62,d63,d64,d65)
box1<-list(d61,d62,d63,d64,d65)

#box1d6$book=="buch1"][d6$tok_position[m]]
}
boxout_t<-function(ttok){
  box1<-data.frame()
  d61<-subset(d6,d6$book=="buch1"&d6$tok==ttok)$tok_position
  d62<-subset(d6,d6$book=="buch2"&d6$tok==ttok)$tok_position
  d63<-subset(d6,d6$book=="buch3"&d6$tok==ttok)$tok_position
  d64<-subset(d6,d6$book=="buch4"&d6$tok==ttok)$tok_position
  d65<-subset(d6,d6$book=="buch5"&d6$tok==ttok)$tok_position
  #box1<-boxplot(d61,d62,d63,d64,d65)
  box1<-list(d61,d62,d63,d64,d65)
  
  #box1d6$book=="buch1"][d6$tok_position[m]]
}

boxp<-boxout("Zeichen")
boxplot(boxp)
boxp<-boxout("sie")
#par(new=T)
boxplot(boxp,col = 3)
plot(boxp[[5]],type = "l")

boxp<-boxout_t("ich")
#par(new=T)
boxplot(boxp,col = 3)
plot(boxp[[5]],type = "l")

boxp

