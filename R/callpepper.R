getwd()
d1<-read_table("local/HU-LX/eval/annis-export_tagVFIN_G.csv")
u1<-unique(d1$`3_anno_salt::lemma`)
u1
d2<-read_table("local/HU-LX/eval/annis-export_tagVFIN_T.csv")
u2<-unique(d2$`3_anno_salt::lemma`)
u2
x<-u1%in%u2
u1[!x]
sum(x)
x2<-u2%in%u1
u2[!x2]
sum(x2)
setwd("~/Documents/GitHub/ETCRA5_dd23/R")
peppercon1<-"~/Documents/GitHub/ETCRA5_dd23/R/r-conxl1.pepper"
peppercon2<-"~/Documents/GitHub/ETCRA5_dd23/R/r-conxl2.pepper"
pepperpath<-"/Users/guhl/pro/Pepper_2023/"
#peppercon1<-"../r-conxl1.pepper"
callpepper1<-paste0("./pepperStart.sh ",peppercon1)
callpepper2<-paste0("./pepperStart.sh ",peppercon2)
setwd(pepperpath)
system(callpepper1)
system(callpepper2)
library(utils)
zippath<-"/Users/guhl/boxHKW/UNI/21S/DH/local/HU-LX/pepper/r-annis1"
annisfiles<-list.files(zippath)
nszip<-paste0(datetime,"_SES_annis_tagged_corpus.zip")
zipfile<-paste(zippath,nszip,sep = "/")
