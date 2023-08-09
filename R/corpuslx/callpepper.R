getwd()
# d1<-read_table("local/HU-LX/eval/annis-export_tagVFIN_G.csv")
# u1<-unique(d1$`3_anno_salt::lemma`)
# u1
# d2<-read_table("local/HU-LX/eval/annis-export_tagVFIN_T.csv")
# u2<-unique(d2$`3_anno_salt::lemma`)
# u2
# x<-u1%in%u2
# u1[!x]
# sum(x)
# x2<-u2%in%u1
# u2[!x2]
# sum(x2)
setwd("~/boxHKW/21S/DH/local/SPUND/corpuslx")
peppercon1<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/r-conxl1.pepper"
peppercon2<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/r-conxl2.pepper"
peppercon3<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/r-conxl3.pepper"
peppercon4<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/r-conxl4.pepper"
peppercon5<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/r-conxl5.pepper"

pepperpath<-"/Users/guhl/pro/Pepper_2023/"
#peppercon1<-"../r-conxl1.pepper"
callpepper1<-paste0("./pepperStart.sh ",peppercon1)
callpepper2<-paste0("./pepperStart.sh ",peppercon2)
callpepper3<-paste0("./pepperStart.sh ",peppercon3)
callpepper4<-paste0("./pepperStart.sh ",peppercon4)
callpepper5<-paste0("./pepperStart.sh ",peppercon5)

setwd(pepperpath)
system(callpepper1)
system(callpepper3)
system(callpepper4)
system(callpepper5)
system(callpepper2)

library(utils)
zippath<-"/Users/guhl/boxHKW/21S/DH/local/spund/corpuslx/testxls01"
#zippath<-"/Users/guhl/boxHKW/21S/DH/local/spund/corpuslx/testexb01annis"

annisfiles<-list.files(zippath)
nszip<-paste0(datetime,"_testxml_corpus.zip")
zipfile<-paste(zippath,nszip,sep = "/")

zip(zipfile = zipfile, files = paste(zippath,annisfiles,sep = "/"))
