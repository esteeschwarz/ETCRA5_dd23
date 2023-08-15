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

pepper.call<-function(peppercon,zippath,nszip){
wd.act<-getwd()
#setwd("~/boxHKW/21S/DH/local/SPUND/corpuslx")
peppercon1<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/r-conxl1.pepper"
peppercon2<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/r-conxl2.pepper"
peppercon3<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/r-conxl3.pepper"
peppercon4<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/r-conxl4.pepper"
peppercon5<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/r-conxl5.pepper"
peppercon6<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/r-conxl6.pepper"
peppercon7<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/r-conxl7.pepper" # tt > annis
peppercon8<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/r-conxl8.pepper" # exb > tt

#13334.global
#peppercon<-peppercon8

pepper.where<-grep("epper",list.files("/users/guhl/pro"))
pepperpath<-"/Users/guhl/pro/Pepper_2023/"
pepperpath<-paste0("/Users/guhl/pro/",list.files("/users/guhl/pro")[pepper.where])
#peppercon1<-"../r-conxl1.pepper"
callpepper1<-paste0("./pepperStart.sh ",peppercon1)
callpepper2<-paste0("./pepperStart.sh ",peppercon2)
callpepper3<-paste0("./pepperStart.sh ",peppercon3)
callpepper4<-paste0("./pepperStart.sh ",peppercon4)
callpepper5<-paste0("./pepperStart.sh ",peppercon5)
callpepper6<-paste0("./pepperStart.sh ",peppercon6)
callpepper7<-paste0("./pepperStart.sh ",peppercon7)
callpepper8<-paste0("./pepperStart.sh ",peppercon8)

callpepper<-paste0("./pepperStart.sh ",peppercon)


setwd(pepperpath)
# system(callpepper1)
# system(callpepper3)
# system(callpepper4)
# system(callpepper5)
# system(callpepper2)
# system(callpepper6)
# system(callpepper7)
# system(callpepper8)

system(callpepper)

library(utils)
# zippath<-"/Users/guhl/boxHKW/21S/DH/local/spund/corpuslx/dd23annis.2"
# zippath<-"/Users/guhl/boxHKW/21S/DH/local/spund/corpuslx/exbtt"
#zippath<-"/Users/guhl/boxHKW/21S/DH/local/spund/corpuslx/testexb01annis"

annisfiles<-list.files(zippath)
#nszip<-paste0(datetime,"_testxml_corpus.zip")
zipfile<-paste(zippath,nszip,sep = "/")


setwd(wd.act)
}

zipannis<-function(zippath,nszip){
  annisfiles<-list.files(zippath)
  #nszip<-paste0(datetime,"_testxml_corpus.zip")
  zipfile<-paste(zippath,nszip,sep = "/")
  zip(zipfile = zipfile, files = paste(zippath,annisfiles,sep = "/"))
  
}
