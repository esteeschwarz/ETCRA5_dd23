
peppercon1<-"/Users/guhl/Documents/GitHub/ETCRA5_dd23/R/corpuslx/r-conxl3_cpt.pepper"
#peppercon2<-"/Users/guhl/Documents/GitHub/HiSon/grammar/r-conxl7.pepper"
#zippath<-"/Users/guhl/Documents/GitHub/ETCRA5_dd23/R/corpuslx/dd23.2.2"
zippath<-paste(getwd(),"annis",sep = "/")
f<-list.files("dd23_17.1")

#nszip<-"philotas-annis_v1.0.zip"
nszip<-"dd23_17.1_annis.zip"

source("/users/guhl/boxhkw/21s/dh/local/spund/corpuslx/callpepper_global.R")

pepper.call(peppercon = peppercon1,zippath = zippath,nszip = nszip)
#pepper.call(peppercon = peppercon2,zippath = zippath,nszip = nszip)
zipannis(zippath = zippath,nszip = nszip)
