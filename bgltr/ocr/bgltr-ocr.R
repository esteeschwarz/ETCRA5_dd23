#20241110(09.08)
#15462.steltzer,montenegro,ocr
#dd25, brgl. trauerspiel
########################
prelim_fun<-function(){
library(tesseract)
png<-paste("png",list.files("png"),sep = "/")
tesseract_download("deu")
png[6]
t<-ocr(png[6],engine = tesseract("deu"))
t
writeLines(t,"text/t14-tess-deu.txt")
#kraken model:
"~/boxHKW/21S/DH/local/OCR/models/german_print_best.mlmodel"
system("kraken -f pdf -i steltzer_montenegro.pdf steltzer-text.txt segment -bl ocr -m ~/boxHKW/21S/DH/local/OCR/models/german_print_best.mlmodel") # output to files, ...txt obsolete
t.dir<-"steltzer-text"
texts<-list.files(t.dir)
texts.d<-paste(t.dir,texts,sep = "/")
t.db<-lapply(texts.d, readLines)
names(t.db)<-texts
t.db[1]
x<-t.db
put.names<-function(x)c(names(x),x)
t.db.n<-lapply(t.db, put.names)
c(names(x[1]),t.db[1])
t.db.n<-mapply(c,texts,t.db)
t.db.n[1]
t.db.u<-unlist(t.db.n)
#writeLines(t.db.u,"actuel/steltzer(1781)_franziska-montenegro.txt")
}
###################################################################
# process text
# regex
rgdf<-data.frame(regex=NA,replace=NA)
rgdf[1,1]<-"(.+er Auftrit.)"
rgdf[1,2]<-'#\\1'

process_1<-function(rgdf){
# rgdf<-data.frame(regex=NA,replace=NA)
# rgdf[1,1]<-"(.+er Auftrit.)"
# rgdf[1,2]<-'#\\1'
# t.db.m1<-gsub(rgdf[1,1],rgdf[1,2],t.db.u)
#writeLines(t.db.m1,"actuel/steltzer(1781)_franziska-montenegro.txt")
# edit castlist manually, read in again
t.db.e<-readLines("actuel/steltzer(1781)_franziska-montenegro.txt")
m<-grep("0004$|0005$",t.db.e)
t.db.e[m[2]]
t.cast<-t.db.e[m[1]:m[2]]
#t.cast[6]<-paste0(t.cast[6],t.cast[7])
#t.cast<-t.cast[4:15]
library(stringi)
t.cast.p<-stri_split_regex(t.cast,",",simplify = T)
t.cast.e<-t.cast.p[4:14,1]
t.cast.e<-gsub("\\.","",t.cast.e)
t.cast.e
rgdf[2:(1+length(t.cast.e)),1]<-t.cast.e
rgdf
get.sp<-function(x)grep(x,t.db.e)

###############
t.db.m2<-t.db.e
for(k in 1:length(rgdf[2:(1+length(t.cast.e)),1])){
  m.st<-k+1
t.db.m2<-gsub(paste0("(^",rgdf[m.st,1],")(\\. )"),"@\\1:\n",t.db.m2)
}
t.db.m2[20:200]
#writeLines(t.db.m2,"actuel/steltzer(1781)_franziska-montenegro.txt")
rgdf[14,1]<-"#(.+trit)"
rgdf[14,2]<-"## \\1"
t.db.m2<-gsub(rgdf[14,1],rgdf[14,2],t.db.m2)
rgdf[15,1]<-"(.+fzug)"
rgdf[15,2]<-"# \\1"
t.db.m2<-gsub(rgdf[15,1],rgdf[15,2],t.db.m2)
#writeLines(t.db.m2,"actuel/steltzer(1781)_franziska-montenegro.txt")
m<-grep(rgdf[14,1],t.db.m2)
t.db.m2[m]
m1<-grep(rgdf[15,1],t.db.m2) #Aufzuge (acts)
rgdf[16,1]<-"@"
rgdf[16,2]<-"## \\1"
m2<-grep(rgdf[16,1],t.db.m2)
sum(m2,na.rm = T)
rgdf[17,1]<-"##"
m3<-grep(rgdf[17,1],t.db.m2) # auftritt
#t.db.m2<-gsub(rgdf[14,1],rgdf[14,2],t.db.m2)
### get (stage) for acts
# get first higher speaker than act
stagelines<-array()
w<-1
for (act in m1){
if(stage.s<act){
  #while(w==1){
  for(sc in m3){
#  for(m in m2){
   # while(m<k){
  if(sc>act){
    stage.s<-sc+1
    #stage.e<-sc-1
    # t.db.m2[stage.s]<-paste0("$ ",t.db.m2[stage.s],collapse = "")
   # w<-0
  }
    t.db.m2[stage.s]<-paste0("$ ",t.db.m2[stage.s],collapse = "")
  }
  } 
#  stage.s<-1
  }

#}
t.db.m2[stage.s]
t.db.m2[111]
t.db.m2[973]
m3
return(rgdf)
#writeLines(t.db.m2,"actuel/steltzer(1781)_franziska-montenegro.txt")
}
### stage 2, manually edited transcript. not save over!
###########
# now try ezdrama 1st run
ezd_markup_text<-"actuel/steltzer_franziska-montenegro.txt"
library(reticulate)
#use_miniconda(miniconda_path())
#py_available()
reticulate::conda_list()
use_miniconda(reticulate::conda_list()[6,2])
#py_version()
#miniconda_path()
#######################################################
### run ezdrama >
system(paste0("python parser.git.py ",ezd_markup_text))
#######################################################
### not yet
t.db.m3<-readLines(ezd_markup_text)
rgdf[17,1]<-"\\+#nl\\+"
rgdf[17,2]<-"\n"
m<-grep(rgdf[17,1],t.db.m3)
t.db.m3<-gsub(rgdf[17,1],rgdf[17,2],t.db.m3)
t.db.m3[105]
#writeLines(t.db.m3,"actuel/steltzer(1781)_franziska-montenegro.txt")
#writeLines(t.db.m3,ezd_markup_text)
t.db.m3<-readLines(ezd_markup_text)
rgdf[18,1]<-".+pdf_0000(.+$)" 
rgdf[18,2]<-"\\1:"
#m<-grep(rgdf[17,1],t.db.m3)
t.db.m3<-gsub(rgdf[18,1],rgdf[18,2],t.db.m3)
writeLines(t.db.m3,ezd_markup_text)
