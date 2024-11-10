#20241110(09.08)
#15462.steltzer,montenegro,ocr
#dd25, brgl. trauerspiel
########################
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
writeLines(t.db.u,"actuel/steltzer(1781)_franziska-montenegro.txt")
###################################################################



