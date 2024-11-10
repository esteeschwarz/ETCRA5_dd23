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