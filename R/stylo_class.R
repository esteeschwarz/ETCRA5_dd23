#13352.ETCRA5.stylometry essai
#20230827(10.43)
################

library(stylo)
wd<-"your-working-directory" 
# > where output files are saved. contains a directory "corpus" with the texts to analyse. adapt to your path. you can navigate in the <files>-view to your preferred folder and choose: <set as working directory> in the <wheel context-menu>
#wd<-getwd()
setwd(wd)
x<-stylo(mfw.min = 100,mfw.max = 100,analysis.type = "CA",output="screen")
x$table.with.all.freqs
