## stylo001
# stylometry
with the R *stylo* package you are able to perform stylometric analyses with text corpora.  
find here: [https://fortext.net/routinen/methoden/stilometrie][1] a detailed description on the subject.   
#### prerequisites
install R and the stylo package according to section [2.7][2]
#### process
sample script:
```r
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


```

![][image-1]

#### further processing
sample [GEPHI][3] output of the stylo-generated data. [`in-out_CA_100_MFWs_Culled_0__Classic Delta_EDGES.csv`]

![][image-2]

[1]:	https://fortext.net/routinen/methoden/stilometrie
[2]:	pre007.md
[3]:	https://gephi.org

[image-1]:	RPlot_001.png
[image-2]:	gephiexpo_001.png