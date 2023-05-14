library(xml2)
library(stringi)
library(Hmisc)
getwd()
d1<-read_xml("~/documents/github/HU-LX/data/transkribus_SES_BS609_001.xml")
data<-d1
data%>%xml_ns_strip()
xtext<-xml_find_all(data,".//TextLine/Coords")
xbase<-xml_find_all(data,".//TextLine/Baseline")
xtext
ptext1st<-xml_attr(xtext[1],"points")
pbase1st<-xml_attr(xbase[1],"points")
ptextlast<-xml_attr(xtext[4],"points")
pbaselast<-xml_attr(xbase[4],"points")

###
#set corners manually:
marginleft<-268
marginright<-2388 #generic
basefirst<-182
baselast<-3366
nr1st<-1484
nrlast<-1547
nrows<-nrlast-nr1st
#compute same values
vtextline1st<-stri_split(ptext1st,regex = " ")
vtextlinelast<-stri_split(ptextlast,regex = " ")
vbaseline1st<-stri_split(pbase1st,regex = " ")
vbaselinelast<-stri_split(pbaselast,regex = " ")
###
getpoint<-function(x)stri_split(x,regex = ",")
vtextline1st<-lapply(vtextline1st,getpoint)
vtextlinelast<-lapply(vtextlinelast,getpoint)
vbaseline1st<-lapply(vbaseline1st,getpoint)
vbaselinelast<-lapply(vbaselinelast,getpoint)
vtextline1st[[1]][[2]][1]
vtextline1st
vtextlinelast
###
# names(vtextline1st[])<-""
# names(vtextline1st[[1]][[1]][1])<-"x"
# names(vtextline1st[[1]][[1]][[1]][2])<-"y-marginleft"
# names(vtextline1st[[1]][1])<-"marginleft"
# names(vtextline1st[[2]])<-"marginright"
# 
#names(vtextline1st[[1]][[2]])<-"marginright"
###
mn1sty<-sum(as.double(vtextline1st[[1]][[1]][2]),as.double(vtextline1st[[1]][[2]][2]))/2
mnlasty<-sum(as.double(vtextlinelast[[1]][[1]][2]),as.double(vtextlinelast[[1]][[2]][2]))/2
cut(c(mn1sty:mnlasty),nrows)

