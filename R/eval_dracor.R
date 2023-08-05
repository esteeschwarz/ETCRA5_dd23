#13266.dracor simple request
library(jsonlite)
einakter <- fromJSON("https://einakter.dracor.org/data.json")
#fetch dataset from dracor server
#set<-einakter
getwd()
wd<-"~/Documents/GitHub/ETCRA5_dd23/R"
setwd(wd)
m<-einakter$printed=="NULL"
sum(m)
einakter$printed[m]<-NA

#build dataframe of name in question
spitcast<-function(set,cast){
  ndf<-data.frame()
  m<-grepl(cast,set$cast)
  print(sum(m))
  m<-grep(cast,set$cast)
  s<-data.frame(author=set$author$name[m],year=unlist(set$printed[m]),title=set$title[m])
  print(s)
  return(s)
}

name_to_analyse<-"Lisette"
ndf<-spitcast(einakter,name_to_analyse)

#print out first 5 elements of dataframe
head(ndf)

#either:
library(writexl)
write_xlsx(ndf,"data/dracor_names-analysed_dataframe.xlsx")

#or:
write.csv(ndf,"data/dracor_names-analysed_dataframe.csv")
