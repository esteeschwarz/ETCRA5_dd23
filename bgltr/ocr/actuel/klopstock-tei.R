#20250320(17.29)
#15126.klopstock-abel.TEI
#########################
t1<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/klopstock/klopstock_tod-abels.txt")
ezd_markup.ns<-paste0("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/klopstock/klopstock_tod-abels_ezd")
ezd_markup_text<-paste0(ezd_markup.ns,".txt")
ezd_markup.temp<-tempfile("ezdtemp.txt")
# m1<-grep("¬",t1)
# m1p<-m1+1
# tm1<-paste0(t1[m1],t1[m1p])
# t2<-t1
# t2[m1]<-tm1
# m2<-1:length(t2)%in%m1p
# t2<-t2[!m2]
# t2[1]
t2<-t1
m3<-grep("#f",t2)
title<-unlist(strsplit(t2[m3],"\\."))
title<-gsub("(^ )|( $)","",title)
title[1]<-"Margarete Klopstock"
title<-paste0(c("@author: ","@title: ","@subtitle: "),title)
title
t3<-c(title[c(2,3,1)],t2[(m3+1):length(t2)])
head(t3,20)
m4<-t3==""
t3<-t3[!m4]
head(t3,20)
m6<-grep("\\^",t3)
m7<-grep("#",t3)
t3[(m6+1):(m7[1]-1)]
pers<-t3[(m6+1):(m7[1]-1)]
pers<-gsub("(\\.)|(^ )|( $)","",pers)
pers
t3[(m6+1):(m7[1]-1)]<-pers
t4<-t3
head(t4,50)
pers.m<-paste0(pers,".")
pers.m<-c(pers.m,"Beyde.")
m11<-t4%in%pers.m
t4[m11]<-paste0("@",t4[m11])
# for (n in pers){
#   gn<-paste0("(^",n,"\\.","$)")
#   m8<-grep(gn,t4)
#   t4[m8]<-paste0("@",t4[m8])
# }
head(t4,50)
m9<-grep("#",t4)
t4[m9]
t4<-gsub("¬\n","&lb",t4)
t4<-gsub("¬","&lb",t4)
writeLines(t4,ezd_markup.temp)

t5<-readtext(ezd_markup.temp)$text
t5<-gsub("(&lb)\n","",t5)
#t5<-gsub("(3p)\n"," ",t5)

writeLines(t5,ezd_markup.temp)
t4<-readLines(ezd_markup.temp)
t4<-gsub("#[(]#?","[",t4)
t4<-gsub("#[)]#","]",t4)
m10<-grep("#o",t4)
t4[m10]
m10<-grepl("#o",t4)
t4<-t4[!m10]
#t4<-gsub("^[(](.+)[)]","$\\1",t4)
char.s<-"[#@$^]"
char.gr<-paste0("^",char.s)
m12<-grep(char.gr,t4)
m12<-grepl(char.gr,t4)
t4[!m12]<-paste0(t4[!m12],"3p")
writeLines(t4,ezd_markup_text)

t5<-readtext(ezd_markup_text)$text
t5
t5<-gsub(paste0("(3p)\n(?!",char.s,")")," ",t5,perl = T)
t5<-gsub("(@.+\\.)\n(\\(.+\\))","\\1\\2",t5)
t5<-gsub("(3p)"," ",t5)

writeLines(t5,ezd_markup_text)

##########
process.ezd<-function(){
  path.local.home<-"~/Documents/GitHub/dybbuk-cor"
  system(paste0("python3 ",path.local.home,"/convert/actuel/","parser.git.py ",ezd_markup_text))
  print("finished python ezd")
} #end ezd process .txt
########################
# perform ezd processing
process.ezd()
########################
xml.ns<-paste0(ezd_markup.ns,".xml")
# read in ezd output .xml
xml.tx<-readLines(xml.ns)
