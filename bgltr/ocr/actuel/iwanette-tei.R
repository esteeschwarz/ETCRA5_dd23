#20250320(17.29)
#15126.klopstock-abel.TEI
#########################
t1<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/klopstock/klopstock_tod-abels.txt")
t1<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/goue/txt/goue_iwanette_ggl.txt")
# to fetch transcript text from transkribus API:
source("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/transkribus-api.R")
t1<-readLines("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/goue/goue_iwanette_apiexpo.txt")
#t1<-tx.text # generated above
# i made few changes corrections/adaptations in the t1 static file to make this script run fluently, so modifications in the transkribus editor make no sense at the moment.
ezd_markup.ns<-paste0("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/klopstock/klopstock_tod-abels_ezd")
ezd_markup.ns<-paste0("~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/goue/goue_iwanette_ezd")
ezd_markup_text<-paste0(ezd_markup.ns,".txt")
ezd_markup.temp<-tempfile("ezdtemp.txt")
#library(readtext)
# m1<-grep("¬",t1)
# m1p<-m1+1
# tm1<-paste0(t1[m1],t1[m1p])
# t2<-t1
# t2[m1]<-tm1
# m2<-1:length(t2)%in%m1p
# t2<-t2[!m2]
# t2[1]
t2<-t1
#### 15175.iwanette
# preprocess
# umlaute:
lx<-548
t2[lx]
#library(stringi)
ltint<-utf8ToInt("Gl--cks")
utf8ToInt("u")
intToUtf8(868)
utf8ToInt("zZ")
utf8ToInt("ß")
utf8ToInt("aA")
utf8ToInt("bB")
utf8ToInt("äaöoüuÄAÖOÜU")
umrepl<-c(A="Ä",O="Ö",U="Ü",a="ä",o="ö",u="ü")
umrepl
library(purrr)
ltx<-unlist(strsplit(t2[lx]," "))
ltx
ltint<-utf8ToInt(ltx[7])
ltint
lim<-252+1
mu<-ltint>lim
mu
mup<-which(mu)-1
x<-umrepl[6]
outlist<-list()
x
umrepl
lti2<-utf8ToInt(t2[548])
lti2
k<-548
t3<-t2
k<-126
x
r<-6
r
k
mp<-4
mp<-32
for(r in 1:length(umrepl)){
  x<-umrepl[r]
  for (k in 1:length(t3)){
    ltint<-utf8ToInt(t3[k])
    mu<-ltint>lim
    mu
    mup<-which(mu)-1
    u<-intToUtf8(ltint[mup])
    u<-unlist(strsplit(u,""))
    u
am<-u==names(x)|u==x
u<-u[am]
am<-u==names(x)|u==x
if(sum(am)>0) {
  mp
for (mp in mup){  
if(ltint[mp+1]!=1000&(intToUtf8(ltint[mp])%in%names(x)|intToUtf8(ltint[mp])%in%x)) {
ltint[mp]<-utf8ToInt(x)
ltint[mp+1]<-1000
}
}
mout<-ltint==1000
lt2<-ltint[!mout]
t3[k]<-intToUtf8(lt2)
  }
}
  
  }

t3[548:570]
utf8ToInt(t3[551])
#writeLines(t2,ezd_markup_text)
writeLines(t3,ezd_markup_text)
#outlist
ltint
mout<-ltint==1000
lt2<-ltint[!mout]
intToUtf8(lt2)
m3<-grep("#f",t2)
title<-unlist(strsplit(t2[m3],"\\."))
title<-gsub("(^ )|( $)","",title)
title[1]<-"Margarete Klopstock"
title<-paste0(c("@author ","@title ","@subtitle "),title)
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
tbody<-(m9[1]+1):length(t4)
m12<-grep(char.gr,t4[tbody])
m12<-grepl(char.gr,t4[tbody])
t4[tbody][!m12]<-paste0(t4[tbody][!m12],"3p")
writeLines(t4,ezd_markup.temp)

t5<-readtext(ezd_markup.temp)$text
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
m14<-grep("[0-9]{1,3}:: ",xml.tx)
xml.tx[m14]<-gsub("([0-9]{1,3}::) ","\\1",xml.tx[m14])
m14<-grep("[0-9]{1,3}::",xml.tx)
xml.tx[m14]<-gsub("([0-9]{1,3})::",'<pb n="\\1"/>',xml.tx[m14])
m15<-grep("(\\[)",xml.tx)
xml.tx[m15]
xml.tx[m15]<-gsub("\\[","(",xml.tx[m15])

m15<-grep("(\\])",xml.tx)
xml.tx[m15]
xml.tx[m15]<-gsub("\\]",")",xml.tx[m15])
xml.tx[m15]
writeLines(xml.tx,xml.ns)
xml.final<-"~/Documents/GitHub/ETCRA5_dd23/tei/klopstock_tod-abels.final.xml"
writeLines(xml.tx,xml.final)
