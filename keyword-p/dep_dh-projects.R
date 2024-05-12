#202411(17.48)
#14201.ETCRA5.keywords
#title: data and the 404 status code
####################################
library(xml2)
library(httr)
src<-"~/Documents/GitHub/ETCRA5_dd23/OES/13123.ada.pinghook.html"
htm<-read_html(src)
alla<-xml_find_all(htm,".//a")
###
alla[1]
k<-1
responsetable<-data.frame(id=1:length(alla),link=NA,href=NA,code=NA)
for (k in 240:length(alla)){
href<-xml_attr(alla[k],"href")
link<-xml_text(alla[k])
responsetable$id[k]<-k
responsetable$link[k]<-link
responsetable$href[k]<-href
cat(k,"\n")
x<-GET(href)
t<-content(x,"text")
t
responsetable$code[k]<-x$status_code
}
out<-c(206,208,162,148,45,9)
library(clipr)
write_clip(responsetable$link[out])
###################################
# the following projects are deprecated:
#synergiewissen, zflprojekte: on a different domain
### not findable: >
#kleist archiv
#GIB: glossar der bildphilosophie, tübingen
#denkmaldatenbank, berlin
#TEI conversion, tei-c.org
#TWL, wholelifeacademy, hkw, berlin

# that is a 
5/277
# key of DH projects vanishing since 2019-20.
