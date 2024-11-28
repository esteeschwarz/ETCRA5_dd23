# 20241127(20.52)
# 15485.get.ETCRA5.fellows&events
############################################

# get epub
init<-function(){
  library(RSelenium)
  #library(rvest)
  library(httr)
  library(xml2)
  library(netstat)
  library(wdman)
  library(binman)
  #library(utils)
  #library(stringi)
  #library(clipr)
  #library(jsonlite)
  rd<-rsDriver(browser = "firefox",port = free_port())
  remdr<-rd$client
  #remdr$navigate(site.art)
  #remdr$navigate("https://zeit.de")
  #remdr$navigate(page)
  return(remdr)
}
remdr<-init()
year<-2024
man<-paste0("https://www.temporal-communities.de/events/",year,"/index.html")
page.navi<-man
remdr$navigate(page.navi)
x<-GET(page.navi)
r<-content(x,"text")
htm<-read_html(r)
boxevents<-xml_find_all(htm,"/html/body/div/div[4]/div[2]/div/main/div/div")
allevents<-xml_find_all(boxevents,"//a")
event.1.xp<-"/html/body/div/div[4]/div[2]/div/main/div/div/div[2]/div[1]/div[1]/h3/a"
event.2.xp<-"/html/body/div/div[4]/div[2]/div/main/div/div/div[2]/div[2]/div[1]/h3/a"
event.3.xp<-"/html/body/div/div[4]/div[2]/div/main/div/div/div[3]/div"
all.a<-xml_find_all(boxevents,"//h3/a")
all.xp<-xml_path(all.a)
all.xp.3<-xml_path(xml_find_all(boxevents,event.3.xp))
k<-1
kstart<-1 #14412.48,49 no, 2021/2 stop danach, 15422.36.finished
event.list<-list()
event.df<-data.frame(id=1:length(all.xp),date=NA,event=NA,text=NA)
events.past.xp<-'//*[@id="button_past_events"]'
df.start<-12
df.end<-df.start+length(all.xp.3)
for (k in df.start:df.end){
  xp<-all.xp.3[[k]]
  xp
  k
  #man<-paste0("https://epaper.zeit.de/abo/diezeit?title=diezeit&issue=0",k,"&year=2018")
  #man<-paste0("https://epaper.zeit.de/abo/diezeit?title=diezeit&issue=",k,"&year=2018")
#  issue<-k
 # if(k<10)
  #  issue<-paste0("0",k)
 # man<-paste0("https://epaper.zeit.de/abo/diezeit?title=diezeit&issue=",issue,"&year=",year)
#  cat("navigating to:",man,"\n")
 # remdr$navigate(man)
  Sys.sleep(4)
  cat("solving issue",k,"\n")
  #css2<-paste0("#issue > option:nth-child(",k,")")
  #com_button<-remdr$findElement(using = "id",id1)
  #com_button$clickElement()
  #Sys.sleep(8)
  #print(1)
  #dropdown<-remdr$findElement(using = "id",id1)
  #dropdown
  #com_button<-dropdown$findElement(using = "xpath", xp1)
  #com_button$clickElement()
  #Sys.sleep(8)
  #print(2)
  #com_button<-remdr$findElement(using = "css selector", css1)
  #com_button$clickElement()
  #dropdown<-remdr$findElement(using = "id", id2)
  #com_button<-dropdown$findElement
  #com_button<-dropdown$findElement(using = "xpath", xp2)
  #com_button$clickElement()
  #Sys.sleep(8)
  #print(3)
  #com_button<-remdr$findElement(using = "css selector", css2)
  #com_button$clickElement()
  #Sys.sleep(8)
  #print(4)
  #com_button<-remdr$findElement(using = "css selector", css3)
  #com_button$clickElement()
  #Sys.sleep(8)
  #print(5)
  #safari:
 # css4<- '#content > section.page-section.page-section-archives.js-archives-section > div.row.margin-bottom-20px.archives-filter-results.js-archives-filter-results > div:nth-child(1) > div > div.epaper-cover > a'
  event<-xp
  com_button<-remdr$findElement(using = "xpath", xp)
  cat("finding:",xp,"\n")
  
  com_button$clickElement()
  Sys.sleep(5)
  print(6)
  art.src<-remdr$getPageSource()
  art.htm<-read_html(art.src[[1]])
  art.p<-xml_find_all(art.htm,"/html/body/div/div[4]/div[2]/div/main/div/div/div[2]")
  art.tx<-xml_text(art.p)
  
  art.h<-xml_find_all(art.htm,"/html/body/div/div[4]/div[2]/div/main/div/div/div[1]/div[1]") 
  art.ttl<-xml_text(art.h)
  art.d<-xml_find_all(art.htm,"/html/body/div/div[4]/div[2]/div/main/div/div/div[1]/div[2]")
  art.date<-xml_text(art.d)
  art.date<-gsub(" \\|.+","",art.date)
  art.date<-strptime(art.date,"%B %d, %Y")
  event.df$date[k]<-as.character(art.date)
  event.df$event[k]<-art.ttl
  event.df$text[k]<-art.tx
  remdr$goBack()
}
# stuck...
write.csv(event.df,"RA-events.csv")
