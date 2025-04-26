library(httr)
library(xml2)
getwd()
# Set your Transkribus credentials
cred<-read.csv("~/boxHKW/21S/DH/local/R/cred_gener.csv")
m<-grep("transkribus",cred$q)
username <- "your_username"
password <- "your_password"
username<-cred$bn[m]
password<-cred$pwd[m]

# Authenticate and get an access token
# auth_url <- "https://transkribus.eu/TrpServer/rest/auth/login"
# auth_response <- POST(auth_url, body = list(user = username, pw = password))
# auth_xml <- read_xml(content(auth_response, "text"))
# 
# # Extract the access token
# access_token <- xml_text(xml_find_first(auth_xml, "//token"))
# 
# # Set the collection ID and document ID
# # yudale_xml-edited006, ID: 2917647
# # xmledited, ID: 411671

# collection_id <- 411671
# document_id <- 2917647


# 
# # Get the transcript file in XML format
# transcript_url <- paste0("https://transkribus.eu/TrpServer/rest/collections/",
#                          collection_id, "/", document_id, "/fulldoc")
# transcript_response <- GET(transcript_url, add_headers(Authorization = paste("Bearer", access_token)))
# 
# # Save the transcript to a file
# writeBin(content(transcript_response, "raw"), "TEI/api-export_transcript.xml")
# 
# Now you have the transcript in 'transcript.xml'

### NO.

#curl -X POST -H "Content-Type: application/x-www-form-urlencoded" -d "user=sampleuser&pw=samplepw" https://transkribus.eu/TrpServer/rest/auth/login


####
# library(httr)

# Define the URL
url <- "https://transkribus.eu/TrpServer/rest/auth/login"

# Create a list of parameters (user and pw)
params <- list(user = username, pw = password)

# Send an HTTP POST request
response <- POST(url, body = params, encode = "form",accept_xml())

# Extract the content from the response
content <- content(response, "text")

# Print the content (or handle it as needed)
# cat(content)
# library(xml2)
###
# Extract the access token
xml<-read_xml(content)
access_token <- xml_text(xml_find_first(read_xml(content), "//sessionId"))
###
# transcript_url<-"https://transkribus.eu/TrpServer/rest/collections/411671/list"
# transcript_url<-"https://transkribus.eu/TrpServer/rest/user/listMyDocs"
# transcript_url<-"https://transkribus.eu/TrpServer/rest/collections/411671/2917647/fulldoc.xml"
###
transcript_url<-"https://transkribus.eu/TrpServer/rest/collections/411671/2917647/5/curr"
transcript_url<-"https://files.transkribus.eu/Get?id=RDKCGHQCNKBZZQOJWZBSTWBZ"
transcript_url<-"https://files.transkribus.eu/Get?id=RDKCGHQCNKBZZQOJWZBSTWBZ"
# klopstock, abel
# gets xml of single page
klopstock<-7516113
iwanette<-7599198
doc_id<-iwanette
transcript_url<-"https://transkribus.eu/TrpServer/rest/collections/1973292/7516113/10/curr"
transcript_url<-paste0("https://transkribus.eu/TrpServer/rest/collections/1973292/",doc_id,"/fulldoc.xml")
# transcript_url<-"https://files.transkribus.eu/Get?id=KRADRBATBFQKQVXJZJWHDLFW" # page10, id fetched from above (json)
# transcript_url<-"https://files.transkribus.eu/Get?id=OVQQGHBEKEBTZSCUHCBFVTKM" # page10, id fetched from above (json) # no

transcript_response <- GET(transcript_url, add_headers(Authorization = paste("sessionId", access_token)))
jsonmeta<-tempfile("meta.json")
library(jsonlite)
metaxml<-read_xml(content(transcript_response, "text"))
all.pg<-xml_find_all(metaxml,"//pages")
all.keys<-xml_find_all(metaxml,"//pages/key")
all.keys
all.id<-xml_find_all(metaxml,"//pages/pageId")
xml_text(all.keys)
transcript_url<-paste0("https://files.transkribus.eu/Get?id=",xml_text(all.keys[10]))
transcript_url<-paste0("https://transkribus.eu/TrpServer/rest/collections/1973292/",xml_text(all.id[10]),"/fulldoc.xml")
k<-10
length(all.pg)
tx.list<-list()
for (k in 1:length(all.pg)){
transcript_url<-paste0("https://transkribus.eu/TrpServer/rest/collections/1973292/",doc_id,"/",k,"/curr")

transcript_response <- GET(transcript_url, add_headers(Authorization = paste("sessionId", access_token)))
json.df<-fromJSON(content(transcript_response,"text"),flatten = T)
trans.url<-json.df$url
transcript_response <- GET(trans.url, add_headers(Authorization = paste("sessionId", access_token)))
#cat(xml_text(read_xml(content(transcript_response,"text"))))
t.xml<-read_xml(content(transcript_response,"text"))
t.all<-xml_find_all(t.xml,"*")
xml_ns_strip(t.all)
t.lines<-xml_find_all(t.xml,"//TextLine")
t.tx<-unlist(xml_text(t.lines))
tx.list[[k]]<-t.tx
}
tx.text<-unlist(tx.list)
# writeLines(tx.text,"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/klopstock/klopstock_tod-abels_apiexpo.txt")
writeLines(tx.text,"~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/goue/goue_iwanette_apiexpo.txt")
# 15131.klopstock
#writeLines(content(transcript_response, "text"), "TEI/api-export_transcript.xml")
# writeLines(content(transcript_response, "text"), "~/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/klopstock/api-export_transcript.xml")

