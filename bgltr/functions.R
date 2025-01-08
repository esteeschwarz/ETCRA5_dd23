update_wikisource_page <- function(page_title, content, username, password) {
  login_url <- "https://en.wikisource.org/w/api.php?action=login&format=json"
  login_url <- "https://de.wikisource.org/w/api.php?action=login&format=json"
  login_response <- POST(login_url, body = list(lgname = username, lgpassword = password))
  login_token <- content(login_response)$login$token
  # Get edit token
  edit_token_url <- "https://de.wikisource.org/w/api.php?action=query&meta=tokens&format=json"
  edit_token_response <- GET(edit_token_url, set_cookies(login_response$cookies$value))
  edit_token <- content(edit_token_response)$query$tokens$csrftoken
#  edit_url <- "https://de.wikisource.org/w/api.php?action=parse&format=json"
  edit_url <- "https://de.wikisource.org/w/api.php?action=edit&format=json"
  edit_response <- POST(edit_url, body = list(
    title = page_title,
    #page = page_title,
    text = content,
    token = edit_token
  ), set_cookies(login_response$cookies$value))
  
  return(content(edit_response))
}
template
page.edit<-function(page,template){
  t<-readLines(page)
  m<-grepl("<temptext/>",template)
  template<-append(template,t,after = which(m))
  template<-template[!m]
  template<-paste(template,collapse = "<br>")
  p.nr<-strsplit(page,"\\.")[[1]][2]
  template<-gsub("<temppagenr/>",p.nr,template)
  wiki.ns<-paste0("Seite:Steltzer_montenegro.pdf/",p.nr)
  return(list(content=template,ns=wiki.ns))
}
page.edit(fns[5],template)
