# info

## visualisation:
[https://ada-sub.dh-index.org/shiny/work/essais/shtern/](https://ada-sub.dh-index.org/shiny/work/essais/shtern/)

## method
looping:
R: `ex<-stri_extract_all_regex(texts[[k]],paste0("(\\b",year,"\\b)"))`
- texts: dataframe of 2839 text from shtern journal.  
- k: number of text.  
- year: the year in question (1901-1940) that is queried in the text.  

so the regexformula results in `\b1901\b` for the query e.g. for the year 1901 and is looped over the years.