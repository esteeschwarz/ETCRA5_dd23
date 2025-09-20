


get.heads.2<-function(t1,vario,h1.all,h2.all,numer.all){
  m1<-grep(h1.all,t1)
  m2<-grep(h2.all,t1)
  m3<-grep(numer.all,t1)
  #t1[m3]
  #numer.all
  ma<-c(m1,m2)
  m1x<-m3%in%ma
  t1[m3][m1x]
  h3<-t1[m3][m1x]
  htemp<-tempfile("heads.txt")
  t2<-t1
  t2[m3][m1x]<-paste0("%#% ",t2[m3][m1x])
  t2[m3][m1x]<-gsub(", ","\n%##% ",t2[m3][m1x]) # seperate ACT from scene declaration
  writeLines(t2,htemp)
  t2<-readLines(htemp)
  #t2[m3][m1x]
  m4<-grep("%#+%",t2)
  m5<-grep("%#%",t2)
  m6<-duplicated(t2[m5])
  print("remove duplicated acts...")
  print(m6)
  t2[m5][m6]<-""
  vario<-t2[m4]
  vario
  vario<-gsub("%","",vario)
  vario
  t2[m4]<-gsub("%","",t2[m4])
  t2
  return(list(text=t2,vario=vario,h1.first=m5[1]))
}


get.heads.3<-function(t1,vario,h1.all,h2.all,numer.all){
  m0<-grep(paste0(vario,collapse = "|"),t1)
  m1<-grep(h1.all,t1[m0])
  m2<-grep(h2.all,t1[m0])
  m3<-grep(numer.all,t1[m0])
  #t1[m3]
  #numer.all
  ma<-c(m0[m1],m0[m2])
  m3<-m0[m3]
  m1x<-m3%in%ma
  vario[m3][m1x]
  h3<-t1[m3][m1x]
  htemp<-tempfile("heads.txt")
  t2<-t1
  t2[m3][m1x]<-gsub("#","%#% ",t2[m3][m1x])
  t2[m3][m1x]<-paste0("%#%",t2[m3][m1x])
  #  t2[m3][m1x]<-paste0("%#% ",t2[m3][m1x])
  t2[m3][m1x]<-gsub(", ","\n%##% ",t2[m3][m1x]) # seperate ACT from scene declaration
  writeLines(t2,htemp)
  t2<-readLines(htemp)
  #t2[m3][m1x]
  m4<-grep("%#+%",t2)
  m5<-grep("%#%",t2)
  m6<-duplicated(t2[m5])
  print("remove duplicated acts...")
  print(m6)
  # t2[m5][m6]<-"removed"
  vario<-t2[m4]
  vario
  vario<-gsub("%","",vario)
  vario
  t2[m4]<-gsub("%","",t2[m4])
  t2
  return(list(text=t2,vario=vario,h1.first=m5[1],mw=m4))
}

get.heads.4<-function(headx.1,headx.2){
  metadf<-read.csv(paste0(Sys.getenv("GIT_TOP"),"/ETCRA5_dd23/bgltr/dracorTEI/lx/metadf-mlx.csv"))
                   numer<-metadf$cardinal
                   #########################
                   do.caps<-function(numer){
                     numer.s<-unlist(strsplit(numer,"\\|"))
                     numer.s<-gsub("[)(]","",numer.s)
                     numer.s<-numer.s[numer.s!=""]
                     numer.s<-numer.s[!is.na(numer.s)]
                     numer.c<-capitalize(numer.s)
                     numer.x<-gsub("[A-ZВ-Ш]",".",numer.c)
                     numer.x
                     # numer.dc<-decapitalize(numer.s)
                     
                     numer.tu<-toupper(numer.s)
                     numer.tu.x<-toupper(numer.x)
                     # n.all<-c(numer.s,numer.c,numer.dc,numer.tu,numer.x,numer.tu.x)
                     n.all<-c(numer.s,numer.c,numer.tu,numer.x,numer.tu.x)
                     n.all<-unique(n.all)
                     numer.all<-paste0("\\b",n.all,"",collapse = "|")
                     # numer.all<-paste0(n.all,collapse = "|")
                     length(numer.all)
                     return(numer.all)
                   }
                   numer.all<-do.caps(numer)
                   #h1.all<-do.caps(h1)
                   #h1<-paste0("(",paste0(metadf$h1,collapse = "|"),")")
                   #h2<-paste0("(",paste0(metadf$h2,collapse = "|"),")")
                   h1<-metadf$h1
                   h2<-metadf$h2
                   h1<-c(h1,headx.1)
                   h2<-c(h2,headx.2)
                   h1.all<-do.caps(h1)
                   h2.all<-do.caps(h2)
                   h1.all
                   rm(h1)
                   rm(h2)
                   numer<-paste0(numer,paste0(1:20,"."),collapse = "|")
                   numer<-paste0(numer,paste0(c("I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII","XIII","XIV","XV"),"[.]{0,}"),collapse = "|")
                   regx.1<-paste0("^.+?",numer,".+(",headx.1,")\\.")
                   regx.1<-paste0("^+?",numer,".+(",headx.1,")\\.")
                   # global from metadf
                   # imp: ^[^a-zA-Z][ \t]{1,}?F.RSTE.+(AKT)\.?$
                   regx.1<-paste0("^[ \t]{0,}?(",numer.all,").+(",h1.all,")\\.?$")
                   regx.1<-paste0("^([ \t]{0,}[^#] ?)?(",numer.all,").+?(",h1.all,")\\.?(.+)?$")
                   regx.1<-paste0("^([ \t]{0,}#?)?((",numer.all,").+?(",h1.all,")\\.?(.+)?)$")
                   ######### 15377.header issue
                   regx.1<-paste0("((","^([ \t]{0,}#?)?((",numer.all,")?(.+?)(",h1.all,")\\.?(.+)?)$",")|(",headx.1,"))( [0-20])?")
                   regx.1<-paste0("((","^([ \t]{0,}#?)?((",numer.all,")?(.+?)(",h1.all,")\\.?(.+)?)$",")|(",headx.1,"))( [0-20])?")
                   ### TEST
                   #parts<-
                   ###
                   
                   regx.2<-paste0("((","^([ \t]{0,}#?)?((",numer.all,")?.+?(",h2.all,")\\.?(.+)?)$",")|(",headx.2,"))( [0-20])?")
                   return(list(h1=regx.1,h2=regx.2))
}

# s7<-"~/Documents/GitHub/ETCRA5_dd23/bgltr/dracorTEI/exc-all-H.txt"
# 
# source<-s7
# 
# regH<-get.regex(headx.1="(Akt|Act|Handlung|Aufzug)",headx.2="(Szene|Scene|Auftritt)")
# 
# regx.1<-regH$h1
# regx.2<-regH$h2
# ###############
# t1<-readLines(source) # @train: m1/m2: 7/5
# p1<-str_match(t1,regx.1)
# m1<-which(!is.na(p1[,1]))
# t1[m1]
# p2<-str_match(t1,regx.2)
# m2<-which(!is.na(p2[,1]))
# t1[m2]
# p12<-p1[!is.na(p1[,1]),]
# p12
# 
# ############
# library(stringr)
# 
# txt <- readLines("exc-all-H.txt")
# Requires stringr
headx.1="Act,Handlung,Akt,Aufzug"
headx.2="Scene,Szene,Auftritt"
get.heads.4<-function(t1,headx.1="Act,Handlung,Akt,Aufzug",headx.2="Scene,Szene,Auftritt"){
library(stringr)
txt<-t1

# txt <- readLines("exc-all-H.txt", encoding = "UTF-8")
# metadf$cardinal
metadf<-read.csv("lx/metadf-mlx-02.csv")
numer<-c(metadf$cardinal,1:20,c(paste0(1:20,"\\.")))
numer
h1<-unlist(strsplit(headx.1,","))
h2<-unlist(strsplit(headx.2,","))
ord_group<-paste0("(?:",paste0(unique(numer),collapse = "|"),")")
ord_group
h1
h2
act_tokens<-paste0("(?:",paste0(c(h1,unique(metadf$h1)),collapse  = "|"),")")
scene_tokens<-paste0("(?:",paste0(c(h2,unique(metadf$h2)),collapse = "|"),")")
act_tokens
scene_tokens
#act_tokens <- "(?:handlung|aufzug|akt|act)"
#scene_tokens <- "(?:szene|scene|auftritt)"
act_anchor    <- paste0("^\\s*(?:(?:\\d+\\.)|(?:[IVXLCDM]+)|", ord_group, ")?\\s*", act_tokens, "\\b[\\s\\.,:;\\-–—]*$")
scene_anchor  <- paste0("^\\s*(?:(?:\\d+\\.)|(?:[IVXLCDM]+)|", ord_group, ")?\\s*", scene_tokens, "\\b[\\s\\.,:;\\-–—]*$")
combined_scene <- paste0("^\\s*(?:(?:\\d+\\.)|(?:[IVXLCDM]+)|", ord_group, ")?\\s*", act_tokens, "\\b.*\\b", scene_tokens, "\\b")
combined_title <- paste0("^\\s*(?:(?:\\d+\\.)|(?:[IVXLCDM]+)|", ord_group, ")?\\s*", act_tokens, "\\b.*[A-Za-zÄÖÜÆØÅäöüßæøå].*$")

act_re   <- regex(act_anchor, ignore_case = TRUE)
#act_re[[1]]
scene_re <- regex(scene_anchor, ignore_case = TRUE)
combined_scene_re <- regex(combined_scene, ignore_case = TRUE)
combined_title_re <- regex(combined_title, ignore_case = TRUE)

scene_loc_re <- regex(paste0("\\b", scene_tokens, "\\b"), ignore_case = TRUE)
# m1<-grep(combined_scene_re,t1)
# m2<-grep(combined_title_re,t1)
# m3<-grep(act_re,t1)
# m4<-grep(scene_re,t1)
# m5<-c(m1,m2,m3,m4)
# m5<-unique(m5)
m1<-which(str_detect(t1,combined_scene_re))
m1
#act_re[[1]]
m2<-which(str_detect(t1,combined_title_re))
m3<-which(str_detect(t1,act_re))
m3
sum(m3)
m4<-which(str_detect(t1,scene_re))
m5<-c(m1,m2,m3,m4)
m5<-unique(m5)
# sum(m4)
# sum(m1)
# t1
# m2<-grep(combined_title_re,t1)
# m3<-grep(act_re,t1)
# m4<-grep(scene_re,t1)
# m5<-c(m1,m2,m3,m4)
# m5
# sum(m5)
#lines<-txt[m5]
#line<-t1[6]
tag_line <- function(line) {
  if (str_detect(line, combined_scene_re)) {
    m <- str_locate(line, scene_loc_re)
    act_part <- str_trim(str_sub(line, 1, m[1,1] - 1))
    scene_part <- str_trim(str_sub(line, m[1,1], str_length(line)))
    return(c(paste0("# ", act_part), paste0("## ", scene_part)))
  } else if (str_detect(line, combined_title_re)) {
    # split after first act token
    parts <- str_split_fixed(line, paste0("\\b(?:",act_tokens,")\\b"), 2)
    parts
    act_part <- paste0(str_trim(parts[1]))  # keep the act token
    title_part <- str_trim(parts[2])
    return(c(paste0("# ", act_part)))
  } else if (str_detect(line, act_re)) {
    return(paste0("# ", str_trim(line)))
  } else if (str_detect(line, scene_re)) {
    return(paste0("## ", str_trim(line)))
  } else {
    return(line)
  }
}
m5<-m5[order(m5)]
txt[m5]
#tagged <- unlist(lapply(txt[m5], tag_line))
tagged <- lapply(txt[m5], tag_line)
replace_with_list <- function(vec, pos, vals_list) {

  stopifnot(length(pos) == length(vals_list))
  
  offset <- 0
  for (i in seq_along(pos)) {
    p <- pos[i] + offset
    v <- vals_list[[i]]
    # Remove the element at position p, insert v at p
    vec <- append(vec[-p], v, after = p - 1)
    # Update offset for next insertion
    offset <- offset + length(v) - 1
  }
  vec
}
#txm<-txt
txm<-replace_with_list(txt,m5,tagged)
print(txm)
vario<-unlist(tagged)
h1.first<-grep("[#]{1}",txm)[1]
return(list(text=txm,vario=vario,h1.first=h1.first))
}

t2
t2<-get.heads.4(t1)
t2$vario
### restore
get.heads.4<-function(t1,headx.1="Act|Handlung|Akt|Aufzug",headx.2="Scene|Szene|Auftritt"){
  library(stringr)
  txt<-t1
  # txt <- readLines("exc-all-H.txt", encoding = "UTF-8")
  # metadf$cardinal
  metadf<-read.csv("lx/metadf-mlx-02.csv")
  numer<-c(metadf$cardinal,1:20,c(paste0(1:20,"\\.")))
  numer
  ord_group<-paste0("(?:",paste0(numer,collapse = "|"),")")
  act_tokens<-paste0("(?:",paste0(metadf$h1,collapse = "|"),")")
  scene_tokens<-paste0("(?:",paste0(metadf$h2,collapse = "|"),")")
  act_tokens
  
  act_anchor    <- paste0("^\\s*(?:(?:\\d+\\.)|(?:[IVXLCDM]+)|", ord_group, ")?\\s*", act_tokens, "\\b[\\s\\.,:;\\-–—]*$")
  scene_anchor  <- paste0("^\\s*(?:(?:\\d+\\.)|(?:[IVXLCDM]+)|", ord_group, ")?\\s*", scene_tokens, "\\b[\\s\\.,:;\\-–—]*$")
  combined_scene <- paste0("^\\s*(?:(?:\\d+\\.)|(?:[IVXLCDM]+)|", ord_group, ")?\\s*", act_tokens, "\\b.*\\b", scene_tokens, "\\b")
  combined_title <- paste0("^\\s*(?:(?:\\d+\\.)|(?:[IVXLCDM]+)|", ord_group, ")?\\s*", act_tokens, "\\b.*[A-Za-zÄÖÜÆØÅäöüßæøå].*$")
  
  act_re   <- regex(act_anchor, ignore_case = TRUE)
  scene_re <- regex(scene_anchor, ignore_case = TRUE)
  combined_scene_re <- regex(combined_scene, ignore_case = TRUE)
  combined_title_re <- regex(combined_title, ignore_case = TRUE)
  act_re[[1]]
  scene_loc_re <- regex(paste0("\\b", scene_tokens, "\\b"), ignore_case = TRUE)
  
  tag_line <- function(line) {
    if (str_detect(line, combined_scene_re)) {
      m <- str_locate(line, scene_loc_re)
      act_part <- str_trim(str_sub(line, 1, m[1,1] - 1))
      scene_part <- str_trim(str_sub(line, m[1,1], str_length(line)))
      return(c(paste0("# ", act_part), paste0("## ", scene_part)))
    } else if (str_detect(line, combined_title_re)) {
      # split after first act token
      parts <- str_split_fixed(line, paste0("\\b(?:",act_tokens,")\\b"), 2)
      act_part <- paste0(str_trim(parts[1]), " Akt")  # keep the act token
      title_part <- str_trim(parts[2])
      return(c(paste0("# ", act_part), paste0("## ", title_part)))
    } else if (str_detect(line, act_re)) {
      return(paste0("# ", str_trim(line)))
    } else if (str_detect(line, scene_re)) {
      return(paste0("## ", str_trim(line)))
    } else {
      return(line)
    }
  }
  
  tagged <- unlist(lapply(txt, tag_line))
  tagged
  tagged<-tagged[!grepl(" [,.:;]{0,}$",tagged)]
  print(tagged[grepl("#",tagged)])
}
#tagged
#writeLines(tagged, "exc-all-H-tagged.txt", useBytes = TRUE)

tag.dep<-function(){
### 1
act_tokens <- "(?:handlung|aufzug|akt|act)"
scene_tokens <- "(?:szene|scene|auftritt)"

# anchored patterns (no inline (?i) here; use regex(..., ignore_case=TRUE) instead)
act_anchor    <- paste0("^\\s*(?:(?:\\d+\\.)|(?:[IVXLCDM]+)|", ord_group, ")?\\s*", act_tokens, "\\b[\\s\\.,:;\\-–—]*$")
scene_anchor  <- paste0("^\\s*(?:(?:\\d+\\.)|(?:[IVXLCDM]+)|", ord_group, ")?\\s*", scene_tokens, "\\b[\\s\\.,:;\\-–—]*$")
combined_anchor <- paste0("^\\s*(?:(?:\\d+\\.)|(?:[IVXLCDM]+)|", ord_group, ")?\\s*", act_tokens, "\\b.*\\b", scene_tokens, "\\b")

act_re  <- regex(act_anchor,  ignore_case = TRUE)
scene_re <- regex(scene_anchor, ignore_case = TRUE)
combined_re <- regex(combined_anchor, ignore_case = TRUE)
scene_loc_re <- regex(paste0("\\b", scene_tokens, "\\b"), ignore_case = TRUE)
act_re
tag_line <- function(line) {
  if (str_detect(line, combined_re)) {
    # find first scene token position
    m <- str_locate(line, scene_loc_re)
    start <- m[1,1]
    act_part <- str_trim(str_sub(line, 1, start - 1))
    scene_part <- str_trim(str_sub(line, start, str_length(line)))
    return(c(paste0("# ", act_part), paste0("## ", scene_part)))
  } else if (str_detect(line, act_re)) {
    return(paste0("# ", str_trim(line)))
  } else if (str_detect(line, scene_re)) {
    return(paste0("## ", str_trim(line)))
  } else {
    return(line)
  }
}

tagged <- unlist(lapply(txt, tag_line))
tagged
writeLines(tagged, "exc-all-H-tagged.txt", useBytes = TRUE)

# Regex patterns
act_pattern    <- "^\\s*(?:Erste|Zweyte|Dritte|Vierte|Fünfte|Sechste|Siebente|[IVXLC]+|[0-9]+\\.?)?\\s*(?:Handlung|Aufzug|AKT|Act)\\b.*$"
scene_pattern  <- "^\\s*(?:Erste|Zweyte|Dritte|Vierte|Fünfte|[IVXLC]+|[0-9]+\\.?)?\\s*(?:Szene|Scene|Auftritt)\\b.*$"
combined_pattern <- "^\\s*.*?(?:Handlung|Aufzug|AKT|Act)\\b.*?(?:Szene|Scene|Auftritt)\\b.*$"
line<-txt[11]
line
tag_line <- function(line) {
  if (str_detect(line, combined_pattern)) {
    parts <- str_split(line, "\\b(?=Szene|Scene|Auftritt)", simplify = TRUE)
    parts <- trimws(parts)
    parts <- c(
      str_replace(parts[1], ".*", "# \\0"),
      str_replace(parts[2], ".*", "## \\0")
    )
    return(parts)
  } else if (str_detect(line, act_pattern)) {
    return(str_replace(line, ".*", "# \\0"))
  } else if (str_detect(line, scene_pattern)) {
    return(str_replace(line, ".*", "## \\0"))
  } else {
    return(line)
  }
}

tagged <- unlist(lapply(txt, tag_line))
tagged
#writeLines(tagged, "exc-all-H-tagged.txt")
}
get.h.dep<-function(){
get.heads.s<-function(t1,headx.1="(Akt|Act|Handlung)",headx.2="(Szene|Scene)"){
  numer<-metadf$cardinal
  #########################
  do.caps<-function(numer){
    numer.s<-unlist(strsplit(numer,"\\|"))
    numer.s<-gsub("[)(]","",numer.s)
    numer.s<-numer.s[numer.s!=""]
    numer.s<-numer.s[!is.na(numer.s)]
    numer.c<-capitalize(numer.s)
    numer.x<-gsub("[A-ZВ-Ш]",".",numer.c)
    numer.x
    # numer.dc<-decapitalize(numer.s)
    
    numer.tu<-toupper(numer.s)
    numer.tu.x<-toupper(numer.x)
    # n.all<-c(numer.s,numer.c,numer.dc,numer.tu,numer.x,numer.tu.x)
    n.all<-c(numer.s,numer.c,numer.tu,numer.x,numer.tu.x)
    n.all<-unique(n.all)
    numer.all<-paste0("\\b",n.all,"",collapse = "|")
    # numer.all<-paste0(n.all,collapse = "|")
    length(numer.all)
    return(numer.all)
  }
  numer.all<-do.caps(numer)
  #h1.all<-do.caps(h1)
  #h1<-paste0("(",paste0(metadf$h1,collapse = "|"),")")
  #h2<-paste0("(",paste0(metadf$h2,collapse = "|"),")")
  h1<-metadf$h1
  h2<-metadf$h2
  h1<-c(h1,headx.1)
  h2<-c(h2,headx.2)
  h1.all<-do.caps(h1)
  h2.all<-do.caps(h2)
  h1.all
  rm(h1)
  rm(h2)
  numer<-paste0(numer,paste0(1:20,"."),collapse = "|")
  numer<-paste0(numer,paste0(c("I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII","XIII","XIV","XV"),"[.]{0,}"),collapse = "|")
  regx.1<-paste0("^.+?",numer,".+(",headx.1,")\\.")
  regx.1<-paste0("^+?",numer,".+(",headx.1,")\\.")
  # global from metadf
  # imp: ^[^a-zA-Z][ \t]{1,}?F.RSTE.+(AKT)\.?$
  regx.1<-paste0("^[ \t]{0,}?(",numer.all,").+(",h1.all,")\\.?$")
  regx.1<-paste0("^([ \t]{0,}[^#] ?)?(",numer.all,").+?(",h1.all,")\\.?(.+)?$")
  regx.1<-paste0("^([ \t]{0,}#?)?((",numer.all,").+?(",h1.all,")\\.?(.+)?)$")
  ######### 15377.header issue
  regx.1<-paste0("((","^([ \t]{0,}#?)?((",numer.all,")?(.+?)(",h1.all,")\\.?(.+)?)$",")|(",headx.1,"))( [0-20])?")
  regx.1<-paste0("((","^([ \t]{0,}#?)?((",numer.all,")?(.+?)(",h1.all,")\\.?(.+)?)$",")|(",headx.1,"))( [0-20])?")
  ### TEST
  #parts<-
  ###
  
  regx.2<-paste0("((","^([ \t]{0,}#?)?((",numer.all,")?.+?(",h2.all,")\\.?(.+)?)$",")|(",headx.2,"))( [0-20])?")
  tx<-c("1. Akt","Act 1","  Act 1","none","First Act","First Scene","Second Scene","Scene 2","dummy 1")
  #m1<-grep(regx.b,tx,perl = T)
  #  regx.a<-paste0("^([ ]{0,})?(",numer.all,")?.+?(",h1.all,").+?([0-20])?$")
  #headx.1
  # regx.1<-paste0("^([ ]{0,}|[0-20])? ?(",numer.all,")? ?(",h1.all,") ?([0-20])?$|(",headx.1,")")
  # parts1<-str_match(t1,regx.1)
  # parts1
  #regx.2<-paste0("^([ ]{0,}|[0-20])? ?(",numer.all,")? ?(",h2.all,") ?([0-20])?$|(",headx.2,")")
  # parts2<-str_match(t1,regx.2)
  # parts2
  # 
  #tx
  # m1<-grep(regx.a,tx,perl = T)
  #  tx[m1]
  #print(regx.1)
  m1<-grep(regx.1,t1,perl = T)
  ############################
  # regx.2<-paste0("^.+?",numer,".+(",headx.2,")\\.")
  # regx.2<-paste0("^+?",numer,".+(",headx.2,")\\.")
  # regx.2<-paste0("^[ \t]{1,}?(",numer.all,").+(",h2.all,")\\.?$")
  # regx.2<-paste0("^([ \t]{1,})?(",numer.all,").+?(",h2.all,")\\.?(.+)?$")
  # regx.2<-paste0("((","^([ \t]{0,}#?)?((",numer.all,").+?(",h2.all,")\\.?(.+)?)$",")|(",headx.2,"))( [0-20])?")
  regx.2
  #print(regx.2)
  #m1<-grep("1\\. AKT",t1)
  t1
  m1<-grep(regx.1,t1,perl = T)
  h1.first<-m1[1]
  m2<-grep(regx.2,t1,perl = T)
  m1c<-grep("#",t1[m1])
  #parts[223,]
  ifelse(length(m1)==0,return(get.heads.2(t1,h1.all,h2.all,numer.all)),print("heads found with M1"))
  print("should print after heads found with M1")
  t1[1:50]
  t2<-t1
  # t2[m1]<-paste0("# ",t2[m1])
  ### 15372.NOTE: the shakespeare-ingentingen contains AKT definitions at each scene, so the <div> element is always created anew. this maybe okay for the library and of editorial perspective, but for the dracor scheme its not a way since it messes up the network and speech distribution.
  
  #########################################################
  ### chk if already marked up
  # m12<-grepl("^#",t2[m1])
  # m1<-m1[!m12]
  parts<-str_match(t2,regx.1)
  parts
  mna<-is.na(parts[,1])
  print(parts[!mna,])
  t2[which(!mna)]
  mw<-which(!mna)
  rm(mna)
  h1.1<-parts[mw,1]
  h1.1
  h1c<-grepl("#",parts[mw,1])
  #mw<-mw[!h1c]
  
  
  #h1[is.na(h1)]<-""
  #h1<-h1[!is.na(h1),]
  cat("h1: ",h1.1,"\n")
  # cat("h1 after removing #-tagged:\n")
  print(t2[mw])
  #h1<-lapply(h1,function(x){paste0(x,collapse = " ")})
  #h1c<-
  #  h2<-t2[m2]  
  #mna
  ### 1538.act-in-text issue.
  rm.na<-function(parts,mw){
    print("---- parts ----")
    print(parts[mw,])
    # cl.1<-c(6)
    #  cl.2<-c(10)
    # cl.3<-11
    #cl.out<-c(10,11)
    cl1<-is.na(parts[,1]) # 6=NA&10:12=NA 
    cl10<-is.na(parts[,10])
    cl3<-100
    ### > this messes up the act-scene-1-line segmentation! parts[6] is not NA!
    #print(" sum !is.na(parts[,11])")
    print("---------- WTF ----------------")
    #print(sum(!is.na(parts[,11])))
    if(length(parts[1,])>10)
      cl11<-which(!is.na(parts[,11]))
    cl8<-is.na(parts[,8])
    cl9<-is.na(parts[,9])
    cl6<-is.na(parts[,6])
    #  cl4<-sum(cl4)>1
    # cl4<-sum(cl4)>1
    print("rm.na")
    print(mw)
    print(which(!cl1))
    print(which(!cl6))
    print(cl3)
    #print(which(!cl4))
    #print(which(!cl5))
    ###################################
    mw4<-mw[mw%in%which(!cl1)] # ground
    mw5<-mw4[mw4%in%which(cl8)&mw4%in%(which(!cl10))] 
    mw5<-mw4[mw4%in%which(cl8)&mw4%in%(which(cl10))]
    mw5<-mw5[mw5%in%which(!cl6)]
    
    # THIS wks with: ^Erste Handlung | forste akt scene 1 
    print(mw4)
    # for IBSEN:
    if(length(mw5)<2)
      mw5<-mw4[mw4%in%which(!cl6)]
    #wks, assuming a minimum count of 2 regular acts found
    #critical, thats why not 1: if a normal line begins as ^Akt as first string
    ###################################
    #print(mw5)
    # print(which(!cl2))
    # mw4<-mw4[mw4%in%which(cl2)]
    # print(mw4)
    # mw<-mw4[!mw4%in%which(cl3)]
    mw<-mw5
    # 15384.still header issue wt iwanette
    print(mw)
    print(t2[mw])
    h1<-parts[mw,1]
    return(list(mw=mw,h=h1))
  }
  ### VIP! check if now is empty!
  mw.sel<-rm.na(parts,mw)
  mw.na<-mw.sel$mw # stayed in headers after cleanup
  h1<-mw.sel$h
  ifelse(length(mw.na)<1,mw<-mw,mw<-mw.na)
  ifelse(length(mw.na)<1,vario<-h1.1,vario<-mw.sel$h)
  #mw<-mw[mw%in%m.out]
  ############################
  t2[mw]<-paste0("# ",parts[mw,1]) #15375.marked up header issue
  t2[mw]<-gsub("#[ ]{0,}#","# ",t2[mw])
  # t1<-t2
  #vario<-h1
  print("vario-h1")
  print(vario)
  print(t2[mw])
  #rm(mw)
  # t2[m1]<-paste0("# ",t2[m1],"%hnl%") #out
  #  ^([ \t]{0,})?(",numer.all,").+?(",h1.all,")\\.?(.+)?$
  # t2[m1]<-gsub(regx.1,"# \\2 \\3%hnl%\\4",t2[m1],perl = T) #in
  # m2<-grep(regx.2,t1)
  t2[m2]
  print("chk for remaining trash in t1")
  # m3<-t1[m1]%in%parts[mw,1]
  # t2[m1]<-t2[m1][!m3]
  rm(mw)
  #print(t2[m1][m3])
  ############################
  ### header 2
  parts2<-str_match(t2,regx.2)
  mna<-is.na(parts2[,1])
  parts2[!mna,1]
  #parts[!mna,3:length(parts[1,])]
  ### 15382.still multiple header issue #TODO
  mw<-which(!mna)
  h2c<-grepl("#",parts2[mw,1])
  mw<-mw[!h2c]
  print("h2 mw in t2 after removing # headers")
  # 15385.here #TODO: iwanette tags {scene} in text! 
  print(mw)
  print(t2[mw])
  if(length(mw)>0){
    mw.sel<-rm.na(parts2,mw)
    mw.na<-mw.sel$mw
    h2<-mw.sel$h
    print("h2----")
    print(h2)
    vario<-c(h1,h2)
    # t2[mw]<-paste0("## ",parts2[mw,1]) #15375.marked up header issue
    print("tagged h2")
    print(t2[mw.na])
  }
  print("chk h2 for remaining trash in t1")
  # m3<-t1[m2]%in%t2[mw]
  # print(t1[m2][m3])
  # ifelse(sum(!m3)==length(mw),print("no h2 replacements"),t2[mw]<-paste0("## ",parts2[mw,1]))
  print("removed trash")
  print(t2[mw])
  # this critical > mw always 0 if found # in vario!
  #if(length(mw.na)==0){
  print(length(mw))
  if(length(mw)==0){
    print("M3-----")
    #print(h1)
    # print(h2)
    print(vario)
    ####################################################
    #  t.sep<-get.heads.3(t2,vario,h1.all,h2.all,numer.all)  
    t.sep<-get.heads.3(t1,vario,h1.all,h2.all,numer.all) #15384.this wks wt iwanette!
    ####################################################
    vario<-t.sep$vario
    t2<-t.sep$text
    h1.first<-t.sep$h1.first
  }
  
  #  h2<-parts2[mw,1]
  #cat("h2:",h2,"\n")
  print("noprint?")
  
  #m3<-m2%in%m1
  
  #h1[is.na(h1)]<-""
  #h1<-h1[!is.na(h1),]
  #h1
  #t2[which(!mna)]
  #  h2<-parts[!mna,1]
  # h2<-parts[!mna,3:length(parts[1,])]
  # h2[is.na(h2)]<-""
  # h2<-h2[!is.na(h2)]
  # h2<-unlist(lapply(h2,function(x){paste(x,collapse = " ")}))
  # #h2<-parts[!mna,length(parts[1,])]
  
  # wma<-which(!mna)
  #t2[wma]
  #wma
  #m22<-grepl("^#",t2[wma])
  #wma<-wma[!m22]
  #t2<-t1
  #  t2[m2]<-paste0("## ",t2[m2])
  #m3<-m2%in%m1
  #if (m2!=m1)
  # t2[mw]<-paste0("##",parts2[mw,1]) #15375.marked up header issue
  #    t2[m2][!m3]<-paste0("## ",t2[m2][!m3],"%hnl%") #out
  # t2[m2]<-gsub(regx.2,"## \\2 \\3%hnl%\\4",t2[m2],perl = T) #in
  #########################################################
  #vario<-c(h1,h2)
  cat("\rvario:",vario)
  # h1<-t2[m1]
  # h2<-t2[m2]  
  #  m3<-duplicated(t2[m1])
  # print("duplicated act") ### > is never run
  #print(m3)
  ### 15373.double act issue, no scene divisions
  #t3<-get.heads.2(t2,h1.all,h2.all,numer.all)
  #t2<-t3$text
  # t2[m1][m3]<-"" # duplicated ACT definitions are deleted
  #return(t1)
  # vario<-c(t2[m1],t2[m2])
  # h1<-t2[m1]
  # h2<-t2[m2]
  #       print(h1[1:10])
  #      print(h2[1:10])
  
  return(list(vario=vario,text=t2,h1.first=h1.first))
}
}