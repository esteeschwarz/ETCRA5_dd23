sm<-paste0(letters)
nm<-1:9
sc<-paste0(sm,nm)
sm.rnd<-sample(sm,length(sm))
nm.rnd<-sample(nm,length(nm))
sc.rnd<-paste0(sm,nm.rnd)
sm.rnd
df1<-data.frame(l=sm,n=rep_len(nm.rnd,length.out = length(sm)),c=sc.rnd)
               
rep(nm.rnd)
?rep

library(lme4)
library(stats)
l1<-lm(df1$n~df1$l)
summary(l1)

for(k in 1:length(df1$c)){
syl<-df1$c[k]
n.syl<-sample(4:10,1)
#word  
words<-paste0(sample(df1$c))
              
}


### copilot

library(stringdist)
src<-"~/Documents/GitHub/ETCRA5_dd23/dybbuk/yudale_tok_freq/tok.freq.list.edited-yudale_tok_freq.csv"
dictionary <- read.csv(src)
dictionary<- dictionary[55:length(dictionary$id),]
rownames(dictionary)<-1:length(dictionary$id)
dict<-dictionary$cor.tok
m<-""==dict
dict<-dict[!m]
token<-token_to_check
custom_spelling_checker <- function(token) {
  min_distance <- Inf
  corrected_word <- NULL
  for (word in dict) {
    distance <- stringdist::stringdist(token, word, method = "lv")
    if (distance < min_distance) {
      min_distance <- distance
      corrected_word <- word
    }
  }
  return(corrected_word)
}
#wks. bit
  dictionary$cor.ai<-NA
  for(k in 1:length(dictionary$WORD)){
    token_to_check<-dictionary$WORD[k]
    corrected_token <- custom_spelling_checker(token_to_check)
  dictionary$cor.ai[k]<-corrected_token  
  cat(k,token_to_check,corrected_token,"\n")
  }
  
  # token_to_check <- "aple"
  token_to_check <- dictionary$WORD[12]
  # corrected_token <- custom_spelling_checker(token_to_check)
# cat("Original:", token_to_check, "Corrected:", corrected_token, "\n")



