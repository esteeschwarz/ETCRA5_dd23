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
word<-  
words<-paste0(sample(df1$c)
              
}