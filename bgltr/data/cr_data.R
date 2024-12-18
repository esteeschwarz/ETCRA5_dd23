header<-c("id","play","author","wiki_page","auth_entry","pages","description","subtitle","year","location","publisher","src_orig_loc","src_wiki_loc","progress","status"
)
eins<-c("1","Franziska Montenegro","Christian Julius Ludwig Steltzer","index","Anonym ({{=}} [[ADB:Steltzer,_Christian_Julius_Ludwig|Christian Julius Ludwig Steltzer]])","95","Franziska Montenegro: Ein Trauerspiel in fünf Aufzügen","Ein Trauerspiel in fünf Aufzügen","1781","","","[https://mdz-nbn-resolving.de/urn:nbn:de:bvb:12-bsb10118086-3 MDZ]","{{co|Steltzer_montenegro.pdf}}","","unkorrigiert")
length(header)
length(eins)
df<-data.frame(matrix(eins,nrow = 1))
colnames(df)<-header
write.csv(df,"data/pages-data.csv",row.names = F)
