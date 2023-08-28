# regular expressions overview
#### tools for learning and applying regex functions
- [https://regexr.com][1]
- [https://regex101.com][2]
- [https://ahkde.github.io/docs/misc/RegEx-QuickRef.htm#Common][3]
- [regex compendium][4]
#### general:
regex functions allow for complex string ("Zeichenketten") searches within a text or textbundle.  
simple: e.g. if you want to find all occurences of a name (speaker) in a play, you could search for (if the speaker is named "Paul"):  
`(Paul)` In R the command would be:  
`m<-grep("Paul",textarray)` where m will be the resulting array of occurences (indices) of "Paul" within a defined array of strings. if you have a text *sample.txt* of plain text consisting of several paragraphs (where the text is divided with CARRIAGE RETURN or by means of a header) the routine looks like this:  

```r
textarray<-readLines("sample.txt")
m<-grep("Paul",textarray)
print(textarray[m])
```
this will output only the textlines containing a Paul-instance.   
you could if you want that make a Paula of all Pauls by:   
```r
textarray.modified<-gsub("Paul","Paula",textarray)
print(textarray.modified)
```

the regex methods allow very fine grained search&replace commands, see the learning tools above where you can experiment with search formula in a app or browser.

[1]:	https://regexr.com
[2]:	https://regex101.com
[3]:	https://ahkde.github.io/docs/misc/RegEx-QuickRef.htm#Common
[4]:	https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html