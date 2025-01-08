#python -m pip install "requests>=2.20.1"
#python -m pip install "mwparserfromhell>=0.5.2"
#python -m pip install packaging

###
import pywikibot as pwb
#site = pywikibot.Site()
#print(site.username())
#site.data_repository()
#pywikibot.Site()
#pwb.login()
#pwb.config()
#site = pywikibot.Site()
#print(site)
#page = pywikibot.Page(site, "Wikipedia:test:Helmut_Bröker")
#text = page.text
#print(text)
###
#site = pywikibot.Site('de', 'wikipedia')  # The site we want to run our bot on
#page = pywikibot.Page(site, 'Wikipedia:Sandbox')
#page.text = page.text.replace('bots', ' more bots')
#text = page.text
#print(text)
#page.save('Replacing "foo" with "bar"')  # Saves the page

site = pwb.Site('de', 'wikipedia')  # The site we want to run our bot on
page = pwb.Page(site, 'Helmut_Bröker')
text = page.text
print(text)

page.text = page.text.replace('St. Gallen', 'St.&nbsp;Gallen')
text = page.text
print(text)
page.save('add nonbreaking space in St. Gallen')  # Saves the page


site = pwb.Site('de', 'wikisource')  # The site we want to run our bot on
page = pwb.Page(site, 'Polytimet')
page = pwb.Page(site, 'Index:Bodmer_polytimet_1760.pdf')
text = page.text
print(text)
### steltzer
# https://de.wikisource.org/w/index.php?title=Seite:Steltzer_montenegro.pdf/5&action=edit&redlink=1
site = pwb.Site('de', 'wikisource')  # The site we want to run our bot on
page = pwb.Page(site, 'Polytimet')
page = pwb.Page(site, 'Seite:Steltzer_montenegro.pdf/5')
text = page.text
page.text = page.text.replace('St. Gallen', 'St.&nbsp;Gallen')
text = page.text
print(text)
page.save('add nonbreaking space in St. Gallen')  # Saves the page
print(text)

pwb.__loader__
### upload pig
pwb.py data_ingestion -csvdir:"/Documents/GitHub/ETCRA5_dd23/bgltr/data" -page:"User:guhlglaser/pig_template/"

-file:z
python pwb.py imagetransfer "User:guhlglaser/pig_template" -file Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/data/upload.md

###
python /mnt/nfs/labstore-secondary-tools-home/wmr-bot/core/pwb.py upload "pdf/000787-周易兼義九卷十三經註疏所收-卷首.pdf" -filename:"IOC.UTokyo-000787 周易兼義九卷十三經註疏所收 卷首.pdf" -ignorewarn -chunked -noverify -descfile:des/0 -always

python3 pwb.py upload /home/username/Logo/Ready "logo images" -keep -noverify -ignorewarn -summary:"Bot: Uploading logo images"

###
python3 pwb.py upload [Global-arguments] [-keep] [-filename:targetFilename] [-ignorewarn] [-noverify] [-chunked:64m] [-summary:SummaryOfTheUpload] [URL-or-filename] [-descfile:description-file]

/Users/guhl/boxHKW/21S/DH/local/EXC2020/bgltr/ocr/steltzer_montenegro.pdf
/Users/guhl/Documents/GitHub/school/api/rating-png/NPRG-index-cola.png


python pwb.py upload -filename:User:File:guhlglaser/pdf/steltzer.pdf -summary:"test pdf upload" /Users/guhl/boxHKW/21S/DH/local/EXC2020/bgltr/ocr/steltzer_montenegro.pdf
-descfile:/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/data/steltzer-desc.txt


("User:File:guhlglaser/pdf/steltzer.pdf" "test pdf upload" "/Users/guhl/boxHKW/21S/DH/local/EXC2020/bgltr/ocr/steltzer_montenegro.pdf" "/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/data/steltzer-desc.txt")

python pwb.py upload -filename: User:File:guhlglaser/pdf/steltzer_montenegro -summary: "test pdf upload" /Users/guhl/boxHKW/21S/DH/local/EXC2020/bgltr/ocr/steltzer_montenegro.pdf
-descfile: /Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/data/steltzer_desc.txt
