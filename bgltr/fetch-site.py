import pywikibot as pwb

site = pwb.Site('de', 'wikisource')  # The site we want to run our bot on
page = pwb.Page(site, 'Polytimet')
#page = pwb.Page(site, 'Index:Kotzebue_-_Blind_geladen.pdf')
#page = pwb.Page(site, 'Blind_geladen')
text = page.text
print(text)
