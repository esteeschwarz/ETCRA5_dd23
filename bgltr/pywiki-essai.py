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
