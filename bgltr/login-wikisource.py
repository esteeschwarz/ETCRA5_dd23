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

site = pwb.Site('de', 'wikisource')  # The site we want to run our bot on
#site._userinfo
site.login()
site.username()
print(site.username())
