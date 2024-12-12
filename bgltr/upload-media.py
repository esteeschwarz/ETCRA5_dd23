import pywikibot
from pywikibot.specialbots import UploadRobot

#site = pywikibot.Site('de', 'wikipedia')  # Change to your target site
site = pwb.Site('de', 'wikipedia')  # The site we want to run our bot on
page = pwb.Page(site, 'Users:guhlglaser/pigindex')
text = page.text
print(text)

file_path = '/Users/guhl/Documents/GitHub/school/api/rating-png/NPRG-index-cola.png'
description = 'test bot upload'
UploadRobot.upload_file(file_url="https://ada-sub.dh-index.org/school/api/png/index.png")
bot = UploadRobot([file_path], description=description, site=page)
bot.run()
