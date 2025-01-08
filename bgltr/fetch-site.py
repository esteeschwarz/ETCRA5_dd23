import pywikibot as pwb

site = pwb.Site('de', 'wikisource')  # The site we want to run our bot on
page = pwb.Page(site, 'Polytimet')
#page = pwb.Page(site, 'Index:Kotzebue_-_Blind_geladen.pdf')
#page = pwb.Page(site, 'Blind_geladen')
page = pwb.Page(site,"Seite:Steltzer_montenegro.pdf/5")
text = page.text
print(text)
# Specify the file path
file_path = '/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/data/steltzer_temp.txt'

# Open the file in write mode
f = open(file_path, 'w')
f.write(text)
# The file is automatically closed when the with block is exited
# f = open("demofile3.txt", "w")
# f.write("Woops! I have duo deleted the content!")
f.close()

#open and read the file after the overwriting:
# f = open("demofile3.txt", "r")
# print(f.read())
