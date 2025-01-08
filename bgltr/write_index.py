import pywikibot as pwb
import os

#pwb generate_user_files
#from pywikibot.specialbots import UploadRobot

def upload_site_text(text_path):

# Specify the directory path
    directory_path = '.'

# List all files in the directory
    files = os.listdir(directory_path)

# Print the list of files
    for file in files:
      print(file)
  
    site = pwb.Site('de', 'wikisource')
   # site.login()
   # pwb.login
    #pwb.login
    print("logged in")

    # Read the description from the file
    with open(text_path, 'r') as file:
        textNeu = file.read()

    # Create the upload robot
    site = pwb.Site('de', 'wikisource')  # The site we want to run our bot on
    page = pwb.Page(site, 'Seite:Bodmer_polytimet_1760.pdf/6')
#    page = pwb.Page(site, 'Index:Steltzer_montenegro.pdf')
    text = page.text
#    textup = ""
    print("alte:")
    print(text)
    print("neue:")
    print(textNeu)
#    page.text = page.text.replace('*',textNeu)
    #page.text = 'blank'
   # page.text = textNeu
    #page.save('init insert OCR text')
    page = pwb.Page(site, 'Seite:Steltzer_montenegro.pdf/5')
    text = page.text    
    print("changed:")
    print(text)
    

    # Upload the file
    #bot.upload_file(pdf_path)

if __name__ == "__main__":
#    pdf_path = "/Users/guhl/boxHKW/21S/DH/local/EXC2020/bgltr/ocr/steltzer_montenegro.pdf"
    text_path = "/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/ocr/actuel/pages/steltzer-p5.txt"
#    filename = "File:/pdf/steltzer_montenegro.pdf"
 #   summary = "test bot pdf upload"

    upload_site_text(text_path)
