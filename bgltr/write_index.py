import pywikibot as pwb
#from pywikibot.specialbots import UploadRobot

def upload_site_text(text_path):
    site = pwb.Site('de', 'wikisource')
    site.login()
    print("logged in")

    # Read the description from the file
    with open(text_path, 'r') as file:
        textNeu = file.read()

    # Create the upload robot
    site = pwb.Site('de', 'wikisource')  # The site we want to run our bot on
    #page = pwb.Page(site, 'Polytimet')
    page = pwb.Page(site, 'Index:Steltzer_montenegro.pdf')
    text = page.text
#    textup = ""
    print("alte:")
    print(text)
    print("neue:")
    print(textNeu)
#    page.text = page.text.replace('*',textNeu)
    #page.text = 'blank'
    page.text = textNeu
    page.save('steltzer page')
    page = pwb.Page(site, 'Index:Steltzer_montenegro.pdf')
    text = page.text    
    print("changed:")
    print(text)
    

    # Upload the file
    #bot.upload_file(pdf_path)

if __name__ == "__main__":
#    pdf_path = "/Users/guhl/boxHKW/21S/DH/local/EXC2020/bgltr/ocr/steltzer_montenegro.pdf"
    text_path = "/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/data/steltzer_index.txt"
#    filename = "File:/pdf/steltzer_montenegro.pdf"
 #   summary = "test bot pdf upload"

    upload_site_text(text_path)
