import pywikibot
from pywikibot.specialbots import UploadRobot

def upload_pdf_to_wikisource(pdf_path, description_path, filename, summary):
    site = pywikibot.Site('de', 'wikisource')
   # site.login()
    print("logged in")

    # Read the description from the file
    with open(description_path, 'r') as file:
        description = file.read()

    # Create the upload robot
    bot = UploadRobot(
        url=None,
        description=description,
        target_site=site,
        keep_filename=True,
        verify_description=False,
        ignore_warning=True,
        filename=filename,
        summary=summary,
        chunk_size=1000000,
        asynchronous=False
    )

    # Upload the file
    bot.upload_file(pdf_path)

if __name__ == "__main__":
    pdf_path = "/Users/guhl/boxHKW/21S/DH/local/EXC2020/bgltr/ocr/steltzer_montenegro.pdf"
    description_path = "/Users/guhl/Documents/GitHub/ETCRA5_dd23/bgltr/data/steltzer_desc.txt"
    filename = "File:/pdf/steltzer_montenegro.pdf"
    summary = "test bot pdf upload"

    upload_pdf_to_wikisource(pdf_path, description_path, filename, summary)
