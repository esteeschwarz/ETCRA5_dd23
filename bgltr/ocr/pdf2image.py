from pdf2image import convert_from_path
import os

# Path to the PDF file
pdf_path = 'steltzer_montenegro.pdf'

# Output directory for images
output_dir = 'png'
os.makedirs(output_dir, exist_ok=True)
# Specify the directory
directory = '~/boxHKW/21S/DH/local/EXC2020/TR/ocr/'
directory="./png"
# List all files in the directory
files = [f for f in os.listdir(directory) if os.path.isfile(os.path.join(directory, f))]

# Print the list of files
print(files)
# Convert PDF to images
images = convert_from_path(pdf_path)

# Save images
for i, image in enumerate(images):
    image_path = os.path.join(output_dir, f'page_{i + 1}.png')
    image.save(image_path, 'PNG')
    print(f'Saved {image_path}')

