from pdf2image import convert_from_path
import os

# Path to the PDF file
pdf_path = 'steltzer,monenegro.pdf'

# Output directory for images
output_dir = 'png'
os.makedirs(output_dir, exist_ok=True)

# Convert PDF to images
images = convert_from_path(pdf_path)

# Save images
for i, image in enumerate(images):
    image_path = os.path.join(output_dir, f'page_{i + 1}.png')
    image.save(image_path, 'PNG')
    print(f'Saved {image_path}')
