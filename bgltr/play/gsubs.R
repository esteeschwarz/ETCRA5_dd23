# Define the text
text <- "<sp><speaker>Katelli</speaker><stage>allein, <stage>dem Anton nachrufend</stage>
<sp><speaker>Franziska.</speaker><p> <stage>steht auf</stage> Komm, wir wollen zu unserm Vater laufen. <stage>gehen Hand in Hand ab.</stage> </p></sp>"

# # Remove unmatched <stage> elements
# #result <- gsub("(<stage>)(?![^<]*</stage>)", "\\1", text, perl = TRUE)
# # Define the text
# text <- "<sp><speaker>Katelli</speaker><stage>allein, <stage>dem Anton nachrufend</stage>
# <sp><speaker>Franziska.</speaker><p> <stage>steht auf</stage> Komm, wir wollen zu unserm Vater laufen. <stage>gehen Hand in Hand ab.</stage> </p></sp>"

# Remove unmatched <stage> elements
#result <- gsub("<stage>(?![^<]*</stage>)", "", text, perl = TRUE)
result <- gsub("(<stage>[^<]*)(<stage>)([^<]*</stage>)", "\\1\\3", text, perl = TRUE)

# Print the result
cat(result)
# Print the result
cat(result)


h <- read_html("<body><p id = 'a'>paragraph</p><p class = 'c d'></p></body>")
html_structure(h)
