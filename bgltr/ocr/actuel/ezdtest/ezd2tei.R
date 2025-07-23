# 20250723(16.49)
# 15305.ezd2tei essai, deeps
############################
# Charger les packages nécessaires
library(xml2)
library(stringr)
library(purrr)

# Fonction d'échappement XML
escape_xml_dep <- function(text) {
  text %>% 
    str_replace_all("&", "&amp;") %>%
    str_replace_all("<", "&lt;") %>% 
    str_replace_all(">", "&gt;") %>% 
    str_replace_all("\"", "&quot;") %>% 
    str_replace_all("'", "&apos;")
}

# Initialiser un nouveau document XML
create_tei_document <- function() {
  doc <- xml_new_document()
  tei <- xml_add_child(doc, "TEI", 
                       `xml:id` = "insertID",
                       `xml:lang` = "ger",
                       xmlns = "http://www.tei-c.org/ns/1.0")
  
  # Ajouter l'en-tête TEI
  teiHeader <- xml_add_child(tei, "teiHeader")
  fileDesc <- xml_add_child(teiHeader, "fileDesc")
  
  # Titre et auteur
  titleStmt <- xml_add_child(fileDesc, "titleStmt")
  xml_add_child(titleStmt, "title", "Der Tod Abels", type = "main")
  xml_add_child(titleStmt, "title", "Ein Trauerspiel", type = "sub")
  xml_add_child(titleStmt, "author", "Margarete Klopstock")
  
  # Section de publication
  publicationStmt <- xml_add_child(fileDesc, "publicationStmt")
  xml_add_child(publicationStmt, "publisher", "DraCor", `xml:id` = "dracor")
  xml_add_child(publicationStmt, "idno", "https://dracor.org", type = "URL")
  
  # Disponibilité
  availability <- xml_add_child(publicationStmt, "availability")
  licence <- xml_add_child(availability, "licence")
  xml_add_child(licence, "ab", "CC0 1.0")
  xml_add_child(licence, "ref", "Licence", 
                target = "https://creativecommons.org/publicdomain/zero/1.0/")
  
  # Source
  sourceDesc <- xml_add_child(fileDesc, "sourceDesc")
  bibl <- xml_add_child(sourceDesc, "bibl", type = "digitalSource")
  xml_add_child(bibl, "name", "ENTER SOURCE NAME HERE")
  xml_add_child(bibl, "idno", "ENTER SOURCE URL HERE", type = "URL")
  
  # Liste des personnages
  profileDesc <- xml_add_child(teiHeader, "profileDesc")
  particDesc <- xml_add_child(profileDesc, "particDesc")
  listPerson <- xml_add_child(particDesc, "listPerson")
  
  # Corps du texte
  text <- xml_add_child(tei, "text")
  front <- xml_add_child(text, "front")
  castList <- xml_add_child(front, "castList")
  xml_add_child(castList, "head", "Personen.")
  
  body <- xml_add_child(text, "body")
  
  list(
    doc = doc,
    body = body,
    castList = castList,
    listPerson = listPerson
  )
}

input_file<-"klopstock_tod-abels_ezd.txt"
parse_drama_text <- function(input_file, output_file) {
  # Lire le fichier d'entrée
  lines <- readLines(input_file, encoding = "UTF-8")
  
  # Initialiser le document XML
  xml_doc <- create_tei_document()
  
  # Variables d'état pour suivre la structure
  current_act <- NULL
  current_scene <- NULL
  line<-lines[length(lines)-2]
  # Traiter chaque ligne
  #line<-l1
  k<-11
  line
  for (k in 1:length(lines)) {
    # 1. Gestion des personnages (@)
   # ?str_detect
    line<-lines[k]
    if (str_detect(line, "^@[^.]+\\.", )) {
      parts <- str_match(line, "^@([^.]+?)\\.(.*)")
      speaker <- str_trim(parts[2])
      text <- str_trim(parts[3])
      
      # Traitement des numéros de page (150::)
      text <- str_replace_all(text, "(\\d{1,4})::", "</p><pb n=\"\\1\"/><p>")
      
      # Traitement des didascalies inline ((texte))
      text <- str_replace_all(text, "\\(([^)]+)\\)", "<stage>\\1</stage>")
      
      # Ajouter au XML
      if (!is.null(current_scene)) {
        sp <- xml_add_child(current_scene, "sp", who = paste0("#", tolower(speaker)))
        xml_add_child(sp, "speaker", speaker)
        p <- xml_add_child(sp, "p")
        xml_text(p) <- text
      }
    } 
    # 2. Didascalies de bloc ($)
     if (str_detect(line, "^\\$")) {
      stage_content <- str_trim(str_sub(line, 2))
      if (!is.null(current_scene)) {
        xml_add_child(current_scene, "stage", stage_content)
      }
    }
    # 3. Actes (#)
    if (str_detect(line, "^#")) {
      act_title <- str_trim(str_sub(line, 2))
      current_act <- xml_add_child(xml_doc$body, "div", type = "act")
      xml_add_child(current_act, "head", act_title)
      current_scene <- NULL
    }
    # 4. Scènes (##)
    if (str_detect(line, "^##")) {
      scene_title <- str_trim(str_sub(line, 3))
      if (!is.null(current_act)) {
        current_scene <- xml_add_child(current_act, "div", type = "scene")
        xml_add_child(current_scene, "head", scene_title)
      }
    }
    # 5. Texte continu
    if (str_trim(line) != "") {
      if (!is.null(current_scene)) {
        # Appliquer les mêmes transformations que pour le texte des personnages
        processed <- line %>%
          str_replace_all("(\\d{1,4})::", "</p><pb n=\"\\1\"/><p>") %>%
          str_replace_all("\\(([^)]+)\\)", "<stage>\\1</stage>")
        
        p <- xml_add_child(current_scene, "p")
        xml_text(p) <- processed
      }
    }
  }
  xml_text(xml_doc$body)
  # Écrire le fichier XML de sortie
  write_xml(xml_doc$doc, output_file)
}
output_file<-"r-output.xml"

# Exemple d'utilisation
parse_drama_text("klopstock_tod-abels_ezd.txt", "r-output.xml")