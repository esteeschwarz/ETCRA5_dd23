#13251.R-translation essai of: jupyter notebook https://github.com/ezdrama
#20230617(05.50)
################




# 
# {
#  "cells": [
#   {
#    "cell_type": "markdown",
#    "id": "264e9ce6",
#    "metadata": {},
#    "source": [
#     "## imports"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "90b424b2",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "import sys\n",
#     "import re\n",
#     "#from transliterate import translit\n",
#     "from bs4 import BeautifulSoup, Tag\n",
#     "from datetime import datetime"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "83f9997f",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "#from translitua import translit, UkrainianISO9"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "75a61808",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "import os"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "5c6fbf56",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "!pip install wget\n",
#     "import wget"
#    ]
#   },
#   {
#    "cell_type": "markdown",
#    "id": "059a4b48",
#    "metadata": {},
#    "source": [
#     "## functions"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "8adcdd27",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def parse_file(fpath):\n",

src<-"data/sample.txt"

post_process<-function(soup){
  soup<-gsub("#","dummy",soup)
  soup<-gsub("\\(","<stage>",soup)
  soup<-gsub("\\)","</stage>",soup)
}
# add_spaces_inline_stages<-function(tree_to_write){
#   tree_to_write<-paste("inserts",tree_to_write)
# }

parse_file<-function(fpath){
  path_to_file<-fpath
  file_lines<-readLines(path_to_file)
  soup<-file_lines
  augmented_soup<-post_process(soup)
 # tree_to_write<-gsub("\n","",augmented_soup)
  tree_to_write<-add_spaces_inline_stages(tree_to_write)
  outfile_ns<-gsub(paste0("\\.(txt|md)"),paste0(".xml"),fpath)
  writeLines(tree_to_write,outfile_ns)
}
fpath<-src
parse_file(src)
readLines(src)
#     "    path_to_file = fpath\n",
#     "    with open(path_to_file) as openfile:\n",
#     "        file_lines = openfile.readlines()\n",
#     "    soup = parse_lines(file_lines) \n",
#     "    augmented_soup = post_process(soup)\n",
#     "    tree_to_write = str(soup).replace('\\n', '')\n",
#     "    tree_to_write = add_spaces_inline_stages(tree_to_write) #\n",
#     "    with open(re.sub(r'\\.(txt|md)', r'.xml',fpath), 'w') as outfile:\n",
#     "        outfile.write(tree_to_write)"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "abbfa0ca",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def add_spaces_inline_stages(tree_as_string):\n",
add_spaces_inline_stages<-function(tree_as_string){
  tree_as_string<-gsub("</stage>([^\\s])","</stage> \\1",tree_as_string)
  tree_as_string<-gsub('([^\\s])<stage>', '\\1 <stage>', tree_as_string)
  return(tree_as_string)
}
#     "    tree_as_string = re.sub(r'</stage>([^\\s])', r'</stage> \\1', tree_as_string)\n",
#     "    tree_as_string = re.sub(r'([^\\s])<stage>', r'\\1 <stage>', tree_as_string)\n",
#     "    return tree_as_string"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "4cbfc1ad",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def add_pbstmt(filedesc):\n",
add_pbstmt<-function(filedesc){
  pubstmt_as_string<-c('<publicationStmt>',
            '<publisher xml:id="dracor">DraCor</publisher>',
            '<idno type="URL">https://dracor.org</idno>',
            '<availability>',
              '<licence>',
                '<ab>CC0 1.0</ab>',
                '<ref target="https://creativecommons.org/publicdomain/zero/1.0/">Licence</ref>',
              '</licence>',
            '</availability>',
          '</publicationStmt>'
          )

#     "    pubstmt_as_string = \"\"\"\n",
#     "      <publicationStmt>\n",
#     "        <publisher xml:id=\"dracor\">DraCor</publisher>\n",
#     "        <idno type=\"URL\">https://dracor.org</idno>\n",
#     "        <availability>\n",
#     "          <licence>\n",
#     "            <ab>CC0 1.0</ab>\n",
#     "            <ref target=\"https://creativecommons.org/publicdomain/zero/1.0/\">Licence</ref>\n",
#     "          </licence>\n",
#     "        </availability>\n",
#     "      </publicationStmt>\n",
#     "    \"\"\"\n",
#     "    pbsoup = BeautifulSoup(pubstmt_as_string, 'xml')\n",
#     "    pbstmt = pbsoup.publicationStmt\n",
#     "    filedesc.append(pbstmt)"
filedesc<-c(pubstmt_as_string,filedesc)
}
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "440466a0",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def add_sourcedesc(filedesc):\n",
add_sourcedesc<-function(titledesc){
        sourcedesc_as_string <-c(
          '<sourceDesc>',
            '<bibl type="digitalSource">',
              '<name>ENTER SOURCE NAME HERE</name>',
              '<idno type="URL">ENTER SOURCE URL HERE</idno>',
              '<availability status="free">',
                '<p>In the public domain.</p>',
              '</availability>',
            '</bibl>',
          '</sourceDesc>'
          )
        
    # "    sdsoup = BeautifulSoup(sourcedesc_as_string, 'xml')\n",
    # "    sd = sdsoup.sourceDesc\n",
    # "    filedesc.append(sd)"
    filedesc<-c(sourcedesc_as_string,titledesc)
}
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "c2f55925",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def add_rev_desc(header):\n",
add_rev_desc<-function(header){
        revdesc_as_string <-c(
        '<revisionDesc>',
             '<listChange>',
            paste0('<change when="',format(Sys.time(),'%Y-%m-%d'),'">DESCRIBE CHANGE</change>'),
            '</listChange>',
        '</revisionDesc>'
        )
    # "    rdsoup = BeautifulSoup(revdesc_as_string, 'xml')\n",
    # "    rd = rdsoup.revisionDesc\n",
    # "    header.append(rd)"
        header<-c(revdesc_as_string,header)
}
        #    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "6e96490c",
#    "metadata": {},
#    "outputs": [],
#    "source": [
create_header<-function(){
    header <-c("teiHeader",add_pbstmt(filedesc),
               )
    #Tag(name='teiHeader')\n",
#     "    fdesc = Tag(name='fileDesc')\n",
#     "    titlestmt = Tag(name='titleStmt')\n",
#     "    fdesc.append(titlestmt)\n",
#     "    add_pbstmt(fdesc)\n",
#     "    add_sourcedesc(fdesc)\n",
#     "    header.append(fdesc)\n",
#     "    return(header)"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "6469cbef",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def get_div_level(line):\n",
#     "    div_level = 1 # since we already located one # and since 0 is <body> level in this model\n",
#     "    for char in line:\n",
#     "        if char == '#':\n",
#     "            div_level+=1\n",
#     "        else:\n",
#     "            break\n",
#     "    return(div_level)"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "8574f445",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def handle_line_with_markup(first_character, rest_of_line, current_lowest_tag, current_lowest_div):\n",
#     "    ''' handles linke with specific ezdrama markup symbol at the start\n",
#     "    returns new current_lowest_tag which is to be appended further'''\n",
#     "    if first_character == '$':\n",
#     "        new_stage = Tag(name='stage')\n",
#     "        new_stage.append(rest_of_line.strip())\n",
#     "        current_lowest_div.append(new_stage)\n",
#     "        current_lowest_tag = new_stage # при отключениии stage перестает быть мультистрочным и поедать все немаркированные строки после себя\n",
#     "    elif first_character == '(':\n",
#     "        new_stage = Tag(name='stage')\n",
#     "        new_stage.append(first_character)\n",
#     "        new_stage.append(rest_of_line.strip())\n",
#     "        current_lowest_div.append(new_stage)\n",
#     "    elif first_character == '@':\n",
#     "        new_sp = Tag(name='sp')\n",
#     "        new_sp.append(rest_of_line)\n",
#     "        current_lowest_div.append(new_sp)\n",
#     "        current_lowest_tag = new_sp\n",
#     "    elif first_character == '^':\n",
#     "        new_cl = Tag(name='castList')\n",
#     "        new_cl.append(rest_of_line)\n",
#     "        current_lowest_div.append(new_cl)\n",
#     "        current_lowest_tag = new_cl\n",
#     "    elif first_character == '#':\n",
#     "        new_div = Tag(name='div')\n",
#     "        head = Tag(name='head')\n",
#     "        head.append(rest_of_line.strip('#'))\n",
#     "        new_div['level'] = get_div_level(rest_of_line)\n",
#     "        new_div.append(head)\n",
#     "        if new_div['level'] > current_lowest_div['level']:\n",
#     "            current_lowest_div.append(new_div)\n",
#     "        elif new_div['level'] == current_lowest_div['level']:\n",
#     "            current_lowest_div.parent.append(new_div)\n",
#     "        else:\n",
#     "            current_lowest_div.parent.parent.append(new_div)\n",
#     "        current_lowest_div = new_div\n",
#     "        current_lowest_tag = new_div\n",
#     "    return(current_lowest_tag, current_lowest_div)"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "f9714d77",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def add_author_to_header(header, line):\n",
#     "    fdesc = header.find('titleStmt')\n",
#     "    author = Tag(name='author')\n",
#     "    author.append(line[7:])\n",
#     "    fdesc.append(author)"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "b5a7e23a",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def add_title_to_header(header, line):\n",
#     "    titlest = header.find('titleStmt')\n",
#     "    title = Tag(name='title')\n",
#     "    title['type'] = 'main'\n",
#     "    title.append(line[7:])\n",
#     "    titlest.append(title)"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "93ccc439",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def add_subtitle_to_header(header, line):\n",
#     "    titlest = header.find('titleStmt')\n",
#     "    title = Tag(name='title')\n",
#     "    title['type'] = 'sub'\n",
#     "    title.append(line[10:])\n",
#     "    titlest.append(title)"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "96699178",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def add_standoff(tei):\n",
#     "    today = datetime.today().strftime('%Y')\n",
#     "    standoff_as_string = f'''\n",
#     "    <standOff>\n",
#     "        <listEvent>\n",
#     "        <event type=\"print\" when=\"{today}\">\n",
#     "        <desc/>\n",
#     "        </event>\n",
#     "        <event type=\"premiere\" when=\"{today}\">\n",
#     "        <desc/>\n",
#     "        </event>\n",
#     "        <event type=\"written\" when=\"{today}\">\n",
#     "        <desc/>\n",
#     "        </event>\n",
#     "        </listEvent>\n",
#     "        <listRelation>\n",
#     "        <relation name=\"wikidata\" active=\"INSERT\" passive=\"INSERT\"/>\n",
#     "        </listRelation>\n",
#     "    </standOff>\n",
#     "    '''\n",
#     "    standoffsoup = BeautifulSoup(standoff_as_string, 'xml')\n",
#     "    standoff = standoffsoup.standOff\n",
#     "    tei.append(standoff)"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "f1d04c3e",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def parse_lines(file_lines):\n",
#     "    '''This function parses lines of a file \n",
#     "    in ezdrama format and \n",
#     "    produces an output XML tree\n",
#     "    at this stage we only identify: \n",
#     "    -- basic <div> structure\n",
#     "    -- external (not-inside-the-sp) <stage> directions \n",
#     "    -- <sp>-s without internal markup\n",
#     "    '''\n",
#     "    special_symb_list = '@$^#('\n",
#     "    root = Tag(name='TEI')\n",
#     "    header = create_header()\n",
#     "    root.append(header)\n",
#     "    add_standoff(root)\n",
#     "    text = Tag(name='text')\n",
#     "    body = Tag(name='body')\n",
#     "    text.append(body)\n",
#     "    root.append(text)\n",
#     "    \n",
#     "    current_lowest_tag = body\n",
#     "    current_lowest_div = body\n",
#     "    current_lowest_div['level'] = 0\n",
#     "    \n",
#     "    for line in file_lines:\n",
#     "        if line.startswith('@author'):\n",
#     "            add_author_to_header(header, line.strip())\n",
#     "        elif line.startswith('@title'):\n",
#     "            add_title_to_header(header, line.strip())\n",
#     "        elif line.startswith('@subtitle'):\n",
#     "            add_subtitle_to_header(header, line.strip())\n",
#     "        else:\n",
#     "            first_character = line[:1] # отрезаем первый спецсимвол, берем его\n",
#     "            rest_of_line = line[1:] # отрезаем первый спецсимвол, берем остаток\n",
#     "            if first_character in special_symb_list:\n",
#     "                current_lowest_tag, current_lowest_div = handle_line_with_markup(first_character, \n",
#     "                                                                 rest_of_line, \n",
#     "                                                                 current_lowest_tag,\n",
#     "                                                                 current_lowest_div)\n",
#     "            else:\n",
#     "                current_lowest_tag.append(line)\n",
#     "    return(root)"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "8133b238",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def check_prose(first_line_of_speech):\n",
#     "    #print(first_line_of_speech)\n",
#     "    if first_line_of_speech.startswith('~'):\n",
#     "        return False\n",
#     "    return True"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "85b17a03",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def clean_after_translit(line):\n",
#     "    line = line.replace('і', 'i')\n",
#     "    line = line.replace('ї', 'i')\n",
#     "    line = line.replace('і', 'i')\n",
#     "    line = line.replace('є', 'e')\n",
#     "    line = line.replace(\"'\", \"\")\n",
#     "    line = line.replace(\"’\", \"\")\n",
#     "    line = line.replace(\"«\", \"\")\n",
#     "    line = line.replace(\"»\", \"\")\n",
#     "    line = line.replace(\"′\", \"\")\n",
#     "    line = line.replace(\" \", \"_\")\n",
#     "    return line"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "5defe38e",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def add_cast_items(soup):\n",
#     "    castList = soup.find('castList')\n",
#     "    casttext = castList.text\n",
#     "    castList.clear()\n",
#     "    cast_lines = casttext.split('\\n')\n",
#     "    # first line is head\n",
#     "    castHead = Tag(name='head')\n",
#     "    castHead.append(cast_lines[0])\n",
#     "    castList.append(castHead)\n",
#     "    # next lines -- castItems\n",
#     "    for line in cast_lines[1:]:\n",
#     "        castItem = Tag(name='castItem')\n",
#     "        castItem.append(line)\n",
#     "        castList.append(castItem)"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "5bc46015",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def post_process_sp(sp):\n",
#     "    text_of_sp = sp.text \n",
#     "    sp.clear()\n",
#     "    \n",
#     "    text_split_in_lines = text_of_sp.split('\\n')\n",
#     "    first_line = text_split_in_lines[0]\n",
#     "    speaker = Tag(name='speaker')\n",
#     "    sp.append(speaker)\n",
#     "    check_stage = re.search('([^()]+)(\\(.+?\\))([.,:!;])?', first_line)\n",
#     "    if check_stage:\n",
#     "        speaker.append(check_stage.group(1).strip())\n",
#     "        inside_stage = Tag(name='stage')\n",
#     "        inside_stage.append(check_stage.group(2).strip())\n",
#     "        sp.append(inside_stage)\n",
#     "        \n",
#     "        ending_punct = check_stage.group(3)\n",
#     "        if ending_punct is not None:\n",
#     "            speaker.append(ending_punct)\n",
#     "    else:\n",
#     "        speaker.append(first_line.strip())\n",
#     "        \n",
#     "    \n",
#     "    prose = check_prose(text_split_in_lines[1])\n",
#     "    if prose:\n",
#     "        speechtext = Tag(name='p') \n",
#     "    else:\n",
#     "        speechtext = Tag(name='lg')\n",
#     "        text_split_in_lines[1] = text_split_in_lines[1].strip('~')\n",
#     "    for line in text_split_in_lines[1:]:\n",
#     "        if line.startswith('%'):\n",
#     "            inlinestage = Tag(name='stage')\n",
#     "            inlinestage.append(line.strip('%'))\n",
#     "            speechtext.append(inlinestage)\n",
#     "        elif len(line) > 0:\n",
#     "            switch_to_poetry = not check_prose(line)\n",
#     "            check_inline_brackes  = re.findall('([^()]*)(\\(.+?\\)[.,:!;]?)([^()]*)', line)\n",
#     "            if check_inline_brackes:\n",
#     "                for triplet in check_inline_brackes: \n",
#     "                    if len(triplet[0]) > 0:\n",
#     "                        if not prose:\n",
#     "                            poetic_line = Tag(name='l')\n",
#     "                            poetic_line.append(triplet[0].strip('~'))\n",
#     "                            speechtext.append(poetic_line)\n",
#     "                        else:\n",
#     "                            speechtext.append(triplet[0])\n",
#     "                    inside_stage = Tag(name='stage')\n",
#     "                    inside_stage.append(triplet[1].strip())\n",
#     "                    speechtext.append(inside_stage)\n",
#     "                    if len(triplet[2]) > 0:\n",
#     "                        if not prose:\n",
#     "                            poetic_line = Tag(name='l')\n",
#     "                            poetic_line.append(triplet[2])\n",
#     "                            speechtext.append(poetic_line)\n",
#     "                        else:\n",
#     "                            speechtext.append(triplet[2])\n",
#     "            #elif line.startswith('i$'):   \n",
#     "\n",
#     "            else:\n",
#     "                if switch_to_poetry:\n",
#     "                    sp.append(speechtext)\n",
#     "                    speechtext = Tag(name='lg')\n",
#     "                    poetic_line = Tag(name='l') \n",
#     "                    poetic_line.append(line.strip('~'))\n",
#     "                    speechtext.append(poetic_line)\n",
#     "                    prose = False\n",
#     "                elif prose:\n",
#     "                    speechtext.append(line)\n",
#     "                else:\n",
#     "                    poetic_line = Tag(name='l') \n",
#     "                    poetic_line.append(line)\n",
#     "                    speechtext.append(poetic_line)\n",
#     "\n",
#     "\n",
#     "    sp.append(speechtext)\n",
#     "\n",
#     "    if re.search('[йцукенгшщзхъфывапролджэячсмитью]', speaker.text.lower()):\n",
#     "        clean_who = clean_after_translit(translit(speaker.text.strip('.,:!; '), UkrainianISO9)).lower()\n",
#     "    else:\n",
#     "        clean_who = speaker.text.strip('.,:!; ').lower()\n",
#     "    sp['who'] = f'#{clean_who}'"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "e9611da9",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def post_process(soup):\n",
#     "    set_of_char_pairs = set() # множество пар ID + строка \n",
#     "    add_cast_items(soup)\n",
#     "    del(soup.find('body')['level'])\n",
#     "    for sp in soup.find_all('sp'):\n",
#     "        post_process_sp(sp)\n",
#     "        if 'who' in sp.attrs:\n",
#     "            #print[sp\n",
#     "            #try:\n",
#     "            set_of_char_pairs.add((sp['who'], sp.speaker.text.strip('. '))) #\n",
#     "            #except:\n",
#     "            #    pass\n",
#     "    for div in soup.find_all('div'):\n",
#     "        if div['level'] == 0:\n",
#     "            div.attrs = {}\n",
#     "        elif div['level'] == 1:\n",
#     "            div.attrs = {}\n",
#     "            div['type'] = 'act'\n",
#     "        elif div['level'] == 2:\n",
#     "            div.attrs = {}\n",
#     "            div['type'] = 'scene'\n",
#     "    add_particdesc_to_header(soup, set_of_char_pairs)\n",
#     "    add_rev_desc(soup.teiHeader)\n",
#     "\n",
#     "    soup['xmlns'] = \"http://www.tei-c.org/ns/1.0\"\n",
#     "\n",
#     "    return(soup)"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "f8213fda",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "def add_particdesc_to_header(soup, set_of_char_pairs):\n",
#     "    #print(set_of_char_pairs)\n",
#     "    profileDesc = Tag(name = 'profileDesc')\n",
#     "    particDesc = Tag(name = 'particDesc')\n",
#     "    profileDesc.append(particDesc)\n",
#     "    listPerson = Tag(name = 'listPerson')\n",
#     "    particDesc.append(listPerson)\n",
#     "    for pair in set_of_char_pairs:\n",
#     "        person = Tag(name = 'person')\n",
#     "        person['xml:id'] = pair[0].strip('#')\n",
#     "        persName = Tag(name = 'persName')\n",
#     "        person.append(persName)\n",
#     "        #print(pair[1])\n",
#     "        persName.append(pair[1])\n",
#     "        listPerson.append(person)\n",
#     "    teiHeader = soup.find('teiHeader')\n",
#     "    teiHeader.append(profileDesc)"
#    ]
#   },
#   {
#    "cell_type": "markdown",
#    "id": "68dee426",
#    "metadata": {},
#    "source": [
#     "## Use\n",
#     "\n",
#     "The code below uploads a [sample play](https://raw.githubusercontent.com/dracor-org/ezdrama/main/sample.txt) marked with EzDrama format and converts it. If you want to convert sth else, then upload your own file and redefine \n",
#     "`path = 'sample.txt'` with the path to your own file"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "69b379cd",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "if not os.path.exists('sample.txt'):\n",
#     "    wget.download('https://raw.githubusercontent.com/dracor-org/ezdrama/main/sample.txt')"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "6bab60b6",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "path = 'sample.txt'\n",
#     "parse_file(path)"
#    ]
#   },
#   {
#    "cell_type": "markdown",
#    "id": "bf1bbe89",
#    "metadata": {},
#    "source": [
#     "## adding indentation"
#    ]
#   },
#   {
#    "cell_type": "markdown",
#    "id": "6710ca3a",
#    "metadata": {},
#    "source": [
#     "take the DraCor indentation scheme from Github"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "efffe6a6",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "if not os.path.exists('format.conf'):\n",
#     "    wget.download('https://raw.githubusercontent.com/dracor-org/ezdrama/main/format.conf')"
#    ]
#   },
#   {
#    "attachments": {},
#    "cell_type": "markdown",
#    "id": "b962e99f",
#    "metadata": {},
#    "source": [
#     "#### indent with xml formatter\n",
#     "\n",
#     "Preparation: \n",
#     "\n",
#     "* If running locally, install the formatter on your machine:http://www.kitebird.com/software/xmlformat/ \n",
#     "\n",
#     "* Elif running in Colab, run the following lines:"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "072f2783",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "if not os.path.exists('xmlformat.pl'):\n",
#     "    wget.download('https://raw.githubusercontent.com/dracor-org/ezdrama/main/xmlformat.pl')"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "a96f2514",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     " !cp xmlformat.pl /usr/local/bin/xmlformat"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "7a968b02",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "!chmod 755 -R /usr/local/bin/xmlformat"
#    ]
#   },
#   {
#    "cell_type": "markdown",
#    "id": "7217f65f",
#    "metadata": {},
#    "source": [
#     "actual indenting"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "389f48f0",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "newfilepath = path.replace('.txt', '.xml')"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "7a9d4e08",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "os.system(f'xmlformat --config-file=format.conf \\\"{newfilepath}\\\" > \\\"{newfilepath.replace(\".xml\",\"\")}_indented.xml\\\" ')"
#    ]
#   },
#   {
#    "cell_type": "code",
#    "execution_count": null,
#    "id": "e0fdbfa9",
#    "metadata": {},
#    "outputs": [],
#    "source": [
#     "os.system(f'rm {newfilepath}')"
#    ]
#   }
#  ],
#  "metadata": {
#   "kernelspec": {
#    "display_name": "Python 3 (ipykernel)",
#    "language": "python",
#    "name": "python3"
#   },
#   "language_info": {
#    "codemirror_mode": {
#     "name": "ipython",
#     "version": 3
#    },
#    "file_extension": ".py",
#    "mimetype": "text/x-python",
#    "name": "python",
#    "nbconvert_exporter": "python",
#    "pygments_lexer": "ipython3",
#    "version": "3.9.13"
#   }
#  },
#  "nbformat": 4,
#  "nbformat_minor": 5
# }
