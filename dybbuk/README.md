# 14272.info
## dybbuk.overview
this folder contains scripts and tables associated to working on a transcription in a yiddish drama corpus. first is an essai to search/replace patterns within the 1st run transcription exported from TRANSKRIBUS according to a growing set of ground truth pages which were manually corrected mostly for the vocalisation in the hebrew type as the rest of the recognition was about 95% perfect. simples approach was to generate a list of overall tokens in the document and a list of tokens in the ground truth pages. in the table we manually assigned replacements to tokens of near distance i.e. same tokens as in gold set but without vocalisation.   

## vocalisation issue
### 2nd approach

  - extract ground truth tokens
  - extract training tokens
  - strip vocalisation of both sets
  - match training to gold set tokens
  - find replacement positions
  - replace token at position in training pages with vocalised gold set token

### 3rd approach.NT.15362
global replacement of common misrecognized vocalisations via find/replace table
see: <dybbuk_transkribus-edit.R> and <replace.df-m.csv>



## proceeding
TEI refactoring for yidracor: <https://github.com/esteeschwarz/dybbuk-cor>