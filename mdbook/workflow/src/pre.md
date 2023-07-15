# prerequisites
I will show you in a few steps an example workflow of how to prepare some base documents that enable further analysis of a dramatic text.  
assumed we start with a plain text file, a lot of work had been done by others yet and we can proceed to the TEI refactoring of the text. if we do not have a text file yet, first will be to transcribe some  source of the text, usually a .pdf or collection of .jpegs. for that purpose you can either transcribe the text manually, from picture to text, or you use e.g. [transkribus][1], a user friendly framework for OCR (optical character recognition). with that half of the work is done by the algorithm, but you still have to check the automatic transcription for recognition failures.  
next step if you have the transcript ready will be to upload the text page by page to [wikisource][2] where it can be proofread by others. if theres two correction runs ready, you can download the proper version of the text from which we proceed to the TEI.  
theres multiple ways of how you can get to the TEI text. one is to wrap text elements which need to be \<marked up\> with [oxygen][3], a powerful XML editor to which the FU grants a permanent license. another way would be to use an R-script that does lot of work yet, but you'll have to very precisely define text specific parameters to be able to apply the script to your drama text. to use the script you have to be some familiar with the R language which is scheduled in class.  
if all that is done you possess a finalized TEI text which allows further analysis of the drama again e.g. using python or R or e.g.[gephi][4] for network analysis.

[1]:	https://readcoop.eu/transkribus/
[2]:	https://de.wikisource.org/wiki/Kategorie:Autoren
[3]:	https://www.oxygenxml.com/xml_editor.html
[4]:	https://gephi.github.io/