# 15133.info
Q: <https://books.google.de/books?id=kYk6AAAAcAAJ&pg=PA1&redir_esc=y#v=onepage&q&f=false>

#### workflow
- pg. 6:61-1 of ggl source OCR in transkribus
- raw uncorrected in [txt](txt) folder, models (0.5 each), pg. 2-4:
    - 2. ONB_Newseye
    - 3. Transkribus print 0.3
    - 4. Transkribus Print M1
    - 32. German Fraktur W(iene)r_Diarium

- automate
    - umlaut substitution/correction
    - f/s

- TEI chk:
  - p.26, p.39, p.24 if 2 \<stage> nacheinander at beginning of \<sp> then \<p> is empty? (ezd:121, xml:349)
  - xml:469: \<pb> in between words, xml:641, p.45 missing l in Qual after \<pb> in word
  - p.41, xml:596: #unclear markup! creates div act, trailing iwanette sp end of page, wrong pagenumber, correct in transkribus.
  - xml:827: end of play line