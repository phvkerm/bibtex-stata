pr drop _all
cd C:\Users\philippe.vankerm\Documents\professional\adofiles-dvp\bibtex\code
bibtex import using lis.bib , clear saving(lis-long.dta) replace
bibtex import using lis.bib , clear wide saving(lis-wide.dta) replace
bibtex buildname author , from(author?*) style(comma) replace
list author*
bibtex buildname editor , style(standard) replace
bibtex buildname author , style(lnameonly) from(author1) replace generate(botafogo)
list author*
list editor*
drop author
bibtex export using lis-out.bib , replace buildauthor(style(comma)) buildeditor(style(standard)) 
bibtex export using lis-out-2.bib , replace buildauthor(style(comma)) exclude(title) include(author pages year) order(author year title) sort(entrytype year bibid)
 

