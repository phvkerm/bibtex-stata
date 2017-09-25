*! bibtex 0.1, 20170915
* help should say, input to be reasonably `clean' -- bibtex import does not check for syntax errors in the bib file ; such can cause havoc in the import... 
* skip(string) is a list of bibtex fields which are to be ignored in the import, e.g., skip(abstract url) .  

pr def bibtex  
  
    version 13
    
    gettoken subcmd 0 : 0 , parse(" ,")
   
    if `"`subcmd'"' == "" | `"`subcmd'"' == "," {
      di as error "please specify a bibtex subcommand"
      exit 198
      }    

    if `"`subcmd'"'=="import" {
      bibtex_import `macval(0)' 
      exit
      }    
    if `"`subcmd'"'=="export" {
      bibtex_export `macval(0)' 
      exit
      }    
    if `"`subcmd'"'=="journallist" {
      bibtex_journallist `macval(0)' 
      exit
      }    
	di as error `" `subcmd'  not a valid bibtex subcommand"'  

end
  
/* ---------------------------------------------------------------------- */
/*    SUB COMMANDS                                                        */
/* ---------------------------------------------------------------------- */


pr def bibtex_export 
	version 13
    syntax [if] [in] using/ [ ,  CONDition(string) wide skip(string)  replace type(string)  ]   
	* wide informs that dta is in wide format
	* condition() applies a condition to select entries to be exported
	* skip() prevents fields in string to be exported
	* keep() only selects fields(). skip() and keep() are mutually exclusive
	
	if ("`type'"=="") loc type "bib"
	if ("`: char _dta[BibFormat]'"=="wide") 	loc wide "wide"

	tempname fout
    file open `fout' using `"`using'"' , write text `replace'

	// Arrange data into wide form (easier to handle)
	if ("`wide'"=="") {
		preserve
		levelsof field , local(fields)
		qui gen labid = .
		loc j 0
		foreach lab of local fields {
			replace labid = `++j' if field=="`lab'" 
		}
		drop field
		reshape wide content , i(id) j(labid)
		forvalues k=1/`j' {
			rename content`k'  `: word `k' of `fields' '
		}
	}
	
	// Author and editor fields
	foreach var of varlist author* {	
		if (strlower("`var'")=="author") {
			gen str1000 tmpauthor = author
			continue , break
		}	
		if ("`var'"=="author1") gen str1000 tmpauthor = author1
		else {
			replace tmpauthor = tmpauthor + cond(`var'=="", "" , " and " + `var') 
		}	
	}
	foreach var of varlist editor* {	
		if (strlower("`var'")=="editor") {
			gen str1000 tmpeditor = editor
			continue , break
		}	
		if ("`var'"=="editor1") gen str1000 tmpeditor = editor1
		else {
			replace tmpeditor = editor + cond(`var'=="", "" , " and " + `var') 
		}	
	}
	
	loc varsordered ??  
	
	forvalues i=1/`=_N' 	{
		loc lcontent  = TYPE[`i']
		file write `fout'   `"@`macval(lcontent)'{`=bibid[`i']',"' _n 
		file write `fout'  _tab
		loc first 1 
		foreach var of varlist `varsordered' {
			loc lcontent  = `var'[`i']
			if (`"`lcontent'"'!="") {
				if (!`first') {
					write `fout'  "," _n
					loc first 0
				}
				* subsinstr for tmpauthor and tmpeditor
				file write `fout'  `=subsinstr("`var'","tmp","",1)  _tab(2)  "="  _tab   `"{`macval(lcontent)'}"' 
			}
		}
		file write `fout'  _n "}" _n 
	}
	
	
	if ("`wide'"=="") 	restore

	
	
	// handle authors and editors.
	
	
		tempvar tag last
		gen byte `tag' = 1 - (field=="TYPE")  
		sort bibid id `tag'
		bys bibid : gen byte `last' = (_n==_N) 
	
		if ("`type'"=="bib") {
			forvalues i=1/`=_N' 	{
				di "i==`i'"
				if ( `tag'[`i']==0 )	{
					loc lcontent  = content[`i']
					file write `fout'   `"@`macval(lcontent)'{`=bibid[`i']',"' 
				}
				else {
					file write `fout'  _tab
					file write `fout'   `"`=field[`i']'"'
					loc lcontent  = content[`i']
					file write `fout'   _tab "=" _tab  `"{`macval(lcontent)'}"'
					if (`last'[`i']==0) {
						file write `fout'   ","
					}
					else {
						file write `fout'  _n
						file write `fout'  "}" 
					}
				}
				file write  `fout' _n
			}
			file write  `fout' _n
		}
		if ("`type'"=="btwsql") {
		}
		
	}
	
	if ("`wide'=="") {
		other way...
	}
	
	file close `fout'
	
end

		

pr def bibtex_exportold 
	version 13
    syntax [if] [in] using/ [ ,  CONDition(string) wide skip(string)  replace type(string)  ]   
	* wide informs that dta is in wide format
	* condition() applies a condition to select entries to be exported
	* skip() prevents fields in string to be exported
	* keep() only selects fields(). skip() and keep() are mutually exclusive
	
	if ("`type'"=="") loc type "bib"

	// handle authors and editors.
	
	tempname fout
    file open `fout' using `"`using'"' , write text `replace'

	if ("`wide'=="") {
	
		tempvar tag last
		gen byte `tag' = 1 - (field=="TYPE")  
		sort bibid id `tag'
		bys bibid : gen byte `last' = (_n==_N) 
	
		if ("`type'"=="bib") {
			forvalues i=1/`=_N' 	{
				di "i==`i'"
				if ( `tag'[`i']==0 )	{
					loc lcontent  = content[`i']
					file write `fout'   `"@`macval(lcontent)'{`=bibid[`i']',"' 
				}
				else {
					file write `fout'  _tab
					file write `fout'   `"`=field[`i']'"'
					loc lcontent  = content[`i']
					file write `fout'   _tab "=" _tab  `"{`macval(lcontent)'}"'
					if (`last'[`i']==0) {
						file write `fout'   ","
					}
					else {
						file write `fout'  _n
						file write `fout'  "}" 
					}
				}
				file write  `fout' _n
			}
			file write  `fout' _n
		}
		if ("`type'"=="btwsql") {
		}
		
	}
	
	if ("`wide'=="") {
		other way...
	}
	
	file close `fout'
	
end



pr def bibtex_import , rclass

    version 13
    syntax using/ , [ noSPLITnames wide skip(string) clear  SAVING(string asis) replace]   

	loc loaddta 0
	if ((c(k)+c(N)==0) | ("`clear'"=="clear") ) {
		loc loaddta 1
	}
	else if (`"`saving'"'=="") {
			di as error "no; data in memory would be lost"
			exit 198
	}	

	loc skiplist
	if ("`skip'"!="") {
		foreach word of local skip {
			loc skiplist `"`skiplist' "`=strlower("`word'")'" ,"'  
		}
	}	
	loc skiplist `"`skiplist' " " "'
	
	tempname fin fout
	if (`"`saving'"'=="") {
		tempfile bibtexout
		local replace replace
	}	
    file open `fin' using `"`using'"' , read text 
	qui postfile `fout' id str30 bibid str20 fields str2045 content  using `"`saving'`bibtexout'"', `replace' 

	loc entrylist article|techreport|incollection|inbook|inproceedings|book|unpublished|misc|phdthesis|masterthesis|software|talk|seminar
	loc i 0   			// count bibtex entries
	loc strentry 
	loc line 
	loc finished 0
	while !(`finished') {
		if  ( (r(eof)==1) | (regexm(strlower(`"`macval(line)'"'),"^@(`entrylist')") ) ) { 
			* Active entry completed: parse and read it
			if (`"`macval(strentry)'"'!="") {
				* get the entry type (stuff between first @ and {)
				loc ++i
				loc posat = strpos(`"`macval(strentry)'"',"@")
				loc poscubra = strpos(`"`macval(strentry)'"',"{")
				loc type = strtrim(substr(`"`macval(strentry)'"', `=`posat'+1' , `=`poscubra'-`posat'-1'))
				loc strentry = substr(`"`macval(strentry)'"', `=`poscubra'+1' , .)
				* get the entry id (stuff until the first ,
				loc poscomma = strpos(`"`macval(strentry)'"',",")
				loc id = strtrim(substr(`"`macval(strentry)'"', 1, `=`poscomma'-1'))
				loc strentry = substr(`"`macval(strentry)'"', `=`poscomma'+1' , .)
				* parse content ...
				loc j 0
				loc finishedentry 0
				while  !(`finishedentry') {
					loc posequal = strpos(`"`macval(strentry)'"',"=")
					if (`posequal'>0) {
						loc ++j
						loc field`j' = strlower(strtrim(substr(`"`macval(strentry)'"',1,`=`posequal'-1')))
						loc strentry = strltrim(substr(`"`macval(strentry)'"', `=`posequal'+1' , .))
					
						if !inlist("`field`j''",`skiplist') {  // skip exluded entry types
						
							* now read content (if enclosed in " " or { } )
							if ( (substr(`"`macval(strentry)'"',1,1)==`"{"') | (substr(`"`macval(strentry)'"',1,1)==`"""') )  {
								if (substr(`"`macval(strentry)'"',1,1)==`"{"') 	{
									loc opendelimiter "{"
									loc closedelimiter "}"
								}	
								else {
									loc opendelimiter `"""'
									loc closedelimiter `"""'
								}
								loc strentry = strltrim(substr(`"`macval(strentry)'"', 2 , .))

								loc delcnt 1
								loc content`j' 
								while (`delcnt'>0) {
									loc c = substr(`" `macval(strentry)'"', 2, 1)
									if strpos(`"(`macval(c)')"', char(92))>0  {   
											// if encounter escape char (backslash) : store it, then store the following char too withouth check for end of field
											// the strpos and brackets are used to handle ` and ' and " (to avoid local macro expansion)
										loc content`j' `"`macval(content`j')'`macval(c)'"'
										loc c = substr(`" `macval(strentry)'"', 3, 1)
										loc content`j' `"`macval(content`j')'`macval(c)'"'
										loc c = substr(`" `macval(strentry)'"', 4, 1)
										loc strentry = substr(`" `macval(strentry)'"', 4, .)
									}	
									if strpos(`"(`macval(c)')"', `"`closedelimiter'"')>0    loc --delcnt
									if (`delcnt'>0) {
										if strpos(`"(`macval(c)')"', `"`opendelimiter'"')>0  loc ++delcnt
										loc content`j' `"`macval(content`j')'`macval(c)'"'
									}	
									loc strentry = substr(`" `macval(strentry)'"', 3, .)
								}
							}
							else {   // read stuff not enclosed in " " or { }
								loc content`j' 
								loc delcnt 1
								while (`delcnt'>0) {
									loc c = substr(`"`macval(strentry)'"', 1, 1)
									if inlist(`"`macval(c)'"',  " ", "," , "}")  loc --delcnt
									if (`delcnt'>0) {
										loc content`j' `macval(content`j')'`macval(c)'
										loc strentry = substr(`"`macval(strentry)'"', 2, .)
									}		
								}	
							}
						
							* now move to next entry field  	
							loc strentry = substr(strltrim(`"`macval(strentry)'"'), 2, .)
						}
						else {  // move to next entry
							loc field`j' 
							loc --j
							loc jumpto = regexm(`"`macval(strentry)'"' , `"[ ]+[a-z,A-Z]+[ ]*[=][ ]*(\"|\{|[0-9])*"')
							loc pos = strpos(`"`macval(strentry)'"' , regexs(0))
							loc strentry = substr(`"`macval(strentry)'"',`pos',.)
								// tries to find next entry:
								// 1+spaces then-a-word 0+spaces = 0+spaces {-or"-or-number 		
								// not 100% foolproof but should be about right most cases! 
						}
						
					}	
					else {
						loc finishedentry 1
					}
				} //  parsing of bibitem i info completed!
			
				//
				// store info
				//
				// number of elements
				loc n`i' = `j'   
				// split author and editor strings  
				if ("`splitnames'"=="") { 
					forv j=1/`n`i'' {
						* authors:
						if (strlower("`field`j''") == "author") {
							loc strnames `macval(content`j')'
							loc pos 999
							loc kaut 0
							while (`pos'>0) { 
								loc pos = strpos(`"`macval(strnames)'"' , " and ")  
								if (`pos'>0) {
									loc author`++kaut' = substr(`"`macval(strnames)'"', 1, `=`pos'-1'  )
									loc strnames = substr(`"`macval(strnames)'"', `=`pos'+5' , . )
								}	
							}
							loc author`++kaut' `macval(strnames)'
						}
						* editors:
						if (strlower("`field`j''") == "editor") {
							loc strnames `macval(content`j')'
							loc pos 999
							loc kedit 0
							while (`pos'>0) { 
								loc pos = strpos(`"`macval(strnames)'"' , " and ")  // picks up the last one
								if (`pos'>0) {
									loc editor`++kedit' = substr(`"`macval(strnames)'"', 1 , `=`pos'-1'  )
									loc strnames = substr(`"`macval(strnames)'"', `=`pos'+5' , . )
								}	
							}
							loc editor`++kedit' `macval(strnames)'
						}
					}
				}	

				// post in dta file 
				* type
				post `fout' (`i') ("`id'") ("TYPE") (strupper("`type'")) 
				* authors and editors 
				if ("`kaut'"!="") {
					forv j=1/`kaut' {
						post `fout' (`i') ("`id'") ("author`j'") (`"`macval(author`j')'"')
					}
					loc kaut 	
				}	
				if ("`kedit'"!="") {
					forv j=1/`kedit' {
						post `fout' (`i') ("`id'") ("editor`j'") (`"`macval(editor`j')'"')
					}
					loc kedit 	
				}	
				* all the rest
				forv j=1/`n`i'' {
					post `fout' (`i') ("`id'") ("`field`j''") (`"`macval(content`j')'"')
				}
		
				// Treatment completed.
				loc strentry
			}
			if (r(eof)==1) loc finished 1
		}
		
		* Fill strentry with latest line read
		loc strentry `macval(strentry)' `macval(line)'
	
		* read next line (skipping empty lines)
		loc line 

		file read `fin' line
		local line = subinstr(`"`macval(line)'"',char(9)," ",.)    // replace all tabs by a white space 
		local line = strtrim(`"`macval(line)'"')
		while ((`"`macval(line)'"'=="") & (r(eof)==0)) {
			file read `fin' line
			local line = subinstr(`"`macval(line)'"',char(9)," ",.)    // replace all tabs by a white space 
			local line = strtrim(`"`macval(line)'"')
		}
	}		
	postclose `fout'
	return scalar N = `i'
	return local bibsource `"`using'"'

	if (`loaddta'==0)  preserve
	qui {
		use `"`saving'`bibtexout'"' , clear
		sort id field 
		char define _dta[BibFormat]	"long"
		if ("`wide'"!="") {
			levelsof field , local(fields)
			gen labid = .
			loc j 0
			foreach lab of local fields {
				replace labid = `++j' if field=="`lab'" 
			}
			drop field
			reshape wide content , i(id) j(labid)
			forvalues k=1/`j' {
				label variable 	content`k' "`: word `k' of `fields' '"	
				rename content`k'  `: word `k' of `fields' '
			}
			order id bibid TYPE
			char define _dta[BibFormat]	"wide"
		}
		char define _dta[Source] `"Original import from [`using'] by -bibtex- on c(current_date)."' 
		compress
		save `"`saving'`bibtexout'"' , replace	
	}
	if (`loaddta'==0)  restore
	
end



exit
	
	
	
