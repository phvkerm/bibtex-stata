*! bibtex 0.3, 20171106: start adding secondary features:  journallist and namelist
* bibtex 0.2, 20171031: export
* bibtex 0.1, 20170915: import



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
    if `"`subcmd'"'=="buildname" {
      bibtex_buildname `macval(0)' 
      exit
      }    
/*
    if `"`subcmd'"'=="widetolong" {
      bibtex_widetolong `macval(0)' 
      exit
      }   
    if `"`subcmd'"'=="longtowide" {
      bibtex_longtowide `macval(0)' 
      exit
      }   
    if `"`subcmd'"'=="duplicates" {
      bibtex_duplicates `macval(0)' 
      exit
      }   
*/
*	if `"`subcmd'"'=="summarize" {   // not very useful
*      bibtex_summarize `macval(0)' 
*      exit
*      }    
	if `"`subcmd'"'=="journallist" {
      bibtex_journallist `macval(0)' 
      exit
      }    
	if `"`subcmd'"'=="namelist" {
      bibtex_namelist `macval(0)' 
      exit
      }
	di as error `" `subcmd'  not a valid bibtex subcommand"'  

end



  
/* ---------------------------------------------------------------------- */
/*    SUB COMMANDS                                                        */
/* ---------------------------------------------------------------------- */


// -------------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------------------

// converts a wide bibdta file to long bibdta file (or vice versa): 
//   sets the data characteristics
//   checks for errors
  
pr def bibtex_widetolong 
	version 13
end

pr def bibtex_longtowide 
	version 13
end

// looks for duplilcate on "bibid" then attempt to identify duplicates on title and authors, etc...  
pr def bibtex_duplicates 
	version 13
end


// provides summary stats of the file
// how many entries
//   tabulate  year
//   tabulate  type
pr def bibtex_summarize 
	version 13
end



// -------------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------------------

pr def bibtex_export 
	version 13
    syntax [if] [in] using/  [ ,  	///
		type(string)        		///   type of export, default creates a bib file   (bib|btwsql|...)  
		wide     					///	informs that the data is in wide format (it can also check this info from data characteristic _dta[format], otherwise assumes long form)	
		replace        				///  replace export file if exsists
		CONDition(string)    		///   select entries to be exported: if ... in ... format (default, all are exported)
		sort(string)		 		///    sort the entries in the export file by XXX (XXX are field names, e.g.  author, title, year ... , e.g. sort(author year) , sort(bibd), etc..
		///		
		EXcludefields(string)     	///     ne pas exporter ces champs
		INcludefields(string)      	///     exporter ces champs
		noDROPNAMES					///   by default fiels author1, author2 ... and editor1 editor2 are not expored ; 
		buildauthor(string asis)	///   create the variable author; string containst syntax for bibtex buildname author
		buildeditor(string asis)	/// create the variable author; string containst syntax for bibtex buildname author
		ORDERfields(string)			///  order fields ; by default, entries are exported in same order as appearing in the file
		///
		bibid(string)      			///  name of variable holding the bibtex entry identifier (otherwise read in _dta[bibid]  or use 'bibid') 	   
		numid(string)      			///  name of variable holding a numberi identifier (otherwise read in _dta[bibid]  or use 'numid'); save as numid(none) if data contains none 	
		entrytype(string)			///  name of the variable (or field) holding the type of entry 
		fieldvar(varname)			/// if data in long form specifies which var contains the field name (otherwise read in _dta[fieldvar] or use 'field')		
		contentvar(varname)			/// if data in long form specifies which var contains the content of entry (otherwise read in _dta[contentvar] or use 'content')		
		]   
	
	
	// parse options:
	// -------------
	if ("`type'"=="") loc type "bib"   // default export type
	
	if ("`: char _dta[Format]'"=="wide") 	loc wide "wide"     	// check format of data if stored in dta char

	if ("`bibid'"=="")    	local bibid 	:  char _dta[bibid]  // name of the variable holding the string entry  identifier
	if ("`bibid'"=="")    	local bibid 	bibid
	if ("`numid'"=="none")	local numid 	
	else {
		if ("`numid'"=="")			local numid 	:	char _dta[numid]
		if ("`numid'"=="")			local numid 	numid 
	}
	if ("`entrytype'"=="")   	local entrytype		: char _dta[entrytype]
	if ("`entrytype'"=="")   	local entrytype		entrytype
	 

	// initiate export file:
	// --------------------
	tempname fout
    file open `fout' using `"`using'"' , write text `replace'


	// organise data in wide form (if necessary):
	// -----------------------------------------
	if ("`wide'"=="") {
		// I assume the long file has variables  [numid] bibid field content  -->   [numid]  bibid content1 content2 ... --> [numid] bibid TYPE title  year ...  (one line must be TYPE)
  		// if numid does not exist, it is created from distinct values of bibid    
		preserve
		if ("`fieldvar'"=="")		local fieldvar 	:	char _dta[fieldvar]
		if ("`fieldvar'"=="")		local fieldvar 	field 
		if ("`contentvar'"=="")		local contentvar :	char _dta[contentvar]
		if ("`contentvar'"=="")		local contentvar content 		
		if ("`numid'"=="") {
			tempvar id
			bys `bibid' : qui gen `id' = _n==_N
			qui replace `id' = sum(`id')			
			loc numid `id'
		}	
		levelsof `fieldvar' , local(fields)
		tempvar labid
		qui gen `labid' = .
		loc j 0
		foreach lab of local fields {
			replace `labid' = `++j' if `fieldvar'=="`lab'" 
		}
		drop `fieldvar'
		reshape wide `contentvar'  , i(`numid') j(`labid')
		forvalues k=1/`j' {
			rename `contentvar'`k'  `: word `k' of `fields' '
		}
	}
	
	
	// selection of entries to export:
	// ------------------------------
	tempvar toexport 
	gen byte  `toexport'  =  0    `condition'    // inverted use of 0 and 1  
	sort `toexport' `sort' 
	qui count if `toexport'==0
	if (r(N)==0) {
		di as error "No entries to export."
		error 2000
	}	
	loc N = r(N)
	
	
	// handle authors and editors:
	// --------------------------	
	if ("`buildauthor'"!="") {
		bibtex buildname author , `buildauthor' replace
	}
	if ("`buildeditor'"!="") {
		bibtex buildname editor , `buildeditor' replace
	}		
	
	// prepare the list of variables for the export
	// --------------------------------------------
	if ("`excludefields'"!="")  unab excludefields : `excludefields'   // handle abrevs
	loc toexclude  `numid' `bibid' `entrytype' `excludefields' `toexport'
	if ("dropnames'"=="")  	loc toexclude  `toexclude' author?* editor?*
	qui describe , varlist
	loc vlist `r(varlist)'	
	if ("`includefields'"!="") {
		unab includefields : `includefields'
		loc vlist : list local(vlist)  &  local(includefields)   // keep vars which are in the data and in includefields
	}
	loc vlist : list local(vlist)  -  local(toexclude)   // drop numid and bibid along with vars which are in excludefield

	if ("`orderfields'"!="") {
		unab orderfields : `orderfields'	
		loc orderfields : list local(orderfields) & local(vlist)
		loc outorderfields : list local(vlist) - local(orderfields) 
		loc vlist `orderfields' `outorderfields'
	}
	
	// here vlist contains fields which are to be exported and in the required order
		
	// export to bib file:
	// ------------------
	if ("`type'"=="bib") {
		forvalues indx=1/`N' 	{
			loc lcontent  = `entrytype'[`indx']
			file write `fout'   `"@`macval(lcontent)'{`=`bibid'[`indx']',"' _n 
			loc first 1 
			foreach var of varlist `vlist' {
				loc lcontent  = `var'[`indx']
				if (`"`macval(lcontent)'"'!="") {
					if (!`first') {
						file write `fout'  "," _n
					}
					* subsinstr for tmpauthor and tmpeditor
					file write `fout'  _tab "`=subinstr("`var'","tmp","",1)'"  _tab(2)  "="  _tab   `"{`macval(lcontent)'}"' 
					loc first 0
				}
			}
			file write `fout'  _n "}" _n 
		}
	} // end export to .bib format
	

	if ("`type'"=="btwsql") {
	}
	if ("`type'"=="...format Orbi? ") {
	}
	
	if ("`wide'"=="") 	restore
	
end


// -------------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------------------
pr def bibtex_buildname , rclass
    version 13
    syntax anything(name=nametype) , [ from(varlist) Generate(name) replace STYle(string) bind(string asis) CONDition(string) ]
	// this will create a 'bibtyle' variable author or editor (or whatever filled in generate() )
	// on the basis of variables passed in from() and style().
	// style determines the bibtex style: LNAME, FNAME or FNAME LNAME (or FNAME only or LNAME only or 'as is')
	// from is a list of variables that contain the data about each author, typically author1, author2 etc. 
	/// it only works with wide formats!
	
	if !inlist("`nametype'","author","editor") {
		di as error "only buildname author or buildname editor are allowed"
		exit 198
	}
	if !inlist("`style'",  "", "asis", "standard", "comma", "fnameonly", "lnameonly") {
		di as error "style(`style') unknown: use style(asis|standard|comma|fnameonly|lnameonly)"
		exit 198
	}	
	if ("`style'"=="")  loc style comma
	
	
	if ("`generate'"=="") {
		loc generate `nametype'
	}	
	cap confirm new variable `generate'
	if (_rc == 0) qui gen str999 `generate' = ""
	else {
		if ("`replace'"!="") {
			qui drop `generate'
			qui gen str999 `generate' = ""
		}
		else {
			di as error "`generate' already exists"
			exit 198
		}		
	}	
	
	// bind is the string to use between names, defualt is " and "
	if ("`bind'"=="") loc bind "and"

	if ("`from'"=="")  loc from "`nametype'?*" // all authorX  or editorX variables
	unab from : `from'
	loc prefix ""
	tempvar fname lname tvar posbraf posbral poscomf poscoml posspaf posspal
	qui gen str999 `fname' = ""
	qui gen str999 `lname' = ""
	qui gen str999 `tvar' = ""
	qui gen int `posbraf' = .
	qui gen int `posbral' = .
	qui gen int `poscomf' = .
	qui gen int `poscoml' = .
	qui gen int `posspaf' = .
	qui gen int `posspal' = .
	foreach var of varlist `from' {
		qui replace `tvar' = strtrim(`var')  `condition'
		if ("`style'"=="asis") {
			qui replace `generate' = `generate' + "`prefix'" + `tvar' 		
		}
		else {
			* read var content and generate fname and lname:
			qui replace `posbraf' = strpos(`tvar', "{")
			qui replace `posbral' = strrpos(`tvar', "}")
			qui replace `poscomf' = strpos(`tvar', ",")
			qui replace `poscoml' = strrpos(`tvar', ",")
			qui replace `posspaf' = strpos(`tvar', " ")
			qui replace `posspal' = strrpos(`tvar', " ")
			qui replace `fname' = ""
			qui replace `lname' = ""
			qui count if (`posbraf'==0 & `posbral'>0) | (`posbraf'>0 & `posbral'==0)
			if (r(N)>0) di as error "Errors detected in `=r(N)' entries of `var' (curly brackets not matched)
			// no brackets ...
				//	 with leading comma:  LNAME , FNAME 
				qui replace `lname' = substr(`tvar', 1 , `poscomf'-1)        			if (`posbraf'+`posbral'==0) & `poscomf'>0     
				qui replace `fname' = substr(`tvar', `poscomf'+1 , strlen(`tvar') )     if (`posbraf'+`posbral'==0) & `poscomf'>0    			     
				//	 without comma:   FNAME LNAME
				qui replace `fname' = substr(`tvar', 1 , `posspal'-1 )      			if (`posbraf'+`posbral'==0) & `poscomf'==0    
				qui replace `lname' = substr(`tvar', `posspal'+1 , strlen(`tvar') ) 	if (`posbraf'+`posbral'==0) & `poscomf'==0    
			// curly bracket detected... 
				// no text outside of brackets:  {LNAME}   : fname empty 
				qui replace `lname' = `tvar'        									if (`posbraf'==1 & `posbral'==strlen(`tvar'))     
				qui replace `fname' = ""      											if (`posbraf'==1 & `posbral'==strlen(`tvar'))	
				// leading comma before brackets : LNAME , {... {...} , ... }	  
				qui replace `lname' = substr(`tvar', 1 , `poscomf'-1)        			if (`posbraf'+`posbral'>0) & inrange(`poscomf' , 1 , `posbraf')     
				qui replace `fname' = substr(`tvar', `poscomf'+1 , strlen(`tvar') )     if (`posbraf'+`posbral'>0) & inrange(`poscomf' , 1 , `posbraf')  	
				// trailing comma after brackets :   {... {...} , ... }	, FNAME
				qui replace `lname' = substr(`tvar', 1 , `poscoml'-1)        			if (`posbraf'+`posbral'>0) & inrange(`poscoml' , `posbral' , strlen(`tvar'))     
				qui replace `fname' = substr(`tvar', `poscoml'+1 , strlen(`tvar') )     if (`posbraf'+`posbral'>0) & inrange(`poscoml' , `posbral' , strlen(`tvar'))  
				// some text after the brackets but no comma:  {... {...}...}  _ LNAME
				qui replace `fname' = substr(`tvar', 1 , `posspal'-1  )      			if (`lname'=="") & `posspal'>`posbral'  
				qui replace `lname' = substr(`tvar', `posspal'+1 , strlen(`tvar'))     	if (`lname'=="") & `posspal'>`posbral'      
				// some text before the brackets but no comma :  ... {...}  : take all bracketed content as lname
				qui replace `fname' = substr(`tvar', 1 , `posspaf'-1)      				if (`lname'=="") & `posspal'<`posbral' 					
				qui replace `lname' = substr(`tvar', `posbraf' , strlen(`tvar'))     	if (`lname'=="") & `posspal'<`posbral'       
			qui count if `lname'=="" & `tvar'!="" 
			if (r(N)>0) di as error "Last name not identified (or empty) for `=r(N)' entries of `var'"   
			
			* add up the pieces:
			if ("`style'"=="standard")  	qui replace `generate' = `generate' + "`prefix'" + strtrim(`fname') + cond(strtrim(`lname')!="" , " " + strtrim(`lname') , "") 	if `tvar'!=""
			if ("`style'"=="comma")  		qui replace `generate' = `generate' + "`prefix'" + strtrim(`lname') + cond(strtrim(`fname')!="" , ", " + strtrim(`fname') , "") if `tvar'!=""
			if ("`style'"=="fnameonly")  	qui replace `generate' = `generate' + "`prefix'" + strtrim(`fname')   				if `tvar'!=""
			if ("`style'"=="lnameonly")  	qui replace `generate' = `generate' + "`prefix'" + strtrim(`lname')   				if `tvar'!=""
		}	
		loc prefix " `bind' "
	}
	qui compress `generate'
end


// -------------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------------------
// bibtex journallist [fieldname--(default is 'journal')] , ...
pr def bibtex_journallist , sortpreserve 
    version 13
    syntax [anything(name=field)]  [, wide contentvar(varname) fieldvar(varname) freq cond(string) saving(string asis) replace ]     
// cond must contain if in
	if ("`field'"=="")   loc field journal
	_bibtex_checktype  , `wide' fieldvar(`fieldvar') contentvar(`contentvar')
	if (`"`saving'"' != "") {
		preserve
	}	
	if ("`wide'"=="") {
		tempvar first
		qui egen `first' = tag(`contentvar') if `fieldvar'=="`field'" 
		qui replace `first' = 0 if `contentvar'=="" & `first'==1	
		if ("`freq'"!="") {
			qui by `fieldvar' `contentvar' : gen _freq = _N if `first'
			loc freq _freq
		}			
		list `contentvar' `freq' if `first'==1 , noobs clean
		if (`"`saving'"' != "") {
			qui keep if `first'==1 
			qui keep `contentvar' `freq'
		}	
	}
	else {
		tempvar first
		sort `field' 
		qui egen `first' = tag(`field')  `cond'
		if ("`freq'"!="") {
			qui by `field' :  gen _freq = _N if `first'
			loc freq _freq
		}	
		qui replace `first' = 0 if (`field'=="") & `first'==1	
		list `field' `freq' if `first'==1 , noobs clean noheader 
		if (`"`saving'"' != "") {
			qui keep if `first'==1 		
			qui keep `field' `freq'
		}	
	}
	if (`"`saving'"' != "") {
		save `saving' , `replace'
		restore
	}
end

// -------------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------------------
// bibtex authorlist
pr def bibtex_namelist , sortpreserve 
    version 13
    syntax  anything(name=nametype) [ , from(string)  wide contentvar(varname) fieldvar(varname) freq CONDition(string) saving(string asis) replace ]     
	// cond must contain if in; 
	_bibtex_checktype  , `wide' fieldvar(`fieldvar') contentvar(`contentvar')
	if ("`wide'"=="") {
		tempvar fname lname
		bibtex buildname `nametype' , from(`contentvar') condition(if (substr(`fieldvar',1,7)=="`nametype'") & (`fieldvar'!="`nametype'"))  gen(`fname') style(fnameonly)
		bibtex buildname `nametype' , from(`contentvar') condition(if (substr(`fieldvar',1,7)=="`nametype'") & (`fieldvar'!="`nametype'"))  gen(`lname') style(lnameonly)	
		tempvar first
		qui egen `first' = tag(`fname' `lname') if `condition'
		qui replace `first' = 0 if `lname'=="" & `first'==1	
		sort `lname' `fname'
		if ("`freq'"!="") {
			qui by `lname' `fname' : gen _freq = _N if `first'
			loc freq _freq
		}			
		list `lname' `fname' `freq' if `first'==1 , noobs clean
		if (`"`saving'"' != "") {
			qui keep if `first'==1 
			qui keep `lname' `fname'  `freq'
			rename `lvar' lastname
			rename `fvar' firstname
			save `saving' , `replace'
		}	
	}
	else {
		tempvar first
		tempname fvar lvar
		unab from : `nametype'?*
		loc i 0
		foreach var of varlist `from' {		
			bibtex buildname `nametype' , from(`var') gen(`fvar'`++i') style(fnameonly)
			bibtex buildname `nametype' , from(`var') gen(`lvar'`i') style(lnameonly)			
		}
		preserve
			if ("`condition'"!="") qui keep `condition'
			if _N==0 {
				di as error "No observation satisfy the condition."
				exit  2000
			}	
			keep `fvar'* `lvar'*
			qui gen n = _n
			qui reshape long `fvar' `lvar' , i(n) 
			drop n
			qui keep if `lvar'!=""
			sort `lvar' `fvar'
			qui egen `first' = tag(`fvar' `lvar') 
			if ("`freq'"!="") {
				qui by `lvar' `fvar'  :  gen _freq = _N if `first'
				loc freq _freq
			}	
			list `lvar' `fvar' `freq' if `first'==1 , noobs clean noheader 
			if (`"`saving'"' != "") {
				qui keep if `first'==1
				rename `lvar' lastname
				rename `fvar' firstname
				qui keep lastname firstname `freq'
				save `saving' , `replace'
			}
		restore	
	}
end
	

// -------------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------------------
pr def _bibtex_checktype 
	version 13
	syntax [, wide fieldvar(varname) contentvar(varname) bibid(varname) ]
	if ( ("`wide'"!="") | ("`: char _dta[Format]'"=="wide") ) 	loc wide "wide" 
	if ("`wide'"!="") & ("`fieldvar'`contentvar'"!="") {
		di as error "fieldvar() and contentvar() irrelevant in wide format."
		exit 198
	}	
	if ("`wide'"=="") {
		if ("`fieldvar'"=="")		local fieldvar 	:	char _dta[fieldvar]
		if ("`fieldvar'"=="")		local fieldvar 	field 
		if ("`contentvar'"=="")		local contentvar :	char _dta[contentvar]
		if ("`contentvar'"=="")		local contentvar content 		
		c_local fieldvar `fieldvar'
		c_local contentvar `contentvar'
	}
	c_local wide `wide'
	if ("`bibid'"=="")    	local bibid 	:  char _dta[bibid]  // name of the variable holding the string entry  identifier
	if ("`bibid'"=="")    	local bibid 	bibid
	c_local bibid `bibid'
end
	

	
// -------------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------------------

// The import routine is relatively complex. Complexity due to various idiosyncrasies in the possible input format, such as handling embedded {}, ` ' , " " etc.
// I do not guarantee 99% success in the import, especially if the input bib file is not clean---but seems to work in vast majority of situations!
// skip(string) is a list of bibtex fields which are to be ignored in the import, e.g., skip(abstract url) .  

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
				post `fout' (`i') ("`id'") ("entrytype") (strupper("`type'")) 
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
		if ("`wide'"=="") {
			char define _dta[Format]	"long"
			char define _dta[contentvar] "content"
			char define _dta[fieldvar] "field"	
		} 
		else {
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
			cap destring year , replace			
			order id bibid entrytype
			char define _dta[Format]	"wide"
		}
		char define _dta[Source] `"Original import from [`using'] by -bibtex- on `=c(current_date)'."' 
		char define _dta[bibid] "bibid"
		char define _dta[numid] "id"	
		char define _dta[entrytype] "entrytype"
		compress
		save `"`saving'`bibtexout'"' , replace	
	}
	if (`loaddta'==0)  restore
	
end



exit
	
	
	
