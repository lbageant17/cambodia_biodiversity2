** Liz Bageant
** August 3, 2022

** Master dofile for Cambodia Biodiversity paper

* Macros
global stata_raw "/Users/erb32/github/cambodia_biodiversity/data/stata_raw"
global excel_raw "/Users/erb32/github/cambodia_biodiversity/data/excel_raw"
global do "/Users/erb32/github/cambodia_biodiversity/syntax"
global temp "/Users/erb32/Desktop/temp"
global processed "/Users/erb32/github/cambodia_biodiversity/data/processed"
global traits "/Users/erb32/github/cambodia_biodiversity/data/traits/"
global documentation "/Users/erb32/github/cambodia_biodiversity/documentation/"
 
*global processed

/*
** Converting all excel files to stata files
clear all
file open _filenames using "$excel_raw/_filenames",read
file read _filenames line
while r(eof)==0 {
	import excel "$excel_raw/`line'.xlsx",  firstrow case(lower) allstring clear
	saveold "$stata_raw/`line'",replace
	file read _filenames line
	}
*/
	
* Do-files

do "$do/1_ccm_q7.do" 					// clean/organize catch data
	// this file calls "0_cfrid_string_to_num.do"
	// this file calls "0_code_fix.do"
do "$do/2_biom_formb.do" 				// clean/organize biomonitoring data
	// this file calls "0_cfrid_string_to_num.do"
	// this file calls "0_code_fix.do" 
do "$do/3_traits.do" 		
do "$do/4_species_reconcile.do" // generate master list of species codes
