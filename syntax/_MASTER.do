** Liz Bageant
** August 30, 2022

** Master dofile for Cambodia Biodiversity paper

* Macros
global umbrella "/Users/erb32/github/cambodia_biodiversity2"
global stata_raw "$umbrella/data/stata_raw"
global excel_raw "$umbrella/data/excel_raw"
global do "$umbrella/syntax"
global temp "/Users/erb32/Desktop/temp"
global processed "$umbrella/data/processed"
global traits "$umbrella/data/traits/"
global documentation "$umbrella/documentation/"
global brochure "$umbrella/data/brochure/"
global component "$umbrella/data/component/"
global output "$umbrella/output/"

* date macro for date-specific output
global date "20221027"
cap mkdir "$umbrella/output/$date"

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
do "$do/5_livelihood_survey.do" // household characteristics files
do "$do/6_cfr_characteristics.do" // CFR characteristics (distance to city, cfr type)
