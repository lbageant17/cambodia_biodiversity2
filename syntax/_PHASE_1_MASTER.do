** Liz Bageant
** November 9, 2022

/*----------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------

 Phase 1 master dofile for Cambodia Biodiversity paper
 
 Use for Phase 1 data prep.
 
 This file must be run prior to Phase 2-4 analysis.

------------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

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
global data "$umbrella/data/"

* date macro for date-specific output
global date "20221109"  // <----UPDATE THIS DATE IN _PHASE_2_MASTER.do

cap mkdir "$umbrella/output/$date"
cap mkdir "$umbrella/output/$date/figures"
cap mkdir "$umbrella/output/$date/figures/secondary"
cap mkdir "$umbrella/output/$date/figures/beta_compare"
cap mkdir "$umbrella/output/$date/tables"
cap mkdir "$umbrella/output/$date/tables/hh_regs_effort"
cap mkdir "$umbrella/output/$date/tables/hh_regs_no_effort"
cap mkdir "$umbrella/output/$date/tables/traits_regs_effort"
cap mkdir "$umbrella/output/$date/tables/traits_regs_no_effort"


* Do-files

do "$do/1_ccm_q7.do" 					// clean/organize catch data
	// this file calls "0_cfrid_string_to_num.do"
	// this file calls "0_code_fix.do"
do "$do/2_biom_formb.do" 				// clean/organize biomonitoring data
	// this file calls "0_cfrid_string_to_num.do"
	// this file calls "0_code_fix.do" 
do "$do/3_traits.do" 		
do "$do/4_species_reconcile.do" // Generate master list of species codes
do "$do/5_livelihood_survey.do" // Generate household characteristics files
do "$do/6_cfr_characteristics.do" // CFR characteristics (distance to city, cfr type)
do "$do/7_asset_index.do" 	// Generate asset indices
