** Liz Bageant
** November 8, 2022

/*----------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
 
 Phase 3 master dofile for Cambodia Biodiversity paper.
 
 Use for Phase 3 regression analyses
 
 Dofiles called from this file use data files generated in R, so Phase 1 (stata)
 and Phase 2 (R) analyses must be run first.

------------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

global umbrella "/Users/erb32/github/cambodia_biodiversity2"
global do "$umbrella/syntax"
do "$do/0_initialize_macros.do" 		// This file initializes file path and date macros.
	
* Do-files

do "$do/15_combine_20221108.do"		// combine all household-level files into a single data file (including some generated in R)
do "$do/16_sumstats.do"		// Generate summary statistics table
do "$do/17a_hh_regressions_effort_20221109.do" 		// test whether HH characteristics attenuate core diversity relationships (with effort controls)
do "$do/17b_hh_regressions_noeffort_20221109" 		// test whether HH characteristics attenuate core diversity relationships (without effort controls)
do "$do/18a_trait_regressions_effort_20221109.do"  	// test whether HH characteristics attenuate trait-based diversity relationships (with effort controls)
do "$do/18b_trait_regressions_noeffort_20221109.do"	// test whether HH characteristics attenuate trait-based diversity relationships (without effort controls) 
do "$do/19a_index_regressions_effort_20221116.do"	// test whether HH characteristics attenuate index-based diversity relationships (with effort controls) 
