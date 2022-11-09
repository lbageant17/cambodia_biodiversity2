** Liz Bageant
** November 8, 2022

** 
/*----------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
 
 SECOND master dofile for Cambodia Biodiversity paper.
 
 Use for Phase 3 household regression analyses
 
 Dofiles called from this file use data files generated in R, so Phase 1 (stata)
 and Phase 2 (R) analyses must be run first.

------------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

* Macros: all in _MASTER1.do

	
* Do-files

do "$do/8_combine_20221108.do"		// combine all household-level files into a single data file (including some generated in R)
do "$do/9_sumstats.do"		// Generate summary statistics table
do "$do/10_hh_regressions_effort_20221109.do" 		// test whether HH characteristics attenuate core diversity relationships (with effort controls)
do "$do/10_hh_regressions_noeffort_20221109" 		// test whether HH characteristics attenuate core diversity relationships (without effort controls)
do "$do/11_trait_regressions_effort_20221109.do"  	// test whether HH characteristics attenuate trait-based diversity relationships (with effort controls)
do "$do/11_trait_regressions_noeffort_20221109.do"	// test whether HH characteristics attenuate trait-based diversity relationships (without effort controls) 
