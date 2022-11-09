** Liz Bageant
** November 8, 2022


clear all
set more off
version 15

/*----------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
	Do household characteristics attenuate the trait-based diversity relationships? 
	
------------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

* controls
global effort effort
global hh hhsize depshare maxeduc index1 index2
global mkt city_distance

* Set up file to store Wald test results
	cd "$temp/"
	tempname wald
	postfile `wald' str40(Outcome) str40(Predictor) str40(Null) pval str40(note) using wald_test_traits, replace
	
* Set up file to store beta coefficients of interest and SEs for plotting
	tempname betas
	postfile `betas' str40(Outcome) str40(Predictor) str40(Model) coeff se str40(coeff_var) str40(note)  using betas_traits, replace


/*----------------------------------------------------------------------------*/
/* ----------- Nutrient density ----------------------------------------------*/
/*----------------------------------------------------------------------------*/


/* ----------- System x Catch ------------------------------------------------*/

	use "$processed/hh_level_stata", clear

* These regressions will be calculated with robust SEs and output
	reg nd_score_catch nd_score_cfr, vce(cluster cfrid)
	est sto base 
	post `betas' ("nd_score_catch") ("nd_score_cfr")  ("base") (_b[nd_score_cfr]) (_se[nd_score_cfr]) ("nd_score_cfr") ("")
	* add effort
	reg nd_score_catch nd_score_cfr $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("nd_score_catch") ("nd_score_cfr") ("effort") (_b[nd_score_cfr]) (_se[nd_score_cfr]) ("nd_score_cfr")  ("")

	* add hh characteristics
	reg nd_score_catch nd_score_cfr $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("nd_score_catch") ("nd_score_cfr") ("hh") (_b[nd_score_cfr]) (_se[nd_score_cfr]) ("nd_score_cfr")  ("")
	
	* add market access
	reg nd_score_catch nd_score_cfr $effort $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("nd_score_catch") ("nd_score_cfr") ("mkt") (_b[nd_score_cfr]) (_se[nd_score_cfr]) ("nd_score_cfr") ("")
		
	* interact market access
	reg nd_score_catch c.nd_score_cfr##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 
	
	outreg2 [base effort hh mkt mktx] using "$output/$date/tables/hh_regs_effort/species_system_x_catch", replace excel label ///
		title("Table X: System X Catch (species counts)") addnote("Outcome variable is count of species caught. Robust standard errors clustered at cfrid level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")

		
		postclose `wald'
use "$temp/wald_test_traits", clear
*export excel using "$output/$date/tables/trait_regs_effort/wald_tests_traits.xlsx", sheet("Species count models") sheetmodify firstrow(variables)

postclose `betas'
use "$temp/betas_traits", clear
* calculate min and max
gen min = coeff-2*se
gen max = coeff+2*se
*export excel using "$data/for_plotting/betas.xlsx", sheet("Species count models") sheetmodify firstrow(variables)
*export excel using "$output/$date/tables/trait_regs_effort/betas_traits.xlsx", sheet("Species count models") sheetmodify firstrow(variables)
