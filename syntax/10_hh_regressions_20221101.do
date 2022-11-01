
** Liz Bageant
** November 1, 2022


clear all
set more off
version 15

/*----------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
	Household characteristics and effort. Do they attenuate biodiversity filtering?
------------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/


* controls
global effort effort
global hh hhsize depshare maxeduc index1 index2
global mkt city_distance

/*----------------------------------------------------------------------------*/
/* ----------- Species counts ------------------------------------------------*/
/*----------------------------------------------------------------------------*/


/* ----------- System x Catch ------------------------------------------------*/

	use "$processed/hh_level_stata", clear

	reg catch_species biom_species
	est sto base 
	* add effort
	reg catch_species biom_species $effort
	est sto effort
	* add hh characteristics
	reg catch_species biom_species $effort $hh
	est sto hh 
	* add market access
	reg catch_species biom_species $effort $hh $mkt 
	est sto mkt 
	* interact market access
	reg catch_species c.biom_species##c.city_distance $effort $hh 
	est sto mktx 
	
	outreg2 [base effort hh mkt mktx] using "$output/$date/species_system_x_catch", replace excel label ///
		title("Table X: System X Catch (species counts)") addnote("Outcome variable is count of species caught. Robust standard errors clustered at cfrid level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")


	suest base effort hh  mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from base model
	
	cd "$temp/"
	tempname reg1
	postfile `reg1' str40(Outcome) str40(Predictor) str40(Null) pval using wald_test, replace


	test [base_mean]biom_species = [effort_mean]biom_species 
	post `reg1' ("Catch diversity") ("System diversity") ("Base = Effort ") (r(p)) 
	
	test [effort_mean]biom_species = [hh_mean]biom_species
	post `reg1' ("Catch diversity") ("System diversity") ("Effort = HH ") (r(p)) 
	
	test [hh_mean]biom_species = [mkt_mean]biom_species  
	post `reg1' ("Catch diversity") ("System diversity") ("HH = Market ") (r(p)) 
	
	test [mkt_mean]biom_species = [mktx_mean]biom_species  
	post `reg1' ("Catch diversity") ("System diversity") ("Market = Market X ") (r(p)) 

	
/* ----------- Catch x Consumption -------------------------------------------*/

	use "$processed/hh_level_stata", clear

	reg consumption_species catch_species 
	est sto base 
	* add effort
	reg consumption_species catch_species $effort
	est sto effort
	* add hh characteristics
	reg consumption_species catch_species $effort $hh
	est sto hh 
	* add market access
	reg consumption_species catch_species $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg consumption_species c.catch_species##c.city_distance $effort $hh 
	est sto mktx 
	
	outreg2 [base effort hh mkt mktx] using "$output/$date/species_catch_x_consumption", replace excel label ///
		title("Table X: Catch X Consumption (species counts)") addnote("Outcome variable is count of species consumed. Robust standard errors clustered at cfrid level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")



	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]catch_species = [effort_mean]catch_species  
	post `reg1' ("Consumption diversity") ("Catch diversity") ("Base = Effort ") (r(p)) 	
	
	test [effort_mean]catch_species = [hh_mean]catch_species  
	post `reg1' ("Consumption diversity") ("Catch diversity") ("Effort = HH ") (r(p)) 
		
	test [hh_mean]catch_species = [mkt_mean]catch_species  
	post `reg1' ("Consumption diversity") ("Catch diversity") ("HH = Market ") (r(p)) 
		
	test [mkt_mean]catch_species = [mktx_mean]catch_species  
	post `reg1' ("Consumption diversity") ("Catch diversity") ("Market = Market X ") (r(p)) 
	
	
/* ----------- Catch x Sold --------------------------------------------------*/

	use "$processed/hh_level_stata", clear

	reg sold_species catch_species
	est sto base 
	* add effort
	reg sold_species catch_species $effort
	est sto effort
	* add hh characteristics
	reg sold_species catch_species $effort $hh
	est sto hh 
	* add market access
	reg sold_species catch_species $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg sold_species c.catch_species##c.city_distance $effort $hh 
	est sto mktx 
	
	outreg2 [base effort hh mkt mktx] using "$output/$date/species_catch_x_sold", replace excel label ///
		title("Table X: Catch X Sold (species counts)") addnote("Outcome variable is count of species sold. Robust standard errors clustered at cfrid level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")


	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]catch_species = [effort_mean]catch_species  
	post `reg1' ("Sold diversity") ("Catch diversity") ("Base = Effort ") (r(p)) 	

	test [effort_mean]catch_species = [hh_mean]catch_species  
	post `reg1' ("Sold diversity") ("Catch diversity") ("Effort = HH ") (r(p)) 

	test [hh_mean]catch_species = [mkt_mean]catch_species  
	post `reg1' ("Sold diversity") ("Catch diversity") ("HH = Market ") (r(p)) 

	test [mkt_mean]catch_species = [mktx_mean]catch_species 
	post `reg1' ("Sold diversity") ("Catch diversity") ("Market = Market X ") (r(p)) 


/* ----------- System x Consumption ------------------------------------------*/

	use "$processed/hh_level_stata", clear

	reg consumption_species biom_species
	est sto base 
	/* add effort
	reg consumption_species biom_species $effort
	est sto effort*/
	* add hh characteristics
	reg consumption_species biom_species /*$effort*/ $hh
	est sto hh 
	* add market access
	reg consumption_species biom_species /*$effort*/ $hh $mkt
	est sto mkt 
	* interact market access
	reg consumption_species c.biom_species##c.city_distance /*$effort*/ $hh 
	est sto mktx 
	
	outreg2 [base hh mkt mktx] using "$output/$date/species_system_x_cons", replace excel label ///
	title("Table X: System X Consumption (species counts)") addnote("Outcome variable is count of species consumed. Robust standard errors clustered at cfrid level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")

	
	
	suest base hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]biom_species = [hh_mean]biom_species  
	post `reg1' ("Consumption diversity") ("System diversity") ("Base = HH ") (r(p)) 	

	test [hh_mean]biom_species = [mkt_mean]biom_species  
	post `reg1' ("Consumption diversity") ("System diversity") ("HH = Market ") (r(p)) 

	test [mkt_mean]biom_species = [mktx_mean]biom_species 
	post `reg1' ("Consumption diversity") ("System diversity") ("Market = Market X ") (r(p)) 




/* ----------- System x Sold -------------------------------------------------*/

	use "$processed/hh_level_stata", clear

	reg sold_species biom_species
	est sto base 
	/* add effort
	reg sold_species biom_species $effort
	est sto effort*/
	* add hh characteristics
	reg sold_species biom_species /*$effort*/ $hh
	est sto hh 
	* add market access
	reg sold_species biom_species /*$effort*/ $hh $mkt
	est sto mkt 
	* interact market access
	reg sold_species c.biom_species##c.city_distance /*$effort*/ $hh
	est sto mktx 
	
	outreg2 [base hh mkt mktx] using "$output/$date/species_system_x_sold", replace excel label ///
	title("Table X: System X Sold (species counts)") addnote("Outcome variable is count of species sold. Robust standard errors clustered at cfrid level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")

	
	suest base hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]biom_species = [hh_mean]biom_species  
	post `reg1' ("Sold diversity") ("System diversity") ("Base = HH ") (r(p)) 	

	test [hh_mean]biom_species = [mkt_mean]biom_species  
	post `reg1' ("Sold diversity") ("System diversity") ("HH = Market ") (r(p)) 

	test [mkt_mean]biom_species = [mktx_mean]biom_species 
	post `reg1' ("Sold diversity") ("System diversity") ("Market = Market X ") (r(p)) 

	
	
postclose `reg1'
use "$temp/wald_test", clear
export excel using "$output/$date/wald_tests.xlsx", sheet("Species count models") sheetmodify firstrow(variables)


/*----------------------------------------------------------------------------*/
/* ----------- Shannon index ------------------------------------------------*/
/*----------------------------------------------------------------------------*/
