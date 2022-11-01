
** Liz Bageant
** October 31, 2022


clear all
set more off
version 15


/*------------------------------------------------------------------------------
	Household characteristics and effort. Do they attenuate biodiversity filtering?
------------------------------------------------------------------------------*/

* controls
global effort effort
global hh hhsize depshare maxeduc index1 index2
global mkt city_distance

/* ----------- Species counts ------------------------------------------------*/

	* System x catch
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
	reg catch_species c.biom_species##c.city_distance $effort $hh $mkt 
	est sto mktx 
	
	outreg2 [base effort hh mkt mktx] using "$output/$date/species_system_x_catch", replace excel label ///
		title("Table X: System X Catch (species counts)") addnote("Outcome variable is count of species at CFR level. Robust standard errors clustered at cfrid level in parenthese. *** p<0.01, ** p<0.05, * p<0.1")


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
	

	* Catch x consumption
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
	reg consumption_species c.catch_species##c.city_distance $effort $hh $mkt $mktx
	est sto mktx 
	
	outreg2 [base effort hh mkt mktx] using "$output/$date/species_catch_x_consumption", replace excel label ///
		title("Table X: Catch X Consumption (species counts)")


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
	
	
	* Catch x sold
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
	reg sold_species c.catch_species##c.city_distance $effort $hh $mkt 
	est sto mktx 
	
	outreg2 [base hh mkt mktx] using "$output/$date/species_catch_x_sold", replace excel label ///
		title("Table X: Catch X Sold (species counts)")

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

	
postclose `reg1'
use "$temp/wald_test", clear
	export excel using "$output/$date/wald_tests.xlsx", sheet("Species count models") sheetmodify firstrow(variables)

	
	
/*

/* ----------- Shannon index ------------------------------------------------*/

* System x catch
	use "$processed/hh_level_stata", clear

	reg h_catch h_biom
	est sto base 
	* add hh characteristics
	reg h_catch h_biom  hhsize maxeduc index1 index2
	est sto hh 
	* add market access
	reg h_catch h_biom  city_distance hhsize maxeduc index1 index2
	est sto mkt 
	* interact market access
	reg h_catch  c.h_biom##c.city_distance hhsize maxeduc index1 index2
	est sto mktx 

	outreg2 [base hh mkt mktx] using "$output/$date/shannon_system_x_catch", replace excel label ///
		title("Table X: System X Catch (Shannon index)") addnote("Outcome variable is shannon index at CFR level. Robust standard errors clustered at cfrid level in parenthese. *** p<0.01, ** p<0.05, * p<0.1")


	suest base hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]h_biom = [effort_mean]h_biom  
	test [effort_mean]h_biom = [hh_mean]h_biom  
	test [hh_mean]h_biom = [mkt_mean]h_biom  
	test [mkt_mean]h_biom = [mktx_mean]h_biom  	

	
* Catch x consumption
	use "$processed/hh_level_stata", clear

	reg h_cons h_catch
	est sto base 
	* add hh characteristics
	reg h_cons h_catch hhsize maxeduc index1 index2
	est sto hh 
	* add market access
	reg h_cons h_catch city_distance hhsize maxeduc index1 index2
	est sto mkt 
	* interact market access
	reg h_cons  c.h_catch##c.city_distance hhsize maxeduc index1 index2
	est sto mktx 

	outreg2 [base hh mkt mktx] using "$output/$date/shannon_catch_x_cons", replace excel label ///
		title("Table X: Catch X Consumption (Shannon index)")

	suest base hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]h_catch = [efort_mean]h_catch  
	test [effort_mean]h_catch = [hh_mean]h_catch  
	test [hh_mean]h_catch = [mkt_mean]h_catch  
	test [mkt_mean]h_catch = [mktx_mean]h_catch	
	
* Catch x sold
	use "$processed/hh_level_stata", clear

	reg h_sold h_catch  
	est sto base 
	* add hh characteristics
	reg h_sold h_catch  hhsize maxeduc index1 index2
	est sto hh 
	* add market access
	reg h_sold h_catch  city_distance hhsize maxeduc index1 index2
	est sto mkt 
	* interact market access
	reg h_sold  c.h_catch##c.city_distance hhsize maxeduc index1 index2 
	est sto mktx 

	outreg2 [base hh mkt mktx] using "$output/$date/shannon_catch_x_sold", replace excel label ///
		title("Table X: Catch X Sold (Shannon index)")
	
	suest base hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]h_catch = [effort_mean]h_catch  
	test [effort_mean]h_catch = [hh_mean]h_catch  
	test [hh_mean]h_catch = [mkt_mean]h_catch  
	test [mkt_mean]h_catch = [mktx_mean]h_catch	

	
	
	
	
	
	
* Experiment with marginsplots for demonstration. could be expanded.
	
reg catch_species c.biom_species##c.city_distance hhsize maxeduc index1 index2
	
margins, dydx(c.biom_species) at (c.city_distance = (5(10)85))
marginsplot, title("Average marginal effects of system diversity on" "catch diversity (species counts) at different levels of 'market access'") 


