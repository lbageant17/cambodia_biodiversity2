
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

* Set up file to store Wald test results
	cd "$temp/"
	tempname wald
	postfile `wald' str40(Outcome) str40(Predictor) str40(Null) pval str40(note) using wald_test, replace
	
* Set up file to store beta coefficients of interest and SEs for plotting
tempname betas
postfile `betas' str40(Outcome) str40(Predictor) str40(Model) coeff se str40(coeff_var) str40(note)  using betas, replace

/* ----------- System x Catch ------------------------------------------------*/

	use "$processed/hh_level_stata", clear

* These regressions will be calculated with robust SEs and output
	reg catch_species biom_species, vce(cluster cfrid)
	est sto base 
	post `betas' ("catch_species") ("biom_species")  ("base") (_b[biom_species]) (_se[biom_species]) ("biom_species") ("")
	* add effort
	reg catch_species biom_species $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("catch_species") ("biom_species") ("effort") (_b[biom_species]) (_se[biom_species]) ("biom_species")  ("")
	post `betas' ("catch_species") ("biom_species") ("effort") (_b[effort]) (_se[effort]) ("effort") ("")

	* add hh characteristics
	reg catch_species biom_species $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("catch_species") ("biom_species") ("hh") (_b[biom_species]) (_se[biom_species]) ("biom_species")  ("")
	post `betas' ("catch_species") ("biom_species") ("hh") (_b[effort]) (_se[effort]) ("effort")  ("")
	
	* add market access
	reg catch_species biom_species $effort $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("catch_species") ("biom_species") ("mkt") (_b[biom_species]) (_se[biom_species]) ("biom_species") ("")
	post `betas' ("catch_species") ("biom_species") ("mkt") (_b[effort]) (_se[effort]) ("effort")  ("")
		
	* interact market access
	reg catch_species c.biom_species##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 
	
	outreg2 [base effort hh mkt mktx] using "$output/$date/species_system_x_catch", replace excel label ///
		title("Table X: System X Catch (species counts)") addnote("Outcome variable is count of species caught. Robust standard errors clustered at cfrid level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	
	
** Set up Wald test of equality across model coefficients
	
	* These are same regressions as above, but used for suest test so robust SEs are included there and not in main regs.
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
	
	suest base effort hh mkt mktx, vce(cluster cfrid)

	test [base_mean]biom_species = [effort_mean]biom_species 
	post `wald' ("Catch diversity") ("System diversity") ("Base = Effort ") (r(p)) ("")
	
	test [effort_mean]biom_species = [hh_mean]biom_species
	post `wald' ("Catch diversity") ("System diversity") ("Effort = HH ") (r(p)) ("")
	
	test [hh_mean]biom_species = [mkt_mean]biom_species  
	post `wald' ("Catch diversity") ("System diversity") ("HH = Market ") (r(p)) ("")
	
	test [mkt_mean]biom_species = [mktx_mean]biom_species  
	post `wald' ("Catch diversity") ("System diversity") ("Market = Market X ") (r(p)) ("")

	
/* ----------- Catch x Consumption -------------------------------------------*/

	use "$processed/hh_level_stata", clear

* These regressions will be calculated with robust SEs and output
	reg consumption_species catch_species, vce(cluster cfrid)
	est sto base 
	post `betas' ("consumption_species") ("catch_species") ("base") (_b[catch_species]) (_se[catch_species]) ("catch_species") ("")
	* add effort
	reg consumption_species catch_species $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("consumption_species") ("catch_species") ("effort") (_b[catch_species]) (_se[catch_species]) ("catch_species") ("")
	post `betas' ("consumption_species") ("catch_species") ("effort") (_b[effort]) (_se[effort]) ("effort") ("")
	* add hh characteristics
	reg consumption_species catch_species $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("consumption_species") ("catch_species") ("hh") (_b[catch_species]) (_se[catch_species]) ("catch_species") ("")
	post `betas' ("consumption_species") ("catch_species") ("hh") (_b[effort]) (_se[effort]) ("effort") ("")
	* add market access
	reg consumption_species catch_species $effort $hh $mkt, vce(cluster cfrid)
	est sto mkt 
	post `betas' ("consumption_species") ("catch_species") ("mkt") (_b[catch_species]) (_se[catch_species]) ("catch_species") ("")
	post `betas' ("consumption_species") ("catch_species") ("mkt") (_b[effort]) (_se[effort]) ("effort") ("")
	* interact market access
	reg consumption_species c.catch_species##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 
	
	outreg2 [base effort hh mkt mktx] using "$output/$date/species_catch_x_consumption", replace excel label ///
		title("Table X: Catch X Consumption (species counts)") addnote("Outcome variable is count of species consumed. Robust standard errors clustered at cfrid level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")


** Set up Wald test of equality across model coefficients
	
	* These are same regressions as above, but used for suest test so robust SEs are included there and not in main regs.		
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

	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]catch_species = [effort_mean]catch_species  
	post `wald' ("Consumption diversity") ("Catch diversity") ("Base = Effort ") (r(p)) ("")	
	
	test [effort_mean]catch_species = [hh_mean]catch_species  
	post `wald' ("Consumption diversity") ("Catch diversity") ("Effort = HH ") (r(p)) ("")
		
	test [hh_mean]catch_species = [mkt_mean]catch_species  
	post `wald' ("Consumption diversity") ("Catch diversity") ("HH = Market ") (r(p)) ("")
		
	test [mkt_mean]catch_species = [mktx_mean]catch_species  
	post `wald' ("Consumption diversity") ("Catch diversity") ("Market = Market X ") (r(p)) ("")
	
	
/* ----------- Catch x Sold --------------------------------------------------*/

	use "$processed/hh_level_stata", clear

* These regressions will be calculated with robust SEs and output
	reg sold_species catch_species, vce(cluster cfrid)
	est sto base 
	post `betas' ("sold_species") ("catch_species") ("base") (_b[catch_species]) (_se[catch_species]) ("catch_species") ("")
	* add effort
	reg sold_species catch_species $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("sold_species") ("catch_species") ("effort") (_b[catch_species]) (_se[catch_species]) ("catch_species") ("")
	post `betas' ("sold_species") ("catch_species") ("effort") (_b[effort]) (_se[effort]) ("effort") ("")
	* add hh characteristics
	reg sold_species catch_species $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("sold_species") ("catch_species") ("hh") (_b[catch_species]) (_se[catch_species]) ("catch_species") ("")
	post `betas' ("sold_species") ("catch_species") ("hh") (_b[effort]) (_se[effort]) ("effort") ("")
	* add market access
	reg sold_species catch_species $effort $hh $mkt, vce(cluster cfrid)
	est sto mkt 
	post `betas' ("sold_species") ("catch_species") ("mkt") (_b[catch_species]) (_se[catch_species]) ("catch_species") ("")
	post `betas' ("sold_species") ("catch_species") ("mkt") (_b[effort]) (_se[effort]) ("effort") ("")
	* interact market access
	reg sold_species c.catch_species##c.city_distance $effort $hh, vce(cluster cfrid)
	est sto mktx 
	
	outreg2 [base effort hh mkt mktx] using "$output/$date/species_catch_x_sold", replace excel label ///
		title("Table X: Catch X Sold (species counts)") addnote("Outcome variable is count of species sold. Robust standard errors clustered at cfrid level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")

** Set up Wald test of equality across model coefficients
	
	* These are same regressions as above, but used for suest test so robust SEs are included there and not in main regs.		
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
	
	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]catch_species = [effort_mean]catch_species  
	post `wald' ("Sold diversity") ("Catch diversity") ("Base = Effort ") (r(p)) ("")

	test [effort_mean]catch_species = [hh_mean]catch_species  
	post `wald' ("Sold diversity") ("Catch diversity") ("Effort = HH ") (r(p)) ("")

	test [hh_mean]catch_species = [mkt_mean]catch_species  
	post `wald' ("Sold diversity") ("Catch diversity") ("HH = Market ") (r(p)) ("")

	test [mkt_mean]catch_species = [mktx_mean]catch_species 
	post `wald' ("Sold diversity") ("Catch diversity") ("Market = Market X ") (r(p)) ("")

	
	
/* ----------- System x Consumption ------------------------------------------*/

	use "$processed/hh_level_stata", clear
	
* These regressions will be calculated with robust SEs and output
	reg consumption_species biom_species, vce(cluster cfrid)
	est sto base 
	post `betas' ("consumption_species") ("biom_species") ("base") (_b[biom_species]) (_se[biom_species]) ("biom_species") ("")
	* add effort
	reg consumption_species biom_species $effort, vce(cluster cfrid)
	post `betas' ("consumption_species") ("biom_species") ("effort") (_b[biom_species]) (_se[biom_species]) ("biom_species") ("")
	post `betas' ("consumption_species") ("biom_species") ("effort") (_b[effort]) (_se[effort]) ("effort") ("")
	est sto effort
	* add hh characteristics
	reg consumption_species biom_species $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("consumption_species") ("biom_species") ("hh") (_b[biom_species]) (_se[biom_species]) ("biom_species") ("")
	post `betas' ("consumption_species") ("biom_species") ("hh") (_b[effort]) (_se[effort]) ("effort") ("")

	* add market access
	reg consumption_species biom_species $effort $hh $mkt, vce(cluster cfrid)
	est sto mkt 
	post `betas' ("consumption_species") ("biom_species") ("mkt") (_b[biom_species]) (_se[biom_species]) ("biom_species") ("")
	post `betas' ("consumption_species") ("biom_species") ("mkt") (_b[effort]) (_se[effort]) ("effort") ("")
	* interact market access
	reg consumption_species c.biom_species##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 
	
	outreg2 [base hh mkt mktx] using "$output/$date/species_system_x_cons", replace excel label ///
	title("Table X: System X Consumption (species counts)") addnote("Outcome variable is count of species consumed. Robust standard errors clustered at cfrid level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included there and not in main regs.		
	reg consumption_species biom_species
	est sto base 
	* add effort
	reg consumption_species biom_species $effort
	est sto effort
	* add hh characteristics
	reg consumption_species biom_species $effort $hh
	est sto hh 
	* add market access
	reg consumption_species biom_species $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg consumption_species c.biom_species##c.city_distance $effort $hh 
	est sto mktx 

	
	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]biom_species = [effort_mean]biom_species  
	post `wald' ("Consumption diversity") ("System diversity") ("Base = Effort") (r(p)) ("Includes effort control")

	test [effort_mean]biom_species = [hh_mean]biom_species
	post `wald' ("Consumption diversity") ("System diversity") ("Effort = HH") (r(p)) ("Includes effort control")
	
	test [hh_mean]biom_species = [mkt_mean]biom_species  
	post `wald' ("Consumption diversity") ("System diversity") ("HH = Market") (r(p)) ("Includes effort control")

	test [mkt_mean]biom_species = [mktx_mean]biom_species 
	post `wald' ("Consumption diversity") ("System diversity") ("Market = Market X") (r(p)) ("Includes effort control")




/* ----------- System x Sold -------------------------------------------------*/

	use "$processed/hh_level_stata", clear

* These regressions will be calculated with robust SEs and output
	reg sold_species biom_species, vce(cluster cfrid)
	est sto base 
	post `betas' ("sold_species") ("biom_species") ("base") (_b[biom_species]) (_se[biom_species]) ("biom_species") ("")
	*add effort
	reg sold_species biom_species $effort
	est sto effort
	post `betas' ("sold_species") ("biom_species") ("effort") (_b[biom_species]) (_se[biom_species]) ("biom_species") ("")
	post `betas' ("sold_species") ("biom_species") ("effort") (_b[effort]) (_se[effort]) ("effort") ("")
	* add hh characteristics
	reg sold_species biom_species $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("sold_species") ("biom_species") ("hh") (_b[biom_species]) (_se[biom_species]) ("biom_species") ("")
	post `betas' ("sold_species") ("biom_species") ("hh") (_b[effort]) (_se[effort]) ("effort") ("")
	* add market access
	reg sold_species biom_species $effort $hh $mkt, vce(cluster cfrid)
	est sto mkt 
	post `betas' ("sold_species") ("biom_species") ("mkt") (_b[biom_species]) (_se[biom_species]) ("biom_species") ("")
	post `betas' ("sold_species") ("biom_species") ("mkt") (_b[effort]) (_se[effort]) ("effort") ("")
	* interact market access
	reg sold_species c.biom_species##c.city_distance $effort $hh, vce(cluster cfrid)
	est sto mktx 
	
	outreg2 [base hh mkt mktx] using "$output/$date/species_system_x_sold", replace excel label ///
	title("Table X: System X Sold (species counts)") addnote("Outcome variable is count of species sold. Robust standard errors clustered at cfrid level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included there and not in main regs.			
	reg sold_species biom_species
	est sto base 
	* add effort
	reg sold_species biom_species $effort
	est sto effort
	* add hh characteristics
	reg sold_species biom_species $effort $hh
	est sto hh 
	* add market access
	reg sold_species biom_species $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg sold_species c.biom_species##c.city_distance $effort $hh
	est sto mktx 

	
	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]biom_species = [effort_mean]biom_species  
	post `wald' ("Sold diversity") ("System diversity") ("Base = Effort") (r(p)) ("Includes effort control")
	
	test [effort_mean]biom_species = [hh_mean]biom_species
	post `wald' ("Sold diversity") ("System diversity") ("HH = Effort") (r(p)) ("Includes effort control")

	test [hh_mean]biom_species = [mkt_mean]biom_species  
	post `wald' ("Sold diversity") ("System diversity") ("HH = Market") (r(p)) ("Includes effort control") 

	test [mkt_mean]biom_species = [mktx_mean]biom_species 
	post `wald' ("Sold diversity") ("System diversity") ("Market = Market X") (r(p)) ("Includes effort control")


/* ----------- System x Consumption -- NO EFFORT------------------------------------------*/

* We do not control for effort in these models. https://ftp.cs.ucla.edu/pub/stat_ser/r493.pdf

	use "$processed/hh_level_stata", clear
	
* These regressions will be calculated with robust SEs and output
	reg consumption_species biom_species, vce(cluster cfrid)
	est sto base 
	post `betas' ("consumption_species") ("biom_species") ("base") (_b[biom_species]) (_se[biom_species]) ("biom_species") ("no_effort")
	/* add effort
	reg consumption_species biom_species $effort, vce(cluster cfrid)
	est sto effort*/
	* add hh characteristics
	reg consumption_species biom_species /*$effort*/ $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("consumption_species") ("biom_species") ("hh") (_b[biom_species]) (_se[biom_species]) ("biom_species") ("no_effort")
	* add market access
	reg consumption_species biom_species /*$effort*/ $hh $mkt, vce(cluster cfrid)
	est sto mkt 
	post `betas' ("consumption_species") ("biom_species") ("mkt") (_b[biom_species]) (_se[biom_species]) ("biom_species") ("no_effort")
	* interact market access
	reg consumption_species c.biom_species##c.city_distance /*$effort*/ $hh, vce(cluster cfrid) 
	est sto mktx 
	
	outreg2 [base hh mkt mktx] using "$output/$date/species_system_x_cons_NE", replace excel label ///
	title("Table X: System X Consumption (species counts)") addnote("Outcome variable is count of species consumed. Robust standard errors clustered at cfrid level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included there and not in main regs.		
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

	
	suest base hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]biom_species = [hh_mean]biom_species  
	post `wald' ("Consumption diversity") ("System diversity") ("Base = HH ") (r(p)) ("No effort control")	

	test [hh_mean]biom_species = [mkt_mean]biom_species  
	post `wald' ("Consumption diversity") ("System diversity") ("HH = Market ") (r(p)) ("No effort control")	 

	test [mkt_mean]biom_species = [mktx_mean]biom_species 
	post `wald' ("Consumption diversity") ("System diversity") ("Market = Market X ") (r(p)) ("No effort control")	




/* ----------- System x Sold -- NO EFFORT-----------------------------------------------*/

* We do not control for effort in these models. https://ftp.cs.ucla.edu/pub/stat_ser/r493.pdf

	use "$processed/hh_level_stata", clear

* These regressions will be calculated with robust SEs and output
	reg sold_species biom_species, vce(cluster cfrid)
	est sto base 
	post `betas' ("sold_species") ("biom_species") ("base") (_b[biom_species]) (_se[biom_species]) ("biom_species") ("no_effort")
	/* add effort
	reg sold_species biom_species $effort
	est sto effort*/
	* add hh characteristics
	reg sold_species biom_species /*$effort*/ $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("sold_species") ("biom_species") ("hh") (_b[biom_species]) (_se[biom_species]) ("biom_species") ("no_effort")
	* add market access
	reg sold_species biom_species /*$effort*/ $hh $mkt, vce(cluster cfrid)
	est sto mkt 
	post `betas' ("sold_species") ("biom_species") ("mkt") (_b[biom_species]) (_se[biom_species]) ("biom_species") ("no_effort")
	* interact market access
	reg sold_species c.biom_species##c.city_distance /*$effort*/ $hh, vce(cluster cfrid)
	est sto mktx 
	
	outreg2 [base hh mkt mktx] using "$output/$date/species_system_x_sold_NE", replace excel label ///
	title("Table X: System X Sold (species counts)") addnote("Outcome variable is count of species sold. Robust standard errors clustered at cfrid level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included there and not in main regs.			
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

	
	suest base hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]biom_species = [hh_mean]biom_species  
	post `wald' ("Sold diversity") ("System diversity") ("Base = HH ") (r(p)) ("No effort control")		

	test [hh_mean]biom_species = [mkt_mean]biom_species  
	post `wald' ("Sold diversity") ("System diversity") ("HH = Market ") (r(p)) ("No effort control")	

	test [mkt_mean]biom_species = [mktx_mean]biom_species 
	post `wald' ("Sold diversity") ("System diversity") ("Market = Market X ") (r(p)) ("No effort control")	

	
	
postclose `wald'
use "$temp/wald_test", clear
export excel using "$output/$date/wald_tests.xlsx", sheet("Species count models") sheetmodify firstrow(variables)

postclose `betas'
use "$temp/betas", clear
* calculate min and max
gen min = coeff-2*se
gen max = coeff+2*se
export excel using "$data/for_plotting/betas.xlsx", sheet("Species count models") sheetmodify firstrow(variables)
export excel using "$output/$date/betas.xlsx", sheet("Species count models") sheetmodify firstrow(variables)



/*----------------------------------------------------------------------------*/
/* ----------- Shannon index ------------------------------------------------*/
/*----------------------------------------------------------------------------*/
