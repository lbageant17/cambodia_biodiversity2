** Liz Bageant
** November 9, 2022


clear all
set more off
version 15


/*----------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
Do household characteristics attenuate the index-based diversity relationships?

These models INCLUDE effort controls.

Note on the strcuture of this dofile: 
The regressions and wald test processes repeat for each index and level of 
analysis, so local macros are used at the beginning of each section changing 
the key parameters (e.g. outcome variables, predictor variables).

------------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

* controls
global effort effort effort2
global hh hhsize depshare maxeduc index1 index2
global mkt city_distance

* Set up file to store Wald test results
	tempname wald
	postfile `wald' str40(Outcome) str40(Predictor) str40(Null) pval str40(note) using "$temp/wald_test_index", replace
	
* Set up file to store beta coefficients of interest and SEs for plotting
	tempname betas
	postfile `betas' str40(Outcome) str40(Predictor) str40(Model) coeff se str40(coeff_var) str40(note)  using "$temp/betas_index", replace

* add note to all sold tables that missing values of sold variables are treated as zero
local sold_caveat "In these models `metric'_sold = 0 for households that sold no fish."


/*----------------------------------------------------------------------------*/
/* ----------- Shannon index -------------------------------------------------*/
/*----------------------------------------------------------------------------*/
local metric shannon  

* add note to all sold tables that missing values of sold variables are treated as zero
local sold_caveat "In these models `metric'_sold = 0 for households that sold no fish."


/* ----------- System x Catch ------------------------------------------------*/

use "$processed/hh_level_stata", clear

	local outcome h_catch
	local pred h_cfr
	local outcome_level CFR
	local tablename "System_X_Catch"
	
* These regressions will be calculated with robust SEs and output
	reg `outcome' `pred', vce(cluster cfrid)
	est sto base 
	post `betas' ("`outcome'") ("`pred'")  ("base") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
	* add effort
	reg `outcome' `pred', vce(cluster cfrid)
	est sto effort
	post `betas' ("`outcome'") ("`pred'") ("effort") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")

	* add hh characteristics
	reg `outcome' `pred' $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("`outcome'") ("`pred'") ("hh") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")
	
	* add market access
	reg `outcome' `pred' $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("`outcome'") ("`pred'") ("mkt") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
		
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 

	outreg2 [base effort hh mkt mktx] using "$output/$date/tables/effort/index_regs_effort/`metric'_`tablename'", replace excel label ///
		title("Table X: `tablename' (`metric')") addnote("Outcome variable is `metric' at the `outcome_level' level. Robust standard errors clustered at CFR level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included in suest command and not in main regs.			
	reg `outcome' `pred'
	est sto base 
	* add effort
	reg `outcome' `pred' $effort
	est sto effort
	* add hh characteristics
	reg `outcome' `pred' $effort $hh
	est sto hh 
	* add market access
	reg `outcome' `pred' $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh
	est sto mktx 

	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]`pred' = [effort_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("Base = Effort") (r(p)) ("")
	
	test [effort_mean]`pred' = [hh_mean]`pred'
	post `wald' ("`outcome'") ("`pred'") ("HH = Effort") (r(p)) ("")

	test [hh_mean]`pred' = [mkt_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("HH = Market") (r(p)) ("") 

	*test [mkt_mean]`pred' = [mktx_mean]`pred' 
	*post `wald' ("`outcome'") ("`pred'") ("Market = Market X") (r(p)) ("")	
	

/* ----------- Catch x Consumption -------------------------------------------*/

	use "$processed/hh_level_stata", clear
	
	local outcome h_cons
	local pred h_catch
	local outcome_level catch
	local tablename "Catch_X_Consumption"
	
* These regressions will be calculated with robust SEs and output
	reg `outcome' `pred', vce(cluster cfrid)
	est sto base 
	post `betas' ("`outcome'") ("`pred'")  ("base") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
	* add effort
	reg `outcome' `pred' $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("`outcome'") ("`pred'") ("effort") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")

	* add hh characteristics
	reg `outcome' `pred' $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("`outcome'") ("`pred'") ("hh") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")
	
	* add market access
	reg `outcome' `pred' $effort $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("`outcome'") ("`pred'") ("mkt") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
		
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 

	outreg2 [base effort hh mkt mktx] using "$output/$date/tables/effort/index_regs_effort/`metric'_`tablename'", replace excel label ///
		title("Table X: `tablename' (`metric')") addnote("Outcome variable is `metric' at the `outcome_level' level. Robust standard errors clustered at CFR level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included in suest command and not in main regs.			
	reg `outcome' `pred'
	est sto base 
	* add effort
	reg `outcome' `pred' $effort
	est sto effort
	* add hh characteristics
	reg `outcome' `pred' $effort $hh
	est sto hh 
	* add market access
	reg `outcome' `pred' $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh
	est sto mktx 

	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]`pred' = [effort_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("Base = Effort") (r(p)) ("")
	
	test [effort_mean]`pred' = [hh_mean]`pred'
	post `wald' ("`outcome'") ("`pred'") ("HH = Effort") (r(p)) ("")

	test [hh_mean]`pred' = [mkt_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("HH = Market") (r(p)) ("") 

	*test [mkt_mean]`pred' = [mktx_mean]`pred' 
	*post `wald' ("`outcome'") ("`pred'") ("Market = Market X") (r(p)) ("")	
	

/* ----------- Catch x Sold --------------------------------------------------*/

	use "$processed/hh_level_stata", clear
	
	local outcome h_sold
	local pred h_catch
	local outcome_level catch
	local tablename "Catch_X_Sold"
	
* These regressions will be calculated with robust SEs and output
	reg `outcome' `pred', vce(cluster cfrid)
	est sto base 
	post `betas' ("`outcome'") ("`pred'")  ("base") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
	* add effort
	reg `outcome' `pred' $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("`outcome'") ("`pred'") ("effort") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")

	* add hh characteristics
	reg `outcome' `pred' $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("`outcome'") ("`pred'") ("hh") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")
	
	* add market access
	reg `outcome' `pred' $effort $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("`outcome'") ("`pred'") ("mkt") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
		
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 

	outreg2 [base effort hh mkt mktx] using "$output/$date/tables/effort/index_regs_effort/`metric'_`tablename'", replace excel label ///
		title("Table X: `tablename' (`metric')") addnote("Outcome variable is `metric' at the `outcome_level' level. Robust standard errors clustered at CFR level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included in suest command and not in main regs.			
	reg `outcome' `pred'
	est sto base 
	* add effort
	reg `outcome' `pred' $effort
	est sto effort
	* add hh characteristics
	reg `outcome' `pred' $effort $hh
	est sto hh 
	* add market access
	reg `outcome' `pred' $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh
	est sto mktx 

	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]`pred' = [effort_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("Base = Effort") (r(p)) ("")
	
	test [effort_mean]`pred' = [hh_mean]`pred'
	post `wald' ("`outcome'") ("`pred'") ("HH = Effort") (r(p)) ("")

	test [hh_mean]`pred' = [mkt_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("HH = Market") (r(p)) ("") 

	*test [mkt_mean]`pred' = [mktx_mean]`pred' 
	*post `wald' ("`outcome'") ("`pred'") ("Market = Market X") (r(p)) ("")	
	
/* ----------- System x Consumption ------------------------------------------*/

	use "$processed/hh_level_stata", clear
	
	local outcome h_cons
	local pred h_cfr
	local outcome_level CFR
	local tablename "System_X_Consumption"
	
* These regressions will be calculated with robust SEs and output
	reg `outcome' `pred', vce(cluster cfrid)
	est sto base 
	post `betas' ("`outcome'") ("`pred'")  ("base") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
	* add effort
	reg `outcome' `pred' $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("`outcome'") ("`pred'") ("effort") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")

	* add hh characteristics
	reg `outcome' `pred' $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("`outcome'") ("`pred'") ("hh") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")
	
	* add market access
	reg `outcome' `pred' $effort $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("`outcome'") ("`pred'") ("mkt") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
		
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 

	outreg2 [base effort hh mkt mktx] using "$output/$date/tables/effort/index_regs_effort/`metric'_`tablename'", replace excel label ///
		title("Table X: `tablename' (`metric')") addnote("Outcome variable is `metric' at the `outcome_level' level. Robust standard errors clustered at CFR level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included in suest command and not in main regs.			
	reg `outcome' `pred'
	est sto base 
	* add effort
	reg `outcome' `pred' $effort
	est sto effort
	* add hh characteristics
	reg `outcome' `pred' $effort $hh
	est sto hh 
	* add market access
	reg `outcome' `pred' $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh
	est sto mktx 

	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]`pred' = [effort_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("Base = Effort") (r(p)) ("")
	
	test [effort_mean]`pred' = [hh_mean]`pred'
	post `wald' ("`outcome'") ("`pred'") ("HH = Effort") (r(p)) ("")

	test [hh_mean]`pred' = [mkt_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("HH = Market") (r(p)) ("") 

	*test [mkt_mean]`pred' = [mktx_mean]`pred' 
	*post `wald' ("`outcome'") ("`pred'") ("Market = Market X") (r(p)) ("")	


/* ----------- System x Sold -------------------------------------------------*/

	use "$processed/hh_level_stata", clear
	
	local outcome h_sold
	local pred h_cfr
	local outcome_level CFR
	local tablename "System_X_Sold"
	
* These regressions will be calculated with robust SEs and output
	reg `outcome' `pred', vce(cluster cfrid)
	est sto base 
	post `betas' ("`outcome'") ("`pred'")  ("base") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
	* add effort
	reg `outcome' `pred' $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("`outcome'") ("`pred'") ("effort") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")

	* add hh characteristics
	reg `outcome' `pred' $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("`outcome'") ("`pred'") ("hh") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")
	
	* add market access
	reg `outcome' `pred' $effort $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("`outcome'") ("`pred'") ("mkt") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
		
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 

	outreg2 [base effort hh mkt mktx] using "$output/$date/tables/effort/index_regs_effort/`metric'_`tablename'", replace excel label ///
		title("Table X: `tablename' (`metric')") addnote("Outcome variable is `metric' at the `outcome_level' level. Robust standard errors clustered at CFR level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included in suest command and not in main regs.			
	reg `outcome' `pred'
	est sto base 
	* add effort
	reg `outcome' `pred' $effort
	est sto effort
	* add hh characteristics
	reg `outcome' `pred' $effort $hh
	est sto hh 
	* add market access
	reg `outcome' `pred' $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh
	est sto mktx 

	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]`pred' = [effort_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("Base = Effort") (r(p)) ("")
	
	test [effort_mean]`pred' = [hh_mean]`pred'
	post `wald' ("`outcome'") ("`pred'") ("HH = Effort") (r(p)) ("")

	test [hh_mean]`pred' = [mkt_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("HH = Market") (r(p)) ("") 

	*test [mkt_mean]`pred' = [mktx_mean]`pred' 
	*post `wald' ("`outcome'") ("`pred'") ("Market = Market X") (r(p)) ("")	

	
	
/*----------------------------------------------------------------------------*/
/* ----------- Simpson Index -------------------------------------------------*/
/*----------------------------------------------------------------------------*/


local metric simpson
local sold_caveat "In these models `metric'_sold = 1 for households that sold no fish."


/* ----------- System x Catch ------------------------------------------------*/

use "$processed/hh_level_stata", clear

	local outcome d1_catch
	local pred d1_cfr
	local outcome_level CFR
	local tablename "System_X_Catch"
	
* These regressions will be calculated with robust SEs and output
	reg `outcome' `pred', vce(cluster cfrid)
	est sto base 
	post `betas' ("`outcome'") ("`pred'")  ("base") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
	* add effort
	reg `outcome' `pred' $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("`outcome'") ("`pred'") ("effort") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")

	* add hh characteristics
	reg `outcome' `pred' $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("`outcome'") ("`pred'") ("hh") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")
	
	* add market access
	reg `outcome' `pred' $effort $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("`outcome'") ("`pred'") ("mkt") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
		
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 

	outreg2 [base effort hh mkt mktx] using "$output/$date/tables/effort/index_regs_effort/`metric'_`tablename'", replace excel label ///
		title("Table X: `tablename' (`metric')") addnote("Outcome variable is `metric' at the `outcome_level' level. Robust standard errors clustered at CFR level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included in suest command and not in main regs.			
	reg `outcome' `pred'
	est sto base 
	* add effort
	reg `outcome' `pred' $effort
	est sto effort
	* add hh characteristics
	reg `outcome' `pred' $effort $hh
	est sto hh 
	* add market access
	reg `outcome' `pred' $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh
	est sto mktx 

	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]`pred' = [effort_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("Base = Effort") (r(p)) ("")
	
	test [effort_mean]`pred' = [hh_mean]`pred'
	post `wald' ("`outcome'") ("`pred'") ("HH = Effort") (r(p)) ("")

	test [hh_mean]`pred' = [mkt_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("HH = Market") (r(p)) ("") 

	*test [mkt_mean]`pred' = [mktx_mean]`pred' 
	*post `wald' ("`outcome'") ("`pred'") ("Market = Market X") (r(p)) ("")	
	

/* ----------- Catch x Consumption -------------------------------------------*/

	use "$processed/hh_level_stata", clear
	
	local outcome d1_cons
	local pred d1_catch
	local outcome_level catch
	local tablename "Catch_X_Consumption"
	
* These regressions will be calculated with robust SEs and output
	reg `outcome' `pred', vce(cluster cfrid)
	est sto base 
	post `betas' ("`outcome'") ("`pred'")  ("base") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
	
	* add effort
	reg `outcome' `pred' $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("`outcome'") ("`pred'") ("effort") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")

	* add hh characteristics
	reg `outcome' `pred' $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("`outcome'") ("`pred'") ("hh") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")
	
	* add market access
	reg `outcome' `pred' $effort $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("`outcome'") ("`pred'") ("mkt") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
		
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 

	outreg2 [base effort hh mkt mktx] using "$output/$date/tables/effort/index_regs_effort/`metric'_`tablename'", replace excel label ///
		title("Table X: `tablename' (`metric')") addnote("Outcome variable is `metric' at the `outcome_level' level. Robust standard errors clustered at CFR level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included in suest command and not in main regs.			
	reg `outcome' `pred'
	est sto base 
	* add effort
	reg `outcome' `pred' $effort
	est sto effort
	* add hh characteristics
	reg `outcome' `pred' $effort $hh
	est sto hh 
	* add market access
	reg `outcome' `pred' $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh
	est sto mktx 

	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]`pred' = [effort_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("Base = Effort") (r(p)) ("")
	
	test [effort_mean]`pred' = [hh_mean]`pred'
	post `wald' ("`outcome'") ("`pred'") ("HH = Effort") (r(p)) ("")

	test [hh_mean]`pred' = [mkt_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("HH = Market") (r(p)) ("") 

	*test [mkt_mean]`pred' = [mktx_mean]`pred' 
	*post `wald' ("`outcome'") ("`pred'") ("Market = Market X") (r(p)) ("")	
	

/* ----------- Catch x Sold --------------------------------------------------*/

	use "$processed/hh_level_stata", clear
	
	local outcome d1_sold
	local pred d1_catch
	local outcome_level catch
	local tablename "Catch_X_Sold"
	
* These regressions will be calculated with robust SEs and output
	reg `outcome' `pred', vce(cluster cfrid)
	est sto base 
	post `betas' ("`outcome'") ("`pred'")  ("base") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
	* add effort
	reg `outcome' `pred' $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("`outcome'") ("`pred'") ("effort") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")

	* add hh characteristics
	reg `outcome' `pred' $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("`outcome'") ("`pred'") ("hh") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")
	
	* add market access
	reg `outcome' `pred' $effort $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("`outcome'") ("`pred'") ("mkt") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
		
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 

	outreg2 [base effort hh mkt mktx] using "$output/$date/tables/effort/index_regs_effort/`metric'_`tablename'", replace excel label ///
		title("Table X: `tablename' (`metric')") addnote("Outcome variable is `metric' at the `outcome_level' level. `sold_caveat' Robust standard errors clustered at CFR level in parentheses. *** p<0.01, ** p<0.05, * p<0.1.")
	

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included in suest command and not in main regs.			
	reg `outcome' `pred'
	est sto base 
	* add effort
	reg `outcome' `pred' $effort
	est sto effort
	* add hh characteristics
	reg `outcome' `pred' $effort $hh
	est sto hh 
	* add market access
	reg `outcome' `pred' $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh
	est sto mktx 

	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]`pred' = [effort_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("Base = Effort") (r(p)) ("")
	
	test [effort_mean]`pred' = [hh_mean]`pred'
	post `wald' ("`outcome'") ("`pred'") ("HH = Effort") (r(p)) ("")

	test [hh_mean]`pred' = [mkt_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("HH = Market") (r(p)) ("") 

	*test [mkt_mean]`pred' = [mktx_mean]`pred' 
	*post `wald' ("`outcome'") ("`pred'") ("Market = Market X") (r(p)) ("")	
	
/* ----------- System x Consumption ------------------------------------------*/

	use "$processed/hh_level_stata", clear
	
	local outcome d1_cons
	local pred d1_cfr
	local outcome_level CFR
	local tablename "System_X_Consumption"
	
* These regressions will be calculated with robust SEs and output
	reg `outcome' `pred', vce(cluster cfrid)
	est sto base 
	post `betas' ("`outcome'") ("`pred'")  ("base") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
	* add effort
	reg `outcome' `pred' $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("`outcome'") ("`pred'") ("effort") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")

	* add hh characteristics
	reg `outcome' `pred' $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("`outcome'") ("`pred'") ("hh") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")
	
	* add market access
	reg `outcome' `pred' $effort $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("`outcome'") ("`pred'") ("mkt") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
		
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 

	outreg2 [base effort hh mkt mktx] using "$output/$date/tables/effort/index_regs_effort/`metric'_`tablename'", replace excel label ///
		title("Table X: `tablename' (`metric')") addnote("Outcome variable is `metric' at the `outcome_level' level. Robust standard errors clustered at CFR level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included in suest command and not in main regs.			
	reg `outcome' `pred'
	est sto base 
	* add effort
	reg `outcome' `pred' $effort
	est sto effort
	* add hh characteristics
	reg `outcome' `pred' $effort $hh
	est sto hh 
	* add market access
	reg `outcome' `pred' $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh
	est sto mktx 

	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]`pred' = [effort_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("Base = Effort") (r(p)) ("")
	
	test [effort_mean]`pred' = [hh_mean]`pred'
	post `wald' ("`outcome'") ("`pred'") ("HH = Effort") (r(p)) ("")

	test [hh_mean]`pred' = [mkt_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("HH = Market") (r(p)) ("") 

	*test [mkt_mean]`pred' = [mktx_mean]`pred' 
	*post `wald' ("`outcome'") ("`pred'") ("Market = Market X") (r(p)) ("")	


/* ----------- System x Sold -------------------------------------------------*/

	use "$processed/hh_level_stata", clear
	
	local outcome d1_sold
	local pred d1_cfr
	local outcome_level CFR
	local tablename "System_X_Sold"
	
* These regressions will be calculated with robust SEs and output
	reg `outcome' `pred', vce(cluster cfrid)
	est sto base 
	post `betas' ("`outcome'") ("`pred'")  ("base") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
	* add effort
	reg `outcome' `pred' $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("`outcome'") ("`pred'") ("effort") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")

	* add hh characteristics
	reg `outcome' `pred' $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("`outcome'") ("`pred'") ("hh") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")
	
	* add market access
	reg `outcome' `pred' $effort $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("`outcome'") ("`pred'") ("mkt") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
		
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 

	outreg2 [base effort hh mkt mktx] using "$output/$date/tables/effort/index_regs_effort/`metric'_`tablename'", replace excel label ///
		title("Table X: `tablename' (`metric')") addnote("Outcome variable is `metric' at the `outcome_level' level. `sold_caveat' Robust standard errors clustered at CFR level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included in suest command and not in main regs.			
	reg `outcome' `pred'
	est sto base 
	* add effort
	reg `outcome' `pred' $effort
	est sto effort
	* add hh characteristics
	reg `outcome' `pred' $effort $hh
	est sto hh 
	* add market access
	reg `outcome' `pred' $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh
	est sto mktx 

	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]`pred' = [effort_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("Base = Effort") (r(p)) ("")
	
	test [effort_mean]`pred' = [hh_mean]`pred'
	post `wald' ("`outcome'") ("`pred'") ("HH = Effort") (r(p)) ("")

	test [hh_mean]`pred' = [mkt_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("HH = Market") (r(p)) ("") 

	*test [mkt_mean]`pred' = [mktx_mean]`pred' 
	*post `wald' ("`outcome'") ("`pred'") ("Market = Market X") (r(p)) ("")
	
	
	
/*	
/*----------------------------------------------------------------------------*/
/* ----------- Inverse Simpson------------------------------------------------*/
/*----------------------------------------------------------------------------*/

Inverse simpson is complicated by infinite values. 
The code is here but it will need attention and thought as to how to handle the zero/infinite cases. 
If we drop households that have infinite inverse simpson index values (these are
households that did not sell fish), then we end up with regressions with different
sample sizes and cannot implement the wald test across models.

local metric inv_simpson
local sold_caveat "In these models `metric'_sold = 0 for households that sold no fish."


/* ----------- System x Catch ------------------------------------------------*/

use "$processed/hh_level_stata", clear

	local outcome d2_catch
	local pred d2_cfr
	local outcome_level CFR
	local tablename "System_X_Catch"
	
* These regressions will be calculated with robust SEs and output
	reg `outcome' `pred', vce(cluster cfrid)
	est sto base 
	post `betas' ("`outcome'") ("`pred'")  ("base") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
	* add effort
	reg `outcome' `pred' $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("`outcome'") ("`pred'") ("effort") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")

	* add hh characteristics
	reg `outcome' `pred' $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("`outcome'") ("`pred'") ("hh") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")
	
	* add market access
	reg `outcome' `pred' $effort $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("`outcome'") ("`pred'") ("mkt") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
		
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 

	outreg2 [base effort hh mkt mktx] using "$output/$date/tables/effort/index_regs_effort/`metric'_`tablename'", replace excel label ///
		title("Table X: `tablename' (`metric')") addnote("Outcome variable is `metric' at the `outcome_level' level. Robust standard errors clustered at CFR level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included in suest command and not in main regs.			
	reg `outcome' `pred'
	est sto base 
	* add effort
	reg `outcome' `pred' $effort
	est sto effort
	* add hh characteristics
	reg `outcome' `pred' $effort $hh
	est sto hh 
	* add market access
	reg `outcome' `pred' $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh
	est sto mktx 

	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]`pred' = [effort_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("Base = Effort") (r(p)) ("")
	
	test [effort_mean]`pred' = [hh_mean]`pred'
	post `wald' ("`outcome'") ("`pred'") ("HH = Effort") (r(p)) ("")

	test [hh_mean]`pred' = [mkt_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("HH = Market") (r(p)) ("") 

	*test [mkt_mean]`pred' = [mktx_mean]`pred' 
	*post `wald' ("`outcome'") ("`pred'") ("Market = Market X") (r(p)) ("")	
	

/* ----------- Catch x Consumption -------------------------------------------*/

	use "$processed/hh_level_stata", clear
	
	local outcome d2_cons
	local pred d2_catch
	local outcome_level catch
	local tablename "Catch_X_Consumption"
	
* These regressions will be calculated with robust SEs and output
	reg `outcome' `pred', vce(cluster cfrid)
	est sto base 
	post `betas' ("`outcome'") ("`pred'")  ("base") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
	* add effort
	reg `outcome' `pred' $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("`outcome'") ("`pred'") ("effort") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")

	* add hh characteristics
	reg `outcome' `pred' $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("`outcome'") ("`pred'") ("hh") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")
	
	* add market access
	reg `outcome' `pred' $effort $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("`outcome'") ("`pred'") ("mkt") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
		
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 

	outreg2 [base effort hh mkt mktx] using "$output/$date/tables/effort/index_regs_effort/`metric'_`tablename'", replace excel label ///
		title("Table X: `tablename' (`metric')") addnote("Outcome variable is `metric' at the `outcome_level' level. Robust standard errors clustered at CFR level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included in suest command and not in main regs.			
	reg `outcome' `pred'
	est sto base 
	* add effort
	reg `outcome' `pred' $effort
	est sto effort
	* add hh characteristics
	reg `outcome' `pred' $effort $hh
	est sto hh 
	* add market access
	reg `outcome' `pred' $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh
	est sto mktx 

	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]`pred' = [effort_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("Base = Effort") (r(p)) ("")
	
	test [effort_mean]`pred' = [hh_mean]`pred'
	post `wald' ("`outcome'") ("`pred'") ("HH = Effort") (r(p)) ("")

	test [hh_mean]`pred' = [mkt_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("HH = Market") (r(p)) ("") 

	*test [mkt_mean]`pred' = [mktx_mean]`pred' 
	*post `wald' ("`outcome'") ("`pred'") ("Market = Market X") (r(p)) ("")	
	

/* ----------- Catch x Sold --------------------------------------------------*/

	use "$processed/hh_level_stata", clear
	
	local outcome d2_sold
	local pred d2_catch
	local outcome_level catch
	local tablename "Catch_X_Sold"
	
* These regressions will be calculated with robust SEs and output
	reg `outcome' `pred', vce(cluster cfrid)
	est sto base 
	post `betas' ("`outcome'") ("`pred'")  ("base") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
	* add effort
	reg `outcome' `pred' $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("`outcome'") ("`pred'") ("effort") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")

	* add hh characteristics
	reg `outcome' `pred' $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("`outcome'") ("`pred'") ("hh") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")
	
	* add market access
	reg `outcome' `pred' $effort $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("`outcome'") ("`pred'") ("mkt") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
		
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 

	outreg2 [base effort hh mkt mktx] using "$output/$date/tables/effort/index_regs_effort/`metric'_`tablename'", replace excel label ///
		title("Table X: `tablename' (`metric')") addnote("Outcome variable is `metric' at the `outcome_level' level. `sold_caveat' Robust standard errors clustered at CFR level in parentheses. *** p<0.01, ** p<0.05, * p<0.1.")
	

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included in suest command and not in main regs.			
	reg `outcome' `pred'
	est sto base 
	* add effort
	reg `outcome' `pred' $effort
	est sto effort
	* add hh characteristics
	reg `outcome' `pred' $effort $hh
	est sto hh 
	* add market access
	reg `outcome' `pred' $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh
	est sto mktx 

	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]`pred' = [effort_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("Base = Effort") (r(p)) ("")
	
	test [effort_mean]`pred' = [hh_mean]`pred'
	post `wald' ("`outcome'") ("`pred'") ("HH = Effort") (r(p)) ("")

	test [hh_mean]`pred' = [mkt_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("HH = Market") (r(p)) ("") 

	*test [mkt_mean]`pred' = [mktx_mean]`pred' 
	*post `wald' ("`outcome'") ("`pred'") ("Market = Market X") (r(p)) ("")	
	
/* ----------- System x Consumption ------------------------------------------*/

	use "$processed/hh_level_stata", clear
	
	local outcome d2_cons
	local pred d2_cfr
	local outcome_level CFR
	local tablename "System_X_Consumption"
	
* These regressions will be calculated with robust SEs and output
	reg `outcome' `pred', vce(cluster cfrid)
	est sto base 
	post `betas' ("`outcome'") ("`pred'")  ("base") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
	* add effort
	reg `outcome' `pred' $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("`outcome'") ("`pred'") ("effort") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")

	* add hh characteristics
	reg `outcome' `pred' $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("`outcome'") ("`pred'") ("hh") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")
	
	* add market access
	reg `outcome' `pred' $effort $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("`outcome'") ("`pred'") ("mkt") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
		
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 

	outreg2 [base effort hh mkt mktx] using "$output/$date/tables/effort/index_regs_effort/`metric'_`tablename'", replace excel label ///
		title("Table X: `tablename' (`metric')") addnote("Outcome variable is `metric' at the `outcome_level' level. Robust standard errors clustered at CFR level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included in suest command and not in main regs.			
	reg `outcome' `pred'
	est sto base 
	* add effort
	reg `outcome' `pred' $effort
	est sto effort
	* add hh characteristics
	reg `outcome' `pred' $effort $hh
	est sto hh 
	* add market access
	reg `outcome' `pred' $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh
	est sto mktx 

	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]`pred' = [effort_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("Base = Effort") (r(p)) ("")
	
	test [effort_mean]`pred' = [hh_mean]`pred'
	post `wald' ("`outcome'") ("`pred'") ("HH = Effort") (r(p)) ("")

	test [hh_mean]`pred' = [mkt_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("HH = Market") (r(p)) ("") 

	*test [mkt_mean]`pred' = [mktx_mean]`pred' 
	*post `wald' ("`outcome'") ("`pred'") ("Market = Market X") (r(p)) ("")	


/* ----------- System x Sold -------------------------------------------------*/

	use "$processed/hh_level_stata", clear
	
	local outcome d2_sold
	local pred d2_cfr
	local outcome_level CFR
	local tablename "System_X_Sold"
	
* These regressions will be calculated with robust SEs and output
	reg `outcome' `pred', vce(cluster cfrid)
	est sto base 
	post `betas' ("`outcome'") ("`pred'")  ("base") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
	* add effort
	reg `outcome' `pred' $effort, vce(cluster cfrid)
	est sto effort
	post `betas' ("`outcome'") ("`pred'") ("effort") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")

	* add hh characteristics
	reg `outcome' `pred' $effort $hh, vce(cluster cfrid)
	est sto hh 
	post `betas' ("`outcome'") ("`pred'") ("hh") (_b[`pred']) (_se[`pred']) ("`pred'")  ("")
	
	* add market access
	reg `outcome' `pred' $effort $hh $mkt, vce(cluster cfrid) 
	est sto mkt 
	post `betas' ("`outcome'") ("`pred'") ("mkt") (_b[`pred']) (_se[`pred']) ("`pred'") ("")
		
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh, vce(cluster cfrid) 
	est sto mktx 

	outreg2 [base effort hh mkt mktx] using "$output/$date/tables/effort/index_regs_effort/`metric'_`tablename'", replace excel label ///
		title("Table X: `tablename' (`metric')") addnote("Outcome variable is `metric' at the `outcome_level' level. `sold_caveat' Robust standard errors clustered at CFR level in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
	

** Set up Wald test of equality across model coefficients

	* These are same regressions as above, but used for suest test so robust SEs are included in suest command and not in main regs.			
	reg `outcome' `pred'
	est sto base 
	* add effort
	reg `outcome' `pred' $effort
	est sto effort
	* add hh characteristics
	reg `outcome' `pred' $effort $hh
	est sto hh 
	* add market access
	reg `outcome' `pred' $effort $hh $mkt
	est sto mkt 
	* interact market access
	reg `outcome' c.`pred'##c.city_distance $effort $hh
	est sto mktx 

	suest base effort hh mkt mktx, vce(cluster cfrid)

	* Wald test whether coefficients are different from each other across models 
	test [base_mean]`pred' = [effort_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("Base = Effort") (r(p)) ("")
	
	test [effort_mean]`pred' = [hh_mean]`pred'
	post `wald' ("`outcome'") ("`pred'") ("HH = Effort") (r(p)) ("")

	test [hh_mean]`pred' = [mkt_mean]`pred'  
	post `wald' ("`outcome'") ("`pred'") ("HH = Market") (r(p)) ("") 

	*test [mkt_mean]`pred' = [mktx_mean]`pred' 
	*post `wald' ("`outcome'") ("`pred'") ("Market = Market X") (r(p)) ("")
*/
	
	
/*----------------------------------------------------------------------------*/
/* ----------- Export coefficients and wald tests ----------------------------*/
/*----------------------------------------------------------------------------*/	
	
postclose `betas'
use "$temp/betas_index", clear
* calculate min and max
gen min = coeff-1.96*se
gen max = coeff+1.96*se
export excel using "$output/$date/tables/effort/betas_index.xlsx", sheet("Index models") sheetmodify firstrow(variables)


postclose `wald'
use "$temp/wald_test_index", clear
export excel using "$output/$date/tables/effort/wald_test_index.xlsx", sheet("Index models") sheetmodify firstrow(variables)
