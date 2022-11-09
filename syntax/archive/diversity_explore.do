** Liz Bageant
** July 12, 2022

** Look at the relationship between CFR level biomonitoring and HOUSEHOLD level catch 
* (combine_binary.do looks at CFR level catch rather than household)

/*------------------------------------------------------------------------------
	Biodiversity Portfolio 1: B_ct = 2 months of B data
	This creates a dataset at the CFR-catchperiod-species level where for each 
	period where we have catch data (catchperiod)
------------------------------------------------------------------------------*/
/* Create a catchperiod variable in the catch data that will be used to match 
	with the aggregated biodiversity portfolio variable */
	
use "$processed/ccm_q7.dta", clear	

	gen catchperiod = . 
	replace catchperiod = 1 if year == 2012 & month == 11
	replace catchperiod = 2 if year == 2013 & month == 1
	replace catchperiod = 3 if year == 2013 & month == 3
	replace catchperiod = 4 if year == 2013 & month == 5
	replace catchperiod = 5 if year == 2013 & month == 8
	replace catchperiod = 6 if year == 2013 & month == 9
	replace catchperiod = 7 if year == 2013 & month == 11
	replace catchperiod = 8 if year == 2014 & month == 1
	replace catchperiod = 9 if year == 2014 & month == 3
	replace catchperiod = 10 if year == 2014 & month == 5
	replace catchperiod = 11 if year == 2014 & month == 7
	replace catchperiod = 12 if year == 2014 & month == 9
	replace catchperiod = 13 if year == 2014 & month == 11
	replace catchperiod = 14 if year == 2015 & month == 1
	replace catchperiod = 15 if year == 2015 & month == 3
	replace catchperiod = 16 if year == 2015 & month == 5
	replace catchperiod = 17 if year == 2015 & month == 7
	replace catchperiod = 18 if year == 2015 & month == 9
	replace catchperiod = 19 if year == 2015 & month == 11
	
	tab catchperiod, miss

gen scode = scode_ccm
save "$temp/catch1", replace	

/* Create separate files for each biodiversity portfolio time period (which
	is the addition of two time periods that correspond with a given catchperiod)*/

	use "$processed/biom_formb.dta", clear
	local i = 3
	keep if year == 2012 & month == 11 | year == 2013 & month == 2
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = 	`i'
	save "$temp/bct`i'", replace

	use "$processed/biom_formb.dta", clear
	local i = 4
	keep if year == 2013 & month == 2 | year == 2013 & month ==5
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = 	`i'
	save "$temp/bct`i'", replace

	use "$processed/biom_formb.dta", clear
	local i = 5
	keep if year == 2013 & month == 5 | year == 2013 & month == 8
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = `i'
	save "$temp/bct`i'", replace

	use "$processed/biom_formb.dta", clear
	local i = 6
	keep if year == 2013 & month == 5 | year == 2013 & month == 8
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = `i'
	save "$temp/bct`i'", replace

	use "$processed/biom_formb.dta", clear
	local i = 7
	keep if year == 2013 & month == 8 | year == 2013 & month == 11
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = `i'
	save "$temp/bct`i'", replace

	use "$processed/biom_formb.dta", clear
	local i = 8
	keep if year == 2013 & month == 8 | year == 2013 & month == 11
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = `i'
	save "$temp/bct`i'", replace

	use "$processed/biom_formb.dta", clear
	local i = 9
	keep if year == 2013 & month == 11 | year == 2014 & month == 2
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = `i'
	save "$temp/bct`i'", replace

	use "$processed/biom_formb.dta", clear
	local i = 10
	keep if year == 2014 & month == 2 | year == 2014 & month == 5
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = `i'
	save "$temp/bct`i'", replace

	use "$processed/biom_formb.dta", clear
	local i = 11
	keep if year == 2014 & month == 2 | year == 2014 & month == 5
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = `i'
	save "$temp/bct`i'", replace

	use "$processed/biom_formb.dta", clear
	local i = 12
	keep if year == 2014 & month == 5 | year == 2014 & month == 8
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = `i'
	save "$temp/bct`i'", replace

	use "$processed/biom_formb.dta", clear
	local i = 13
	keep if year == 2014 & month == 8 | year == 2014 & month == 11
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = `i'
	save "$temp/bct`i'", replace

	use "$processed/biom_formb.dta", clear
	local i = 14
	keep if year == 2014 & month == 8 | year == 2014 & month == 11
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = `i'
	save "$temp/bct`i'", replace

	use "$processed/biom_formb.dta", clear
	local i = 15
	keep if year == 2014 & month == 11 | year == 2015 & month == 2
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = `i'
	save "$temp/bct`i'", replace

	use "$processed/biom_formb.dta", clear
	local i = 16
	keep if year == 2015 & month == 2 | year == 2015 & month == 5
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = `i'
	save "$temp/bct`i'", replace

	use "$processed/biom_formb.dta", clear
	local i = 17
	keep if year == 2015 & month == 2 | year == 2015 & month == 5
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = `i'
	save "$temp/bct`i'", replace

	use "$processed/biom_formb.dta", clear
	local i = 18
	keep if year == 2015 & month == 5 | year == 2015 & month == 8
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = `i'
	save "$temp/bct`i'", replace

	use "$processed/biom_formb.dta", clear
	local i = 19
	keep if year == 2015 & month == 8 | year == 2015 & month == 11
	duplicates report cfrid year month scode // no duplicates
	gen catchperiod = `i'
	save "$temp/bct`i'", replace



	* append
	cd "$temp"
	use bct3, clear
		forv i = 4/19 {
		append using bct`i'
		}

	count // n = 20,003 observations

	/* check to confirm identical pairs. Identical pairs should be:
		5 & 6
		7 & 8 
		10 & 11
		13 & 14
		16 & 17
		*/
	forv i = 3/19 {
		qui: count if catchperiod == `i'
		di "Catchperiod `i': `r(N)'"
		}

		/*
		Catchperiod 3: 1417
		Catchperiod 4: 1057
		Catchperiod 5: 937
		Catchperiod 6: 937
		Catchperiod 7: 1284
		Catchperiod 8: 1284
		Catchperiod 9: 1499
		Catchperiod 10: 1176
		Catchperiod 11: 1176
		Catchperiod 12: 982
		Catchperiod 13: 1216
		Catchperiod 14: 1216
		Catchperiod 15: 1346
		Catchperiod 16: 1127
		Catchperiod 17: 1127
		Catchperiod 18: 972
		Catchperiod 19: 1151
		*/

	gen scode = scode_biom	
	duplicates tag cfrid catchperiod scode, gen(tag)
	* duplicates are a logical outcome of combining multiple time periods into catchperiods
	keep cfrid catchperiod scode
	duplicates drop
	
	save "$temp/biodiv1", replace	

/* Combine the two files to see whether that generates enough species overlap */

use "$temp/biodiv1", clear
merge 1:m cfrid catchperiod scode using "$temp/catch1"
	/*
    Result                           # of obs.
    -----------------------------------------
    not matched                        23,870
        from master                     9,841  (_merge==1) <-- species found in CFR biomonitoring but not caught by fishers
        from using                     14,029  (_merge==2) <-- species caught by fishers but not found in CFR biomonitoring

    matched                            12,905  (_merge==3)
    -----------------------------------------
	*/
	
	distinct(scode) if _mer == 2 // 130 species caught by fishers but not found in biomonitoring

	
/*------------------------------------------------------------------------------
	Biodiversity portfolio 2: B_c. Aggregate all CFR species over all time points
	and compare to catch data. Are there still unrepresented species?
------------------------------------------------------------------------------*/
	
use "$processed/biom_formb.dta", clear
keep scode
ren scode_biom scode
duplicates drop
count
save "$temp/biodiv2", replace

use "$temp/catch1", clear
keep scode
duplicates drop
count
merge 1:1 scode using "$temp/biodiv2"
	/*
		Result                           # of obs.
		-----------------------------------------
		not matched                            12
			from master                         5  (_merge==1)
			from using                          7  (_merge==2)

		matched                               125  (_merge==3)
		-----------------------------------------
	*/











































	
	
	

/*
/*------------------------------------------------------------------------------
	Biodiversity 2: CFR biomonitoring species + all catch species (list of 
	species by CFR by time point)
------------------------------------------------------------------------------*/

use "$temp/biodiv1", clear
keep if timekey2 != . 
merge 1:m cfrid timekey2 scode using "$temp/catch1"

	/*
		Result                           # of obs.
		-----------------------------------------
		not matched                        22,508
			from master                     4,738  (_merge==1) <-- these are species that were found in a CFR at a given time point but not caught by any households in that time point
			from using                     17,770  (_merge==2) <-- these are species that were caught by a hh at a given time point, but not found in that CFR at that time point

		matched                             9,164  (_merge==3) <-- these are species that were found in a CFR at a given time point adn were also caught by households
		-----------------------------------------
	*/

order hhid year month cfrid scode scode_*
sort hhid year month cfrid scode


/*------------------------------------------------------------------------------
	Catch diversity 1: Household level catch data by species and time point
------------------------------------------------------------------------------*/
use "$processed/ccm_q7.dta", clear

isid hhid year month scode_ccm

keep hhid cfrid year month scode_ccm sname_ccm timekey2
bys hhid month year: egen hh_speciescount = count(scode)
la var hh_speciescount "Number of species caught by household in a given period"

gen scode = scode_ccm
la var scode "Species code"

duplicates report // 0 duplicates, as expected.

save "$temp/catch1", replace


/*------------------------------------------------------------------------------
	Catch diversity 2: Household level catch data by species and time point, 
	dropping species that were not represented in the biomonitoring data**
------------------------------------------------------------------------------*/





















