** Liz Bageant
** June 9, 2022

** This file combines catch and CFR biomonitoring species to create binary 
** variables indicting what species were present


/* 
NOTE: Before merging, decisions about how to align misaligned data collection
	times must be made. I have created three different varaibles that can be used
	to merge the timepoints:
		- timekey1 aligns all catch data with the biomonitoring data point that
			precedes it. The distance in time from biomonitoring to catch data
			ranges from 0 months to 3 months. 
		- timekey2 aligns all catch data with the biomonitoring data point that
			precedes it within 2 months. The distance in time from biomonitoring
			to catch data ranges from 0-2 months. This is more conservative. 
			Catch data that does not have biomonitoring data from 2 months prior
			or less is dropped. 
		- timekey3 represents only cases where catch and biomonitoring dates
			match perfectly (month/year)
	The mappings for timekey1, timekey2 and timekey3 are listed in timekey_mapping.xlsx.

*/

/*------------------------------------------------------------------------------
	Prepare biomonitoring data
------------------------------------------------------------------------------*/

use "$processed/biom_formb.dta", clear

* map time points

* timekey1
gen timekey1 = . 
	replace timekey1 = 1 if month == 11 & year == 2012
	replace timekey1 = 2 if month == 2 & year == 2013
	replace timekey1 = 3 if month == 5 & year == 2013
	replace timekey1 = 4 if month == 8 & year == 2013
	replace timekey1 = 5 if month == 11 & year == 2013
	replace timekey1 = 6 if month == 2 & year == 2014
	replace timekey1 = 7 if month == 5 & year == 2014
	replace timekey1 = 8 if month == 8 & year == 2014
	replace timekey1 = 9 if month == 11 & year == 2014
	replace timekey1 = 10 if month == 2 & year == 2015
	replace timekey1 = 11 if month == 5 & year == 2015
	replace timekey1 = 12 if month == 8 & year == 2015
	replace timekey1 = 13 if month == 11 & year == 2015

* timekey2 
* Note: for biomonitoring data timekey1 = timekey2 (see timekey_mapping.xlsx for details)
	gen timekey2 = timekey1
	
* timekey3
	gen timekey3 = .
	replace timekey3 = 1 if month == 11 & year == 2012
	replace timekey3 = 2 if month == 5 & year == 2013
	replace timekey3 = 3 if month == 8 & year == 2013
	replace timekey3 = 4 if month == 11 & year == 2013
	replace timekey3 = 5 if month == 5 & year == 2014
	replace timekey3 = 6 if month == 11 & year == 2014
	replace timekey3 = 7 if month == 5 & year == 2015
	replace timekey3 = 8 if month == 11 & year == 2015

keep cfrid year month scode sname timekey*

gen code = scode 
gen name = sname

save "$temp/formb", replace

/*------------------------------------------------------------------------------
	Prepare catch data
------------------------------------------------------------------------------*/

use "$processed/ccm_q7.dta", clear

* map time points

* timekey1
	gen timekey1 = .
	replace timekey1 = 1 if month == 11 & year ==  2012
	replace timekey1 = 1 if month == 1 & year ==  2013
	replace timekey1 = 2 if month == 3 & year ==  2013
	replace timekey1 = 3 if month == 5 & year ==  2013
	replace timekey1 = 4 if month == 8 & year ==  2013
	replace timekey1 = 4 if month == 9 & year ==  2013
	replace timekey1 = 5 if month == 11 & year ==  2013
	replace timekey1 = 5 if month == 1 & year ==  2014
	replace timekey1 = 6 if month == 3 & year ==  2014
	replace timekey1 = 7 if month == 5 & year ==  2014
	replace timekey1 = 7 if month == 7 & year ==  2014
	replace timekey1 = 8 if month == 9 & year ==  2014
	replace timekey1 = 9 if month == 11 & year ==  2014
	replace timekey1 = 9 if month == 1 & year ==  2015
	replace timekey1 = 10 if month == 3 & year ==  2015
	replace timekey1 = 11 if month == 5 & year ==  2015
	replace timekey1 = 11 if month == 7 & year ==  2015
	replace timekey1 = 12 if month == 9 & year ==  2015
	replace timekey1 = 13 if month == 11 & year ==  2015

* timekey2
	gen timekey2 = . 
	replace timekey2 = 1 if month == 11 & year == 2012
	replace timekey2 = 1 if month == 1 & year == 2013
	replace timekey2 = 2 if month == 3 & year == 2013
	replace timekey2 = 3 if month == 5 & year == 2013
	replace timekey2 = 4 if month == 8 & year == 2013
	replace timekey2 = 4 if month == 9 & year == 2013
	replace timekey2 = 5 if month == 11 & year == 2013
	replace timekey2 = 6 if month == 3 & year == 2014
	replace timekey2 = 7 if month == 5 & year == 2014
	replace timekey2 = 8 if month == 9 & year == 2014
	replace timekey2 = 9 if month == 11 & year == 2014
	replace timekey2 = 10 if month == 3 & year == 2015
	replace timekey2 = 11 if month == 5 & year == 2015
	replace timekey2 = 12 if month == 9 & year == 2015
	replace timekey2 = 13 if month == 11 & year == 2015

* timekey3
	gen timekey3 = .
	replace timekey3 = 1 if month == 11 & year == 2012
	replace timekey3 = 2 if month == 5 & year == 2013
	replace timekey3 = 3 if month == 8 & year == 2013
	replace timekey3 = 4 if month == 11 & year == 2013
	replace timekey3 = 5 if month == 5 & year == 2014
	replace timekey3 = 6 if month == 11 & year == 2014
	replace timekey3 = 7 if month == 5 & year == 2015
	replace timekey3 = 8 if month == 11 & year == 2015

keep hhid cfrid year month soldfresh atefresh process lost other timekey* scode sname

gen code = scode
gen name = sname
save "$temp/q7", replace

/*------------------------------------------------------------------------------
	Merge on timekey1
------------------------------------------------------------------------------*/

use "$temp/q7.dta", clear
merge m:1 cfrid timekey1 code using "$temp/formb"

	/*
    -----------------------------------------
    not matched                        20,093
        from master                    15,621  (_merge==1) <-- fish caught but not in biomonitoring
        from using                      4,472  (_merge==2) <-- fish in biomonitoring but not caught

    matched                            11,313  (_merge==3)
    -----------------------------------------
	*/
	
/*------------------------------------------------------------------------------
	Investigate total number of species in CFR vs catch using TIMEKEY1
------------------------------------------------------------------------------*/
use "$temp/q7", clear
bys cfr: distinct(code)	

keep cfrid code name
duplicates drop
tab cfrid, miss // this is the number of different species caught in each CFR
gen catchspecies = 1
collapse (count) catchspecies, by(cfrid)
save "$temp/c", replace

use "$temp/formb", clear
keep cfrid code name
duplicates drop
tab cfrid, miss // this is the number of different species observed in each CFR
gen cfrspecies = 1
collapse (count) cfrspecies, by(cfrid)

merge 1:1 cfrid using "$temp/c"
drop _mer
count if catchspecies > cfrspecies // 27 cases (67%)
gen diff = cfrspecies - catchspecies
	
	/** TAKEAWAY: 27 out of 40 CFRs have broader catch species than is found 
		in CFR. This isn't shocking because they could be fishing much more 
		widely than the area of influence of the CFR. */
		
* Do the differences above vary over time? 
use "$temp/q7", clear
keep cfrid timekey1 code name
duplicates drop
gen catchspecies = 1
collapse (count) catchspecies, by(cfrid timekey1)
save "$temp/c1", replace

use "$temp/formb", clear
keep cfrid timekey1 code name
duplicates drop
gen cfrspecies = 1
collapse (count) cfrspecies, by(cfrid timekey1)

merge 1:1 cfrid timekey1 using "$temp/c1"
keep if _mer == 3
count if catchspecies > cfrspecies // 303 out of 504 (60%)
gen diff = cfrspecies - catchspecies
sort diff

gen posdif = (diff >=0)
tab  timekey1 posdif

	/** TAKEAWAY: Roughly same as above. */

	
/*------------------------------------------------------------------------------
	Investigate total number of species in CFR vs catch using TIMEKEY3
------------------------------------------------------------------------------*/	
use "$temp/q7", clear
bys cfr: distinct(code)	
keep if timekey3 != .

keep cfrid code name
duplicates drop
tab cfrid, miss // this is the number of different species caught in each CFR
gen catchspecies = 1
collapse (count) catchspecies, by(cfrid)
save "$temp/c", replace

use "$temp/formb", clear
keep if timekey3 != . 
keep cfrid code name
duplicates drop
tab cfrid, miss // this is the number of different species observed in each CFR
gen cfrspecies = 1
collapse (count) cfrspecies, by(cfrid)

merge 1:1 cfrid using "$temp/c"
drop _mer
count if catchspecies > cfrspecies // 20 cases (50%)
gen diff = cfrspecies - catchspecies


* Do the differences above vary over time? 
use "$temp/q7", clear
keep cfrid timekey3 code name
duplicates drop
drop if timekey3 == .
gen catchspecies = 1
collapse (count) catchspecies, by(cfrid timekey3)
save "$temp/c1", replace

use "$temp/formb", clear
keep cfrid timekey3 code name
duplicates drop
drop if timekey3 == .
gen cfrspecies = 1
collapse (count) cfrspecies, by(cfrid timekey3)

merge 1:1 cfrid timekey3 using "$temp/c1"
keep if _mer == 3
count if catchspecies > cfrspecies // 156 out of 305 (51%)
gen diff = cfrspecies - catchspecies
sort diff	

	/** TAKEAWAY: Given that tightening up the timeline matching only marginally
		improves the issue, it is likely driven by other systemic factors such as:
		- fishers being better at fishing than CFR biomonitoring team
		- fishers' universe in which to fish is larger than CFR or CFR ZOI
		- some fish species may not ever be in CFR due to habitat needs */
		
Ideas for next steps: 
Use the Q5 data to narrow down fish catch to that we know took place in
the CFR or the ZOI. Not clear that we will be able to link this to specific 
species because we won't know where each species was caught, so some assumptions
would need to be made. 
