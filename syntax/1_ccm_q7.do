** Liz Bageant
** July 12, 2022

** Catch and Consumption Q7 and Q7sub

/*------------------------------------------------------------------------------
	Match cfrid to hhid
------------------------------------------------------------------------------*/


/*------------------------------------------------------------------------------
	Organize catch data from Q7
------------------------------------------------------------------------------*/
use "$stata_raw/Catch & Consumption_Q7.dta", clear

count // 23,448

isid householdid dateinter speciesgroup

* tidy
	* rename varibales for easy typing
	ren speciesgroup group
	ren householdid hhid
	* generate date variables
	replace dateinter = strtrim(dateinter)
	gen year = substr(dateinter, -4, 4)
	destring year, replace
	gen month = substr(dateinter, 1, 2)
	replace month = subinstr(month, "/", " ", .)
	destring month, replace
	destring hhid, replace
	* Parse speciesgroup
	replace group = substr(group, -2, 2)
	destring group, replace
	
	order hhid month year group
	destring *, replace
	
	* generate a total weight variable before merge
	bys hhid month year: egen hhcatch_weight = total(weight_indicate)
	
	* how do use breakdowns by weight relate to the total weight indicated
	egen temp = rowtotal(weight_soldfresh weight_consumedfresh weight_process weight_lost weight_other)
	bys hhid month year: egen breakdown_weight = total(temp)
	br hhcatch_weight breakdown_weight
	count if hhcatch_weight != breakdown_weight // 3131
	count if breakdown_weight > hhcatch_weight  // 1720 cases where breakdowns sum to more than total catch
	gen weight_discrepancy = 1 if hhcatch_weight != breakdown_weight
		//NOTE: FURTHER INVESTIGATION NEEDED HERE. Could recalculate from percentages to start.
	
	drop temp
	
	save "$temp/q7", replace
	

/*------------------------------------------------------------------------------
	Organize catch data from Q7sub
------------------------------------------------------------------------------*/
/* This file contains species names that a given household caught and the 
corresponding species group code. 

1. Parce speciesgroup to get a numeric code. 
2. Update codes per "species code and group reconciliation.xlsx"
3. Identify cases there are of multiple species per code
4. Merge with Q7 data 
5. Divide Q7 weights by the number of species per group
*/

use "$stata_raw/Catch & Consumption_Q7sub.dta", clear
sort householdid dateinter speciesgroup

* Parse speciesgroup to get a numeric code
	replace speciesgroup = substr(speciesgroup, -2, 2)
	destring speciesgroup, replace
	destring speciescode, replace

* Update codes per "species code and group reconciliation.xlsx"
do "$do/0_code_fix.do"
	// 0_code_fix.do creates several duplicates in terms of all variables that need to be eliminated prior to merging with q7
	duplicates tag, gen(tag)
	duplicates drop // 3 observations deleted
	drop tag
	
* Identify cases where there are multiple species per code
	
	* tidy
	ren speciesgroup group
	ren speciescode code
	ren speciesname name
	ren householdid hhid
	* generate date variables
	replace dateinter = strtrim(dateinter)
	gen year = substr(dateinter, -4, 4)
	destring year, replace
	gen month = substr(dateinter, 1, 2)
	replace month = subinstr(month, "/", " ", .)
	destring month, replace
	
	destring hhid, replace

	duplicates tag hhid year month group, gen(duplicate)

	la var duplicate "Indicates cases where two species within a group were caught by hh"

	tab duplicate, miss

		/*
		  duplicate |      Freq.     Percent        Cum.
		------------+-----------------------------------
				  0 |     20,130       74.46       74.46 <-- only one species in a group
				  1 |      5,642       20.87       95.33
				  2 |        894        3.31       98.64
				  3 |        232        0.86       99.50
				  4 |         85        0.31       99.81
				  5 |         18        0.07       99.88
				  6 |         14        0.05       99.93
				  7 |          8        0.03       99.96
				 10 |         11        0.04      100.00
		------------+-----------------------------------
			  Total |     27,034      100.00
		*/


* Merge with Q7 data containing weights and fish destiny
	merge m:1 hhid month year group using "$temp/q7.dta"

		/*
			Result                           # of obs.
			-----------------------------------------
			not matched                           121
				from master                         2  (_merge==1)
				from using                        119  (_merge==2) <-- HH reported catching fish, but no species were selected in Q7sub

			matched                            27,029  (_merge==3)
			-----------------------------------------
		*/

	distinct(hhid) if _mer == 2 // 76 households
	tab year if _mer == 2 // relativley evenly distributed across years
	tab month if _mer == 2 // lots in sept and nov. Not sure why...

	list hhid month year if _mer == 1, clean noobs

		/*
			hhid   month   year  
			 143       9   2013  
			 151       5   2015  
		*/

	keep if _mer == 3 
	drop _mer

	destring *,replace


* Checking understanding of Q7 data
	bys hhid month year: egen sumweight = total(weight_indicate)
	bys hhid month year: egen sumpct = total(indicatefrom)
	bys hhid month year: egen dupmax = max(duplicate) // generate a variable indicating that this household/year/month has multiple species per group
		distinct(hhid) if dupmax != 0 // 353 households have multiple species per group

		* check whether all percentages sum to zero in cases where there are no duplicates
		count if sumpct > 101 & dupmax == 0 // 0 meaning that all cases where percentages sum to > 100 are because of multiple species within a group, as expected
											// using 101 instead of 100 because of rounding errors
		*br hhid month year group duplicate indicatefrom weight_indicate sumpct if sumpct > 101 & dupmax > 0
		
		* check whether all weights sum to the household catch weight (hhweight_catch)
		count if sumweight != hhcatch_weight & dupmax == 0 // 190 observations
		distinct(hhid) if sumweight != hhcatch_weight & dupmax == 0 // 190 obs across 40 households
		gen diff = sumweight - hhcatch_weight
		sum diff, d
		count if diff != 0 // 16546 61% of observations
		count if diff >0 // 98% of all differences are positive
		gen pctdiff = diff/sumweight
		sum pctdiff, d // some of these are quite large
			// In cases where sumweight != hhcatch_weight, the calculated weight is almost
			// always larger than the originally reported weight, in some cases by 
			// a large amount. I am unsure of the implications of this.
		

* Divide each weight by the number of duplicates. 
	* NOTE: duplicates variables indicate the number of ADDITIONAL DUPLICATE observations, 
		* therefore duplicate = 1 indicates that there are two rows of data. 
	gen duplicate_divider = duplicate + 1
	gen catch_iweight = weight_indicate/duplicate_divider
	la var catch_iweight "Reported catch weight by species (imputed by dividing equally)"
	
	gen soldfresh_iweight = weight_soldfresh/duplicate_divider
		la var soldfresh_iweight "Reported sold weight by species (imputed by dividing equally)"
	gen atefresh_iweight = weight_consumedfresh/duplicate_divider
		la var atefresh_iweight "Reported consumed weight by species (imputed by dividing equally)"
	gen process_iweight = weight_process/duplicate_divider
		la var process_iweight "Reported processed weight by species (imputed by dividing equally)"
	gen lost_iweight = weight_lost/duplicate_divider
		la var lost_iweight "Reported lost weight by species (imputed by dividing equally)"
	gen other_iweight = weight_other/duplicate_divider
		la var other_iweight "Reported 'other' weight by species (imputed by dividing equally)"
	
	ren weight_indicatefromq5 q5_weight_bygroup
	la var q5_weight "Q5 group-level catch weight reported by HH"
	la var hhcatch_weight "Q5 catch weight summed across household"
	
	* double check that subtotals add up to total
	egen rowtotal = rowtotal(soldfresh_iweight atefresh_iweight process_iweight lost_iweight other_iweight)
	sum catch_iweight rowtotal // only rounding errors
	drop rowtotal
	
	* NOTE: There are some pretty big outliers (e.g. hhid = 55 in January 2015; hhid = 50 in January 2015)
	* 		that probably need to be dealt with.
	list hhid year month q5_weight_bygroup hhcatch_weight catch_iweight if catch_iweight > 100, clean noobs
		/*
			hhid   year   month   q5_wei~p   hhcatc~t   catch_~t  
		  50   2015       1        300      314.5        300  
		  55   2015       1        750        850        750  
		 427   2015      11        120        120        120  
		 453   2014       3   124.9904        382   124.9904  
		*/


/*------------------------------------------------------------------------------
	Organize merged file
------------------------------------------------------------------------------*/

ren group sgroup_ccm
ren code scode_ccm
ren name sname_ccm
	replace sname_ccm = strtrim(sname_ccm)
	replace sname_ccm = strltrim(sname_ccm)
	replace sname_ccm = strrtrim(sname_ccm)

	
* remove "Ophisternon bengalense" which is an eel misclassified as a fish in the original codes
drop if scode_ccm == 77


* Generate binary variables for use breakdowns

foreach x in soldfresh atefresh process lost other {
	gen `x' = (`x'_iweight != 0 & `x'_iweight != .)
	la var `x' "Fish use: `x'"
	}

keep hhid year month sgroup scode sname duplicate soldfresh atefresh process lost other soldfresh_iweight atefresh_iweight process_iweight lost_iweight other_iweight catch_iweight //q5_weight_bygroup hhcatch_weight 
order hhid year month sgroup scode sname duplicate soldfresh atefresh process lost other soldfresh_iweight atefresh_iweight process_iweight lost_iweight other_iweight catch_iweight //q5_weight_bygroup hhcatch_weight 

save "$temp/q7all", replace


/*------------------------------------------------------------------------------
	Match households to their CFR
------------------------------------------------------------------------------*/
use "$stata_raw/Catch & Consumption_Q1_BasicInfo.dta", clear

list householdid dateinter enumeratorname cfrname interviewname village district province if householdid == "383", clean noobs
	/*
    househ~d    dateinter   enumerator~e         cfrname             interviewname   village    district    province  
         383    1/11/2013     Tes Bunthy   Trapaing Veng                Chea Leang      Lvea   Chikraeng   Siem reap  
         383    3/14/2013   Sorn Kimsang   Trapaing Veng                Chea Leang      Lvea   Chikraeng   Siem reap  
         383    5/21/2013   Sorn Kimsang   Trapaing Veng                Chea Leang      Lvea   Chikraeng   Siem reap  
         383   11/27/2012     Tes Bunthy   Trapaing Veng   Chea Leang/ Thaing Khon      Lvea   Chikraeng   Siem reap  
         383   11/28/2014     Pich Lhach        Pur Sdey              Toem Sambath    Sranal     Kralanh   Siem Reap  
	*/
	* NOTE: household ID 383 on 11/28/2014 is listed as associated with a different CFR/village/district. 
	* I suspect it was mis-labeled and could be another household, since I have no basis to fix this
	* potential error, I will drop it.
	drop if householdid == "383" & dateinter == "11/28/2014"

	
keep cfrname householdid
ren cfrname cfr
ren householdid hhid

do "$do/0_cfrid_string_to_num.do"
duplicates drop

isid hhid

destring *, replace

save "$processed/cfr_hhid_link", replace


/*------------------------------------------------------------------------------
	Merge
------------------------------------------------------------------------------*/

use "$temp/q7all", clear
merge m:1 hhid using "$processed/cfr_hhid_link"

	/*
		Result                           # of obs.
		-----------------------------------------
		not matched                             4
			from master                         0  (_merge==1)
			from using                          4  (_merge==2) <--these are households that appear in basicinfo file but no further data is collected

		matched                            26,945  (_merge==3)
		-----------------------------------------
	*/
	keep if _mer == 3
	drop _mer
	
	* dropping hhid 383 data from 2014 due to issue noted above
	drop if hhid == 383 & year == 2014 & month == 11

order cfrid, after(hhid)

/*------------------------------------------------------------------------------
	Generate timekey variables
------------------------------------------------------------------------------*/

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
	
foreach var of varlist timekey* {
	la var `var' "See timekey_mapping.xls for interpretation"
	}
	
* create a catchperiod variable that represents the catch periods as a continuous time series

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

	
save "$processed/ccm_q7.dta", replace
export delimited using "$processed/ccm_q7.csv", replace quote















