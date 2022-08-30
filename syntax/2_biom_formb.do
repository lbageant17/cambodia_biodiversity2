** Liz Bageant
** July 12, 2022

** Organizing biomonitoring data


/*------------------------------------------------------------------------------
	Organize FormBSub data (biomonitoring species caught)
------------------------------------------------------------------------------*/

use "$stata_raw//Biological Monitoring_FormBSub.dta", clear

destring *, replace

* standardize cfr names
	ren cfrname cfr
	do "$do/0_cfrid_string_to_num.do"
	tab cfrid, miss
	tab cfr, miss
	
save "$temp/formb", replace


/*------------------------------------------------------------------------------
	Clean up basic info
------------------------------------------------------------------------------*/

use "$stata_raw/Biological Monitoring_BasicInfo.dta", clear
* standardize cfr names
	ren cfrname cfr
	do "$do/0_cfrid_string_to_num.do"
	tab cfrid, miss
	tab cfr, miss

save "$temp/basicinfo", replace

/*------------------------------------------------------------------------------
	Combine FormB with BasicInfo to map occasion to months/years
------------------------------------------------------------------------------*/

use "$temp/formb", clear
tostring occasion, replace
merge m:1 occasion cfrid using "$temp/basicinfo", keepusing(date_s)
drop _mer
destring occasion, replace
replace date_s = strtrim(date_s)
gen year = substr(date_s, -4, 4)
gen month = substr(date_s, 1, 2)
	replace month = subinstr(month, "/", " ", .)
destring month year, replace

/*------------------------------------------------------------------------------
	Update codes per "species code and group reconciliation.xlsx"
------------------------------------------------------------------------------*/

gen speciesgroup = . 
do "$do/0_code_fix.do"


** Fish that are in the biomonitoring data but have no group code in CCM data
	** I have confirmed that all below are fish (and not OAA)
	replace speciesgroup = 0 if speciesname == "Crossocheilus reticulatus"
	replace speciesgroup = 0 if speciesname == "Gyrinocheilus pennocki"
	replace speciesgroup = 0 if speciesname == "Hampala dispar"
	replace speciesgroup = 0 if speciesname == "Pangasius pleurotaenia"
	replace speciesgroup = 0 if speciesname == "Toxotes microlepis"
	replace speciesgroup = 0 if speciesname == "Yasuhikotakia caudipunctata"
	replace speciesgroup = 0 if speciesname == "Epalzeorhynchos munense"
	
tab speciesgroup, miss
	* dropping all missing speciesgroup since I replaced ungrouped fish with 
	* speciesgroup = 0 in previous step
	drop if speciesgroup == . 
	
* drop all OAA 
foreach i in 87 88 89 90 91 92 93 94 95 98 115 118 120 157 160 168 178 {
	drop if speciescode == `i'
	}


/*------------------------------------------------------------------------------
	Tidy and keep relevant variables
------------------------------------------------------------------------------*/	

ren speciescode scode_biom
ren speciesgroup sgroup_biom
ren speciesname sname_biom
	replace sname_biom = strtrim(sname_biom)
ren totalweight totalweight_biom
keep cfrid year month scode_biom sgroup_biom sname_biom totalweight_biom cfr replicate
order cfrid year month scode_biom sgroup_biom sname_biom totalweight_biom cfr replicate


/*------------------------------------------------------------------------------
	Collapse by month/year, CFR, species
------------------------------------------------------------------------------*/	

collapse (sum) totalweight_biom, by(cfrid year month scode_biom sname_biom sgroup_biom)

isid cfrid year month scode

/*------------------------------------------------------------------------------
	Generate timekey variables
------------------------------------------------------------------------------*/	

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
	
foreach var of varlist timekey* {
	la var `var' "See timekey_mapping.xls for interpretation"
	}

/*------------------------------------------------------------------------------
	Save and export 
------------------------------------------------------------------------------*/

save "$processed/biom_formb.dta", replace
export delimited using "$processed/biom_formb.csv", replace quote

