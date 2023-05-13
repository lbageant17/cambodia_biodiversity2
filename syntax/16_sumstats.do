** Liz Bageant
** April 28, 2023


/*------- Household-level summary statistics ---------*/

use "$processed/hh_level_stata", clear
tab category, gen(cat)
la var cat1 "Reservoir for irrigation in upland area"
la var cat2 "Community pond within agricultural land--not flood prone"
la var cat3 "Community pond within agricultural land--flood prone"
la var cat4 "Demarcated area in larger water body"

la var buff "Number of buffalo owned"
la var chicken "Number of chickens owned"
la var cow "Number of cows owned"
la var duck "Number of ducks owned"
la var pig "Number of pigs owned"
la var land "Total land area owned (Ha)"
la var city_distance "Distance to nearest town (km)"

la var cfr_species "Number of species in each CFR"

global diversity cfr_species catch_species consumption_species sold_species h_cfr h_catch h_cons h_sold
global traits nd_score_cfr nd_score_catch nd_score_cons nd_score_sold body_size_cfr body_size_catch body_size_cons body_size_sold com_cfr com_catch com_cons com_sold
global hh effort hhsize depshare maxeduc index1 index2 buff chicken cow duck pig land incfish incfarm incwage incskill
global cfr city_distance cat1 cat2 cat3 cat4
	
estpost tabstat $diversity $traits $hh $cfr, statistics (p50 mean sd min max ) columns(statistics) casewise
esttab using "$output/$date/tables/sumstats.csv", cells ("p50() mean() sd() min() max() ") 		///
	nostar replace label unstack nonumber ///
	title("Summary statistics of key variables") csv

** Look at missing values
foreach var of varlist $cfr $hh $traits $diversity {
	qui: count if `var' != .
	di _newline "`var'"
	di r(N)
	}

		* HH characteristics:
			* index1 n = 410
			* index2 n = 411
			* livestock missing some values
			* only hhsize, depshare, maxeduc are complete
		* CFR characteristics and diversity indices n = 413
	
* What is the distribution of the share of species in the CFR that households catch?
	use "$processed/hh_level_stata", clear
	keep hhid catch_species cfr_species
	gen share = catch_species/cfr_species
	sum share, d
	
* What is distribution of the share of species in the CFR that households consume? 
	use "$processed/hh_level_stata", clear
	keep hhid consumption_species cfr_species
	gen share = consumption_species/cfr_species
	sum share, d // median = 0.408. Reported in paper as of 5/13/2023

* What is distribution of the share of species in the CFR that households sell? 
	use "$processed/hh_level_stata", clear
	keep hhid sold_species cfr_species
	gen share = sold_species/cfr_species
	sum share, d // mean = 0.055. Reported in paper as of 5/13/2023
	
* What is the distribution of the share of species caught that households consume? 
	use "$processed/hh_level_stata", clear
	keep hhid catch_species consumption_species
	gen share = consumption_species/catch_species
	sum share,d
	
* What is the distribution of the share of species caught that are sold? 
	use "$processed/hh_level_stata", clear
	keep hhid catch_species sold_species
	gen share = sold_species/catch_species
	sum share,d
		

	


	
	
/*------- System-level species richness ---------*/

* Note: "System" is defined here as the aggregation of all CFRs, so this will
* be a single value (not a distribution). 


* What is the total number of species in the system per the biomonitoring data?
use "$processed/biom_formb.dta", clear
keep scode_biom
duplicates drop
count // 132 species. 
save "$temp/biom_species"

* What is the total number of species in the system that are caught by households? 
	use "$processed/ccm_q7", clear
	keep scode_ccm
	duplicates drop
	count
	gen scode_actual = scode_ccm
	save "$temp/ccm_species_actually_caught"


* What is the union of biomonitoring and catch species in the system? 
use "$processed/species_codes.dta", clear
keep scode_biom scode scode_ccm
merge 1:1 scode_ccm using "$temp/ccm_species_actually_caught"
count if scode_actual != . // 130 species listed as having been caught
count if scode_actual != . & scode_biom != . // 123 species that are listed in the biomonitoring data were caught

* What is the total number of species that were sold. 
	use "$processed/ccm_q7", clear
	keep scode_ccm soldfresh
	keep if soldfresh == 1
	duplicates drop 
	count // 110


* What is the range of species richness across CFRs
use "$processed/biom_formb.dta", clear
keep cfrid scode_biom
duplicates drop
collapse (count) scode_biom, by(cfrid)
sum scode_biom, d // This information is already reported in summary table




