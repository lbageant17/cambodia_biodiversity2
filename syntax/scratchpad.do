


* looking at whether we can do a paired ttest in a regression framework with clustered SEs

	use "$processed/hh_level_stata", clear

	keep hhid cfrid *_species
	
	ttest catch_species = consumption_species, unpaired
	/*
	Paired t test
------------------------------------------------------------------------------
Variable |     Obs        Mean    Std. Err.   Std. Dev.   [95% Conf. Interval]
---------+--------------------------------------------------------------------
catch_~s |     410    23.48049    .6016771    12.18303    22.29772    24.66325
consum~s |     410     21.8878    .5543449    11.22463    20.79808    22.97753
---------+--------------------------------------------------------------------
    diff |     410    1.592683    .1353271    2.740165    1.326659    1.858706
------------------------------------------------------------------------------
     mean(diff) = mean(catch_species - consumption_sp~s)          t =  11.7691
 Ho: mean(diff) = 0                              degrees of freedom =      409

 Ha: mean(diff) < 0           Ha: mean(diff) != 0           Ha: mean(diff) > 0
 Pr(T < t) = 1.0000         Pr(|T| > |t|) = 0.0000          Pr(T > t) = 0.0000
*/

ttest catch_species = sold_species

ttest

twoway (kdensity biom_species) (kdensity catch_species)  (kdensity consumption_species) (kdensity sold_species)
	
	
	
	
	* long version
	use "$processed/hh_level_stata", clear

	keep hhid cfrid *_species
	
	
	ren biom_species species1
	ren catch_species species2
	ren consumption_species species3
	rename sold_species species4

	reshape long species, i(hhid) j(diversity_level)
	labdtch diversity_level	
	
	keep if diversity_level == 2 | diversity_level == 3
	ttest species, by(diversity_level)
	












* controls
global effort effort
global hh hhsize depshare maxeduc index1 index2
global mkt city_distance

use "$processed/hh_level_stata", clear

reg catch_species biom_species /*, vce(cluster cfrid)
	est sto base 
	predict c_hat_base
	* add effort
	reg catch_species biom_species $effort
	est sto effort
	predict c_hat_effort
	* add hh characteristics
	reg catch_species biom_species $effort $hh
	est sto hh 
	predict c_hat_hh
	* add market access
	reg catch_species biom_species $effort $hh $mkt 
	est sto mkt 
	predict c_hat_mkt
	* interact market access
	reg catch_species c.biom_species##c.city_distance $effort $hh 
	est sto mktx 
	predict c_hat_mktx















/*
use "$processed/effort",clear
collapse (sum) effort, by(hhid)
la var effort "Person-days"
merge 1:1 hhid using "$processed/hh_level_stata"
keep if _mer == 3
drop _mer
gen spue_catch = catch_species/effort
gen spue_cons = consumption_species/effort
gen spue_sold = sold_species/effort

save "$temp/USING", replace

* System x catch
cd "$temp"
	use USING, clear
	
	reg catch_species biom_species
	est sto a
	reg catch_species biom_species effort
	est sto b
	reg spue_catch biom_species
	est sto c
	
	outreg2 [a b c] using "spue_catch", replace excel label
	
	
	
	// less explanatory power in model c
	
	reg consumption_species catch_species 
	est sto a
	reg consumption_species catch_species effort
	est sto b
	reg spue_cons catch_species
	est sto c

	outreg2 [a b c] using "spue_cons", replace excel label
	
estpost tabstat spue* biom_species catch_species consumption_species effort, statistics (p50 mean sd min max cv ) columns(statistics) casewise
esttab using "$temp/spue_sumstats", cells ("p50(fmt(2)) mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2)) cv(fmt(2))") 		///
	nostar replace label unstack nonumber ///
	title("Summary statistics of key variables") csv
	
	
	hist spue_catch, saving(spue_catch)
	hist catch_species, saving(catch)
	hist effort, saving(effort)
	graph combine catch.gph effort.gph spue_catch.gph 
	
	hist spue_cons, saving(spue_cons)
	hist consumption_species, saving(cons)
	hist effort, saving(effort)
	graph combine cons.gph effort.gph spue_cons.gph
