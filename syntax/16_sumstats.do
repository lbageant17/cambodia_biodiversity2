** Liz Bageant
** October 27, 2022

** Generate some basic summary tables



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

global diversity cfr_species catch_species consumption_species sold_species h_biom_cfr_level h_catch h_cons h_sold
global traits nd_score_cfr nd_score_catch nd_score_cons nd_score_sold body_size_cfr body_size_catch body_size_cons body_size_sold com_cfr com_catch com_cons com_sold
global hh effort hhsize depshare maxeduc index1 index2 buff chicken cow duck pig land incfish incfarm incwage incskill
global cfr city_distance cat1 cat2 cat3 cat4
	
estpost tabstat $diversity $traits $hh $cfr, statistics (p50 mean sd min max ) columns(statistics) casewise
esttab using "$output/$date/tables/sumstats.csv", cells ("p50(fmt(2)) mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2)) ") 		///
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
	
