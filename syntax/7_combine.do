** Liz Bageant
** October 27, 2022

clear all
set more off
version 15


/*------------------------------------------------------------------------------
	This file creates stata equivalents of files in R for analysis in stata. 
	In some cases it re-creates them.
	In other cases it uses files generated in R (e.g. diet quality)
------------------------------------------------------------------------------*/


/* -------- Species count data -----------------------------------------------*/

* CFR level biodiversity in terms of species counts (cfr_biom in R)
use "$processed/biom_formb.dta",clear
keep cfrid scode_biom
duplicates drop 
collapse (count) scode_biom, by (cfrid)
ren scode_biom biom_species
la var biom_species "Number of species in system (biom)"
save "$component/cfr_biom", replace

* Household level catch biodiversity in terms of species counts (hh_catch in R)
use "$processed/ccm_q7.dta", clear
count if catch_iweight== 0 // check. all ok.
keep hhid cfrid scode_ccm
duplicates drop
collapse (count) scode_ccm, by(hhid)
ren scode_ccm catch_species
la var catch_species "Number of species caught by HH (ccm)"
save "$component/hh_catch", replace

* Household level consumption and sale biodiveristy in terms of species counts (hh_soldcons in R)
use "$processed/ccm_q7.dta", clear
keep hhid cfrid scode_ccm atefresh
keep if atefresh == 1
duplicates drop
count // 9032
collapse (count) scode_ccm, by(hhid)
ren scode_ccm consumption_species
la var consumption_species "Number of species consumed by HH (ccm)"
save "$component/hh_cons", replace

use "$processed/ccm_q7.dta", clear
keep hhid cfrid scode_ccm soldfresh
keep if soldfresh == 1
duplicates drop
count // 1879
collapse (count) scode_ccm, by(hhid)
ren scode_ccm sold_species
la var sold_species "Number of species sold by HH (ccm)"
save "$component/hh_sold", replace

merge 1:1 hhid using "$component/hh_cons"
recode sold_species (. = 0) // these are cases where a household did not sell anything over the period
drop _mer
save "$component/hh_soldcons", replace

* Combine above files to create the equivalent of cfr_biodiv R file (name is misleading--file is at HH level)
cd "$component"

use hh_soldcons, clear
merge 1:1 hhid using hh_catch
drop _mer
merge 1:1 hhid using "$processed/cfr_hhid_link" // bringing in cfr codes
keep if _mer == 3 // 413 hh
drop _mer
merge m:1 cfrid using cfr_biom
drop _mer
order hhid cfrid biom catch cons sold
save "$component/cfr_biodiv", replace


/* -------- CFR and household characteristics data ---------------------------*/

* Add CFR characteristics (type and distance to city) and household characteristics from livelihood data. This file has no equivalent in core R data files.
use "$component/cfr_biodiv", clear
drop cfrid
do "$do/0_cfrid_string_to_num.do"
tostring cfrid, replace
*merge m:1 cfrid using "$processed/city_distance"
merge m:1 cfrid using "$processed/cfr_category"
drop _mer
destring cfrid, replace
gen year = 2012 // livelihood data has 2012 and 2015. This facilitates the merge.
merge 1:1 hhid year using "$processed/livelihood"
keep if _mer == 3
drop year _mer
la var city_distance "Distance to nearest large town (km)--Battambang, Kampong Thom, Pursat, Siem Reap"
la def category 1 "Reservoir for irrigation in upland area" 2 "Community pond within agricultural land--not flood prone" 3 "Community pond within agricultural land--flood prone" 4 "Demarcated area in larger water body"
la val category category
la var category "CFR type"
save "$temp/hh", replace

/* -------- Biodiversity indices data ----------------------------------------*/

* CFR and household level diversity indices were generated in R
import delimited "$processed/diversity_indices_cfr_level.csv", clear 
drop v1
foreach var of varlist h_* d1_* d2_* {
	rename `var' `var'_cfr_level
	}
la var h_biom "Shannon index--system CFR level"
la var d1_biom "Simpson index--system CFR level"
la var d2_biom "Inverse simpson index--system CFR level"
la var h_catch "Shannon index--catch CFR level"
la var d1_catch "Simpson index--catch CFR level"
la var d2_catch "Inverse simpson index--catch CFR level"
la var h_cons "Shannon index--consumption CFR level"
la var d1_cons "Simpson index--consumption CFR level"
la var d2_cons "Inverse simpson index--consumption CFR level"
la var h_sold "Shannon index--sold CFR level"
la var d1_sold "Simpson index--sold CFR level"
la var d2_sold "Inverse simpson index--sold CFR level"
	
save "$component/cfr_shannon_index", replace 


import delimited "$processed/diversity_indices_hh_level.csv", clear 
drop v1
la var h_catch "Shannon index--catch"
la var d1_catch "Simpson index--catch"
la var d2_catch "Inverse simpson index--catch"
la var h_cons "Shannon index--consumption"
la var d1_cons "Simpson index--consumption"
la var d2_cons "Inverse simpson index--consumption"
la var h_sold "Shannon index--sold"
la var d1_sold "Simpson index--sold"
la var d2_sold "Inverse simpson index--sold"

save "$component/hh_shannon_index", replace


/* -------- Diet quality metrics ---------------------------------------------*/

* Diet quality metrics were generated in R
import delimited "$processed/diet_quality_hh_level.csv", clear
drop v1
replace minbio_hh_sold_all = "." if minbio_hh_sold_all == "Inf"
destring minbio_hh_sold_all, replace
la var rda_hh_catch "RDAs met by 100g of a given HH catch portfolio (RDA thresh set in R)"
la var rda_hh_cons "RDAs met by 100g of a given HH cons portfolio (RDA thresh set in R)"
la var rda_hh_sold "RDAs met by 100g of a given HH sold portfolio (RDA thresh set in R)"
la var minbio_hh_catch "Minimum portion size of HH catch portfolio that meets all RDAs (RDA thresh set in R)"
la var minbio_hh_cons "Minimum portion size of HH cons portfolio that meets all RDAs  (RDA thresh set in R)"
la var minbio_hh_sold "Minimum portion size of HH sold portfolio that meets all RDAs  (RDA thresh set in R)"
save "$component/diet_quality", replace


/* -------- Combine all HH level data files ----------------------------------*/

use "$temp/hh", clear
merge 1:1 hhid using "$component/hh_shannon_index"
drop _mer
merge m:1 cfrid using "$component/cfr_shannon_index"
drop _mer
mer 1:1 hhid using "$component/diet_quality"
drop _mer
order hhid cfrid *_species h_* rda_* minbio_* d1_* d2_* 

save "$processed/hh_level_stata", replace



