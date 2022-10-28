** Liz Bageant
** October 26, 2022


/* CFR CHARACTERISTICS */

** 1. Distance to nearest large city or town (proxy for market access).

** 2. CFR category


/*
DISTANCE
The Brochure data has incomplete distances to towns/cities. This file merges the
Brochure file with the GPS coordinates. 
Four cities: Kampong Thom, Pursat, Siem Reap and Battambang
*/

import excel "$brochure/Brochure_20221026.xlsx", sheet("CFR and Region") cellrange(A1:F41) firstrow clear
ren CFR cfr
ren DistancetoRoad distance_to_road

do "$do/0_cfrid_string_to_num.do"

save "$temp/temp", replace

import delimited "$brochure/cfr_coordinates.csv", clear 
ren v3 lon
merge 1:1 cfrid using "$temp/temp"

keep cfrid lat lon cfr distance_to_city city distance_to_road

export delimited using "$brochure/export.csv", replace 

** Here I google distances to four cities and re-save export.csv as import.csv ***

import delimited using "$brochure/import.csv", clear

/*
Notes:
The distances in the original distance_to_city_km variable are nonsensical. 
We are assuming the GPS coordinates are correct. 
*/

egen city_distance = rowmin(dist_to_sr dist_to_kt dist_to_pur dist_to_bat)

keep cfrid city_distance
tostring cfrid, replace

save "$temp/temp", replace

/*
CATEGORY */

import delimited using "$processed/cfr_category.csv", clear

* CFR is a string. Need to parse CFR ID from string. 

gen id = substr(cfrid, 1, 2)
gen id2 = subinstr(id, ".", "", .)
gen cfrid_num = id2
keep cfrid category 
merge 1:1 cfrid cfrid_num using "$temp/temp"
drop _mer



save "$processed/cfr_category", replace
export delimited using "$processed/cfr_category.csv", replace

