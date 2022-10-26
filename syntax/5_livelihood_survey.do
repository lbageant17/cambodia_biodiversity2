** Liz Bageant
** October 26, 2022

clear all
set more off
version 15

cd "$processed/livelihood"

** Livelihood questionnaire data that might be useful. 
* This file was adapted from the Science Advances fisher behavior paper dofile.


* Household composition variables
use LH_A_pid.dta,clear
	* Household size
	bys hhid year: egen hhsize=count(pid)

	* Dependency ratio
	gen dep=1 if (age<=15 | age>=65) & age!=.
	replace dep=0 if dep==.
	gen depshare=dep/hhsize

	* Highest educational attainment?
		* requires cleaning
		replace educ=strtrim(educ)
		encode educ,gen(temp)
		numlabel,add
		tab temp,miss
		*recode temp (16=0) (1 5/9=1) (10/12 2/4=2) (13/15=3) 
		recode temp (1 = 1) (2 = 10) (3 = 11) (4 = 12) (5 = 2) (6 = 3) (7 = 4) (8 = 5) (9 = 6) (10 = 7) (11 = 8) (12 = 9) (13 = 16) (14 = 16) (15 = 14) (16 = 0)
		ren educ _temp
		ren temp educ
		tab _temp educ
		drop _temp
		tab educ,miss
	bys hhid year: egen maxeduc=max(educ)

	collapse (max) hhsize depshare maxeduc, by(hhid year)
	la var hhsize "Household size"
	la var depshare "Share of household members <=15 or >=65"
	la var maxeduc "Max educational attainment by any hh member 0=none; 1=primary; 2=sec; 3=postsec"
	destring hhid year,replace
	save "$temp/composition",replace

* Asset index of:
	* Improved housing material
	* Improved drinking water source
	* Improved cooking fuel
	* Electricity?
	* Non-shared toilet facility
use LH_B1_hhid.dta,clear
	* wall
	gen wall=1 if q5=="Concrete" | q5=="Metal" | q5=="Wood"
	replace wall=1 if q5_other=="Sipro/conc"
	count if q5==""
	replace wall=0 if q5==""
	tab q5 wall,miss
	la var wall "1=improved wall materials (concrete, metal, wood)"
	* roof
	gen roof=1 if q6=="Concrete" | q6=="Metal" | q6=="Tiles" | q6=="Wood"
	replace roof=0 if roof==. & q6!=""
	la var roof "1=improved roof materials (concrete, metal, tiles, wood)"
	* water source wet season
	encode q7,gen(temp)
	numlabel,add
	tab temp,miss
	tab q7_,miss
	gen water_wet=(temp==2 | temp==4 | temp==5 | q7_=="Tap water")
	tab water_wet,miss
	la var water_wet "1=improved water source rainy season (well, rainwater, piped water)"
	* water source dry season
	cap drop temp
	encode q8,gen(temp)
	numlabel, add force
	tab temp,miss
	tab q8_*,miss
	gen water_dry=(temp==9 | temp==10 | temp==12 | q8_other=="Pump/mixed" | q8_=="Tap water")
	tab water_dry,miss
	tab water_dry q8_*,miss
	la var water_dry "1=improved water source dry season (well, rainwater, piped water)"
	* clean cooking fuel (wrt air quality)
	tab q9_1,miss
	gen fuel=(q9_1=="Charcoal" | q9_1=="Electricit" | q9_1=="Gas" | q9_1=="Biogas")
	replace fuel=. if q9_1==""
	la var fuel "1=clean cooking fuel source (charcoal, electric, gas, biogas)"
	tab fuel q9_1,miss
	* electric
	tab q10,miss
	encode q10,gen(electric)
	numlabel electric,add
	recode electric (1=0) (2=1)
	la var electric "1=HH connected to electric grid"
	* toilet
	tab q11,miss
	encode q11,gen(toilet)
	numlabel toilet, add
	recode toilet (1=0) (2/3=1)
	la var toilet "1=HH has non-shared toilet; 0=no toilet"

	pca wall roof water_* fuel electric toilet
	predict index1 
	la var index1 "Household amenities index"
	
	keep hhid year index1
	destring hhid year,replace
	save "$temp/index1",replace


* Livelihood asset index--fishing/farming?

	* fishing assets
	use LH_B2_fishasset.dta,clear
		encode fishasset,gen(temp)
		numlabel temp,add
		tab temp,miss
		destring q13,gen(fasset)
		drop fishasset q13
		reshape wide fasset, i(hhid year) j(temp)
		keep hhid year fasset*
		save "$temp/fasset",replace

	* farming assets
	use LH_B2_farmasset.dta,clear
		encode farmasset,gen(temp)
		numlabel temp,add
		tab temp,miss
		destring q16,gen(masset)
		drop farmasset q16
		reshape wide masset, i(hhid year) j(temp)
		keep hhid year masset*
		save "$temp/masset",replace
		
	* aquaculture assets
	use LH_B2_aquaasset.dta,clear
	gen pond=q17 if aquaasset=="Pond for aquaculture"
	gen cage=q17 if aquaasset=="Cage for aquaculture"
		tab cage
		tab pond
		// these are a bit too limited to be useful
	
	* create index
	use "$temp/fasset",replace
	merge 1:1 hhid year using "$temp/masset" // perfect merge
	drop _mer
	pca masset* fasset*
	predict index2
	la var index2 "Livelihood asset index"
	keep hhid year index2
	destring hhid year,replace
	save "$temp/index2",replace
	
* Livestock (current livestock cow, goat, buffalo, pig)
use LH_D_animal.dta,clear
	drop q26_3-q26_5
	encode animal,gen(type)
	numlabel type,add
	destring q26_2 ,gen(num)
	replace num=0 if num==.
	drop q26_2 animal
	reshape wide num, i(hhid year) j(type)
	ren (num1-num7) (buff chicken cow duck goat other pig)
	foreach var of varlist buff-pig {
		*di _newline `var'
		count if `var'==0
		}
	drop goat other
	foreach var of varlist buff-pig {
		recode `var' (0=0) (1/300=1)
		tab `var'
		}	
		
	destring hhid year,replace
	
	save "$temp/livestock",replace

* Reliance on fishing for income
use LH_A_hhid,clear
	encode q3_1,gen(temp)
	numlabel temp,add
	gen incwage=(temp==10 | temp==11)
	gen incfish=(temp==4)
	gen incfarm=(temp==2 | temp==5)
	gen incskill=(temp==6 | temp==7 |temp==8 | temp==9)
	keep hhid year inc*
	la var incwage "Primary household income from wage labor"
	la var incfarm "Primary household income from farming sector (excl. wage labor)"
	la var incfish "Primary household income from fishing sector"
	la var incskill "Primary household income from skilled/salaried labor"
	destring hhid year,replace
	save "$temp/inc",replace


* Land size
use LH_C_hhid,clear
	egen land=rowtotal(q18_2 q20_2 q21_2 q22)
	sum land,d
	la var land "Total area of land: paddy, chamka, home garden, fallow (Ha)"

* Inundated flooded area
	egen flood=rowtotal(q18_3 q20_3 q21_3)
	sum flood,d
	la var flood "Total area of seasonally flooded land: paddy, chamka, fallow (Ha)"
	egen floodmonths=rowmean(q18_4 q20_4 q21_4)
	sum floodmonths,d
	la var floodmonths "Mean months flooded across all land types"
	keep hhid year land flood*
	destring hhid year,replace
	save "$temp/land",replace


** Merge all together
cd "$temp"
use composition,clear
merge 1:1 hhid year using index1
	* 4 master only
	drop _mer
merge 1:1 hhid year using index2
	* 4 master only
	drop _mer
merge 1:1 hhid year using livestock
	* 4 master only
	drop _mer
merge 1:1 hhid year using inc
	* 4 master only
	drop _mer
merge 1:1 hhid year using land
	* 4 master only
	drop _mer


save "$processed/livelihood.dta",replace

export delimited using "$processed/livelihood.csv", replace

