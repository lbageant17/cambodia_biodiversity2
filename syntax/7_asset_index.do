** Liz Bageant
** October 31, 2022

** Asset index construction (taken directly from 6a_livelihood_survey.do file from Science Advances fisher behavior paper)

clear all 
set more off
version 15

cd "$processed"

/*------- Housing amenities asset index --------------------------------------*/
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
	keep if year == 2012
	drop year
	
	drop if index == . // 1 obs dropped. hhid 566 is missing amenities index.
	
	save "$component/index1",replace


/*------- Livelihood asset index ---------------------------------------------*/

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
	keep if year == 2012
	save "$component/index2",replace

