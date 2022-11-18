* Liz Bageant
* November 18, 2022


/*----------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
Shapley-Owen regression decompositions

------------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

global effort effort effort2
global hh hhsize depshare maxeduc 
global assets index1 index2
global mkt city_distance



* Set up individual variable regression decomposition output file
tempname shapley
postfile `shapley' str40(Outcome) str40(Predictor) pred effort effort2 hhsize depshare maxeduc amenities livelihood city_distance total_r2 using "$output/shapley_$date",replace

* Set up grouped regression decomposition output file
tempname shapleyg
postfile `shapleyg' str40(Outcome) str40(Predictor) pred effort demog assets city_distance total_r2 using "$output/shapley_grouped_$date",replace


/*----------------------------------------------------------------------------*/
/* ----------- Species richness ----------------------------------------------*/
/*----------------------------------------------------------------------------*/


/*---------- Outcome: Catch species ------------------------------------------*/

use "$processed/hh_level_stata", clear

	local outcome catch_species
	local pred cfr_species

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])

/*---------- Outcome: Cons species -------------------------------------------*/

use "$processed/hh_level_stata", clear

	local outcome consumption_species
	local pred catch_species

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])


/*---------- Outcome: Sold species -------------------------------------------*/
use "$processed/hh_level_stata", clear

	local outcome sold_species
	local pred catch_species

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])

/*----------------------------------------------------------------------------*/
/* ----------- Commonness ----------------------------------------------------*/
/*----------------------------------------------------------------------------*/

/*---------- Outcome: Catch commonness ---------------------------------------*/
use "$processed/hh_level_stata", clear

	local outcome com_catch
	local pred com_cfr

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])

/*---------- Outcome: Consumption commonness ---------------------------------*/
use "$processed/hh_level_stata", clear

	local outcome com_cons
	local pred com_catch

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])


/*---------- Outcome: Sold commonness ----------------------------------------*/
use "$processed/hh_level_stata", clear

	local outcome com_sold
	local pred com_catch

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])


/*----------------------------------------------------------------------------*/
/* ----------- Body size -----------------------------------------------------*/
/*----------------------------------------------------------------------------*/

/*---------- Outcome: Catch body size ----------------------------------------*/
use "$processed/hh_level_stata", clear

	local outcome body_size_catch
	local pred body_size_cfr

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])

/*---------- Outcome: Cons body size -----------------------------------------*/
use "$processed/hh_level_stata", clear

	local outcome body_size_cons
	local pred body_size_catch

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])

/*---------- Outcome: Sold body size -----------------------------------------*/
use "$processed/hh_level_stata", clear

	local outcome body_size_sold
	local pred body_size_catch

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])


/*----------------------------------------------------------------------------*/
/* ----------- Nutrient density ----------------------------------------------*/
/*----------------------------------------------------------------------------*/

/*---------- Outcome: Catch ND -----------------------------------------------*/
use "$processed/hh_level_stata", clear

	local outcome nd_score_catch
	local pred nd_score_cfr

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])


/*---------- Outcome: Cons ND ------------------------------------------------*/
use "$processed/hh_level_stata", clear

	local outcome nd_score_cons
	local pred nd_score_catch

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])


/*---------- Outcome: Sold ND ------------------------------------------------*/
use "$processed/hh_level_stata", clear

	local outcome nd_score_sold
	local pred nd_score_catch

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])



/*----------------------------------------------------------------------------*/
/* ----------- Shannon index -------------------------------------------------*/
/*----------------------------------------------------------------------------*/

/*---------- Outcome: Catch Shannon ------------------------------------------*/
use "$processed/hh_level_stata", clear

	local outcome h_catch
	local pred h_cfr

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])

	
/*---------- Outcome: Sold Shannon -------------------------------------------*/
use "$processed/hh_level_stata", clear

	local outcome h_cons
	local pred h_catch

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])

	
/*---------- Outcome: Cons Shannon--------------------------------------------*/
use "$processed/hh_level_stata", clear

	local outcome h_sold
	local pred h_catch

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])


/*----------------------------------------------------------------------------*/
/* ----------- Simpson index -------------------------------------------------*/
/*----------------------------------------------------------------------------*/

/*---------- Outcome: Catch Simpson ------------------------------------------*/
use "$processed/hh_level_stata", clear

	local outcome d1_catch
	local pred d1_cfr

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])

/*---------- Outcome: Cons Simpson -------------------------------------------*/
use "$processed/hh_level_stata", clear

	local outcome d1_cons
	local pred d1_catch

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])

/*---------- Outcome: Sold Simpson -------------------------------------------*/
use "$processed/hh_level_stata", clear

	local outcome d1_sold
	local pred d1_catch

	qui: reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapley' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (p[6,1]) (p[7,1]) (p[8,1]) (p[9,1]) (r[1,1])
	
	reg `outcome' `pred' $effort $hh $assets $mkt, vce(cluster cfrid) 
	shapley2, stat(r2) group(`pred', $effort, $hh, $assets, $mkt)
		mat p=e(shapley_rel)
		mat r=e(r2)
	post `shapleyg' (" `outcome'") ("`pred'") (p[1,1]) (p[2,1]) (p[3,1]) (p[4,1]) (p[5,1]) (r[1,1])




	postclose `shapley'
	use "$output/shapley_$date",clear
export excel using "$output/$date/tables/variance_decomposition/variance_decomposition.xlsx", sheet("By variable") sheetmodify firstrow(variables)
	
	postclose `shapleyg'
	use "$output/shapley_grouped_$date", clear
export excel using "$output/$date/tables/variance_decomposition/variance_decomposition.xlsx", sheet("By group") sheetmodify firstrow(variables)
