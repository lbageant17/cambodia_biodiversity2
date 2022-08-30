** Liz Bageant
** June 9, 2022


** Species group, code and name comparison.

/* In this dofile I check the species groups, codes and names in the files 
	ccm_q7 and biom_formb to see whether there are inconsistencies that need
	to be reconciled.
	
	This dofile was part of an interative process where corrections were made in 
	prior dofiles (e.g. code_fix.do), so the final version demonstrates
	the lack of inconsistencies rather than demonstrates the inconsistencies. 
	
	It could be useful in detecting further inconsistencies after future code
	revisions.
*/


/*------------------------------------------------------------------------------
	Organize Q7 catch data
------------------------------------------------------------------------------*/

use "$processed/ccm_q7.dta", clear
keep scode sgroup sname
duplicates drop
count

gen code = scode
gen group = sgroup
gen name = sname

isid scode
isid sname

save "$temp/q7_names", replace

/*------------------------------------------------------------------------------
	Organize biomonitoring data
------------------------------------------------------------------------------*/
use "$processed/biom_formb.dta", clear

keep scode sgroup sname
duplicates drop
count

gen code = scode
gen group = sgroup
gen name = sname

isid scode
isid sname

save "$temp/formb", replace


/*------------------------------------------------------------------------------
	Merge catch and biomonitoring data files
------------------------------------------------------------------------------*/
merge 1:1 code using "$temp/q7_names"

	/*
		Result                           # of obs.
		-----------------------------------------
		not matched                            31
			from master                        26  (_merge==1) <-- present in CFR but never caught
			from using                          5  (_merge==2) <-- not present in CFR but sometimes caught

		matched                               125  (_merge==3)
		-----------------------------------------
	*/


/*------------------------------------------------------------------------------
	Investigate mis-matches
------------------------------------------------------------------------------*/ 

* Cases where a species is listed as caught, but not found in any CFR.
* It seems likely that these species perhaps don't reside in the CFR but are
* caught elsewhere. 
		list code name if _mer == 2, clean noobs

		/*
			code                            name  
			  73           Acantopsis thiemmedhi <-- https://en.wikipedia.org/wiki/Acantopsis. Listed as separate species. No basis to correct. 
			  80                    Pangio fusca <-- https://en.wikipedia.org/wiki/Pangio_fusca No obvious synonyms. No basis to correct.
			 114          Cyprinus carpio carpio <-- Eurasian carp. No basis to correct. 
			 144               Bagarius yarrelli <-- Not clear this species exists in the region.
			 152   Tuberoschistura cambodgiensis <-- No basis to correct
		*/

* Cases where something is present in CFR but never caught. 
		list code name if _mer == 1, clean noobs

		/* All confirmed to be fish and not mis-coded

		code                          name  
		 129            Toxotes microlepis  
		 155       Epalzeorhynchos munense  
		 156     Crossocheilus reticulatus  
		 158   Yasuhikotakia caudipunctata  
		 163        Pangasius pleurotaenia  
		 170        Gyrinocheilus pennocki  
		 172                Hampala dispar  

		*/

* Check for misspellings or synonymous species
		list sname_ccm sname_biom if sname_ccm != sname_biom, clean noobs

		/* Confirmed no misspellings or synonymous species
								sname_ccm                    sname_biom  
													 Toxotes microlepis  
												Epalzeorhynchos munense  
											  Crossocheilus reticulatus  
											Yasuhikotakia caudipunctata  
												 Pangasius pleurotaenia  
												 Gyrinocheilus pennocki  
														 Hampala dispar  
					Acantopsis thiemmedhi                                
							 Pangio fusca                                
				   Cyprinus carpio carpio                                
						Bagarius yarrelli                                
			Tuberoschistura cambodgiensis                                
		*/
		
		
/*------------------------------------------------------------------------------
	Create a master list to use for reference
------------------------------------------------------------------------------*/ 

keep group code name
order group code name
sort code

save "$processed/species_codes.dta", replace
export delimited using "$processed/species_codes.csv", replace quote


