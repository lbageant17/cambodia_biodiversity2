
** Liz Bageant
** August 3, 2022

/*
The purpose of this file is to ensure that species names and codes match across
the three data files generated thus far: ccm_q7, biom_formb and traits.

Sebastian's species names are aligned with information available on 
specific species in FishBase and other sources. Because the traits are only 
available for certian species, some assumptions have been made and some species
have been combined into a single species for the purpose of nutrient information 
(e.g. Acantopsis spp). 

*/


* Load Sebastian species list 
import delimited "$documentation/WorldFish_Cambodia_Names_withNotes.csv", clear

	keep genusspecies speciesname speciescode
	gen sname_seb = speciesname
	ren speciesname Speciesname
	gen scode_seb = speciescode
	ren speciescode Speciescode

	duplicates drop // 0 dropped

	save "$temp/seb_species",replace

* WorldFish species list
import excel "$documentation/Species code and group reconciliation.xlsx", sheet("Species Name, Code-comment") cellrange(B5:D237) firstrow clear

	* Dropping all OAA and plants
	keep if Type == "Fish"
	drop Type

	duplicates drop // 6 dropped

	* Making some corrections based on known differences between WF species list and trait species names
	drop if Speciesname == "Anematichthys apogon" // Anematichthys apogon will be combined with Cyclocheilichthys apogon to creat Speciescode 9, per Sebastian's decision-making process
	drop if Speciesname == "Cyclocheilichthys armatus" // Cyclocheilichthys armatus is combined with Anematichthys armatus, per WorldFish notes
	drop if Speciesname == "Cyclocheilichthys repasson" & Speciescode == 10 // Cyclocheilichthys repasson will be combined with Anematichthys repasson to create speciescode 10
	drop if Speciesname == "Cirrhinus siamensis" & Speciescode == 22 // Cirrhinus siamensis will be combined with Henicorhynchus siamensis to create Speciescode 22
	drop if Speciesname == "Cirrhinus lobatus" & Speciescode == 23 // Cirrhinus lobatus will be combined with Henicorhynchus lobatus to create Speciescode 23
	drop if Speciesname == "Trichogaster microlepis" & Speciescode == 52 // Trichogaster microlepis will be combined with Trichopodus microlepis to create Speciescode 52 per Sebastian's decision-making process
	drop if Speciesname == "Trichogaster trichopterus" & Speciescode == 53 // Trichogaster trichopterus will be combined with Trichopodus trichopterus to create Speciescode 53
	drop if Speciesname == "Kryptopterus hexapterus" & Speciescode == 64 // Kryptopterus hexapterus will be combined with Micronema hexapterus to create Speciescode 64
	drop if Speciesname == "Puntius orphoides" & Speciescode == 99 // Puntius orphoides will be combined with Systomus orphoides to create Speciescode 99
	drop if Speciesname == "black sharkminnow" & Speciescode == 104 // black sharkminnow will be combined with Labeo chrysophekadion to create Speciescode 104
	drop if Speciesname == "Monotreta cambodgiensis" & Speciescode == 131 // Monotreta cambodgiensis will be combined with Tetraodon cambodgiensis to create Speciescode 131

	* Rasbora tornieri is 33 in trait data and 29 AND 33 in WorldFish data. Making it 29 only.
	replace Speciesname = "Rasbora aurotaenia" if Speciesname == "Rasbora tornieri" & Speciescode == 29

	duplicates tag Speciescode, gen(tag)
	*br if tag > 0
	drop tag

	gen sname_wf = Speciesname
	gen scode_wf = Speciescode


* Merge the WorldFish files with Sebastian's trait list
	merge 1:1 Speciescode using seb_species

	list Speciescode if _mer == 1, clean noobs
		/* 
		All of these do not appear in Sebastian's data files
		Speci~de  
			 130  
			 136  
			 154  
		*/

	list sname_seb if _mer == 2, clean noobs
		/*
							sname_seb  
		Rasbora tornieri / aurotaenia  <--there is no species code for this in Sebastian's data. We will drop.
		*/
		
	keep if _mer != 2
	drop _mer 

	keep Speciescode genusspecies
	ren Speciescode scode
	ren genusspecies sname_traits // This is the name of the species that will link WorldFish codes to the traits. 
	la var sname_traits "Species name in traits database"

save "$temp/species_reconciled", replace


** Checking to see whether these align with what was generated in the CCM data cleaning process. 

use "$processed/ccm_q7.dta", clear

	keep sname scode
	duplicates drop

	gen scode = scode_ccm

	merge 1:1 scode using species_reconciled

	/*

		Result                           # of obs.
		-----------------------------------------
		not matched                             1
			from master                         0  (_merge==1)
			from using                          1  (_merge==2) <- this is one case where there is a code for something that doesn't appear in the data

		matched                               130  (_merge==3)
		-----------------------------------------
	*/

	drop _mer


	replace sname_traits = "Acantopsis spp" if scode == 154

save "$temp/specieslist", replace


** How do these align with Biomonitoring data? 

use "$processed/biom_formb.dta", clear

	keep scode_biom sname_biom
	duplicates drop

	gen scode = scode_biom

	merge 1:1 scode using specieslist

	/*
		Result                           # of obs.
		-----------------------------------------
		not matched                            13
			from master                         7  (_merge==1) <-- these are all cases that are not present in ccm or nutrition data
			from using                          6  (_merge==2) <-- these are all cases that are present in nutrition or CCM data, but not biomonitoring

		matched                               125  (_merge==3)
		-----------------------------------------
	*/
	drop _mer
	
	
** How do these align with traits data? 

count if sname_traits == "" // 9 cases
drop if sname_traits == ""

merge m:1 sname_traits using "$processed/traits.dta"

list scode* sname* if _mer == 2, clean noobs

/*
    Result                           # of obs.
    -----------------------------------------
    not matched                             1
        from master                         0  (_merge==1)
        from using                          1  (_merge==2)

    matched                               129  (_merge==3)
    -----------------------------------------
*/

keep if _mer == 3
drop _mer

** Generate a data file that we can use for reference and merging with ccm_q7, biom_formb and traits
keep sname* scode*

save "$processed/species_codes.dta", replace
export delimited using "$processed/species_codes.csv", replace quote




