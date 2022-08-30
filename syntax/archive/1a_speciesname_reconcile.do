** Reconciling WF species list with Sebastian's species list

* Load Sebastian's data
import delimited /Users/erb32/github/cambodia_biodiversity/documentation/WorldFish_Cambodia_Names_withNotes.csv, clear
cd "/Users/erb32/desktop/temp"

keep genusspecies speciesname speciescode
gen sname_seb = speciesname
ren speciesname Speciesname
gen scode_seb = speciescode
ren speciescode Speciescode

duplicates drop // 0 dropped

save seb_species,replace

* Load WF species list
import excel "/Users/erb32/github/cambodia_biodiversity/documentation/Species code and group reconciliation.xlsx", sheet("Species Name, Code-comment") cellrange(B5:D237) firstrow clear

keep if Type == "Fish"
drop Type


duplicates drop // 6 dropped


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

* Rasbora tornieri is 33 in Sebastian's data and 29 AND 33 in WF data
replace Speciesname = "Rasbora aurotaenia" if Speciesname == "Rasbora tornieri" & Speciescode == 29

duplicates tag Speciescode, gen(tag)
br if tag > 0


* This appears twice in WF data with slightly different spelling
* drop if Speciesname == "pauciperforata/Trigonopoma pauciperforatum" & Speciescode == 30


gen sname_wf = Speciesname
gen scode_wf = Speciescode


* Merge
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
ren genusspecies sname_nutrition
la var sname_nutrition "Species name in nutrition database"

save species_reconciled, replace

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


replace sname_nutrition = "Acantopsis spp" if scode == 154

save "specieslist", replace



** How does this file correspond with Biomonitoring data? 

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

** 



