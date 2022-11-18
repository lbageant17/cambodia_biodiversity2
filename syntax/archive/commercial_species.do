** Liz Bageant
** November 17, 2022

** Identifying the 20 commercial species among our biodiversity data.

import delimited "$processed/scode.csv", clear 
gen soundex_species = soundex(sname_ccm)
save "$temp/scode", replace

import delimited "$processed/commercial_species.csv", varnames(1) clear
ren ïcommercial_species commercial_species
gen sname_ccm = commercial_species
*gen sname_biom = commercial_species
gen soundex_species = soundex(commercial_species)
save "$temp/commercial_species", replace

* try some merges
use "$temp/commercial_species", clear
merge 1:1 sname_ccm using "$temp/scode"
list sname_ccm scode if _mer == 3, clean noobs
list sname_ccm scode if _mer == 1, clean noobs


list sname_ccm scode if _mer == 3, clean noobs
/* MATCHES
                      sname_ccm   scode  
             Anabas testudineus      38  
               Barbonymus altus     142  
          Barbonymus gonionotus      15  
             Channa micropeltes     111  
                 Channa striata      43  
              Clarias batrachus      40  
      Cyclocheilichthys enoplos     159  
         Henicorhynchus lobatus      23  
       Henicorhynchus siamensis      22  
            Leptobarbus hoeveni     169  
          Oxyeleotris marmorata      78  
    Pangasianodon hypophthalmus      86  
        Thynnichthys thynnoides     128  
        Trichogaster microlepis      52  
         Trichopodus pectoralis      51 
*/
. list sname_ccm scode if _mer == 1, clean noobs
/* NOT MATCHED
                                 sname_ccm   scode  
    Anematichthys/Cyclocheilichthys apogon       9  <-- this is actually a match but doesn't merge due to odd characters in string variable
                     Boesemania microlepis       .  
                      Cirrhinus microlepis       .  
                         Pangasius djambal       .  
                       Pangasius larnaudii       .  
*/


