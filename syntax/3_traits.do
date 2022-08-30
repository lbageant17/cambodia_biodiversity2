** Liz Bageant
** August 3, 2022

** This file imports and organizes fish trait data, including bodysize, trophic level and nutrient content
** Further details on this dataset can be found in traits_by_species_Readme.txt

import delimited "$traits/traits_by_species_20220802.csv", clear 

drop longmig // this variable is all missing
drop community
drop total consumed sold numday *_daily // these we will reconstruct using CCM data

ren species sname_traits // There are some differences in names/spellings between the traits data and the CCM/biom data. 

isid sname_traits

count if sname_traits == ""

* this data file does not have species code, so merges will have to be done on string values.

save "$processed/traits", replace
export delimited using "$processed/traits.csv", replace quote


