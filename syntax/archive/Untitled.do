** Liz Bageant
** June 7, 2022

** Initial organization of data for Cambodia Biodiversity paper


* Macros
global ccm_processed "/Users/erb32/Library/CloudStorage/Box-Box/Rice Field Fisheries Project/Biodiversity Paper/data/survey data september 2016/processed"
global ccm_raw "/Users/erb32/Library/CloudStorage/Box-Box/Rice Field Fisheries Project/Biodiversity Paper/data/survey data september 2016/excel raw"

** There are two Q7 data files that we could start with. One called "no duplicates"
** but after investigation below, it is not clear what this means. 

** I cannot find dofiles that generate either of these files from the raw files.
	* "$ccm_processed/Catch & Consumption_Q7andQ7sub.dta"
	* "$ccm_processed/processed_CCM_Q7_Q8_No_Duplicates.dta"

	use "$ccm_processed/Catch & Consumption_Q7andQ7sub.dta", clear

	count // 27,034

	tab year, miss

	duplicates report
	duplicates report hhid year month speciesgroup speciescode
	duplicates report hhid year month speciescode


	use "$ccm_processed/processed_CCM_Q7_Q8_No_Duplicates.dta", clear

	count // 30,820

	tab year, miss

	duplicates report
	duplicates report hhid year month speciesgroup speciescode
	duplicates report hhid year month speciescode
