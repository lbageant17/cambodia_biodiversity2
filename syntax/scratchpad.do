

use "$processed/effort",clear
collapse (sum) effort, by(hhid)
la var effort "Person-days"
merge 1:1 hhid using "$processed/hh_level_stata"
keep if _mer == 3
drop _mer
gen spue_catch = catch_species/effort
gen spue_cons = consumption_species/effort
gen spue_sold = sold_species/effort

save "$temp/USING", replace

* System x catch
cd "$temp"
	use USING, clear
	
	reg catch_species biom_species
	est sto a
	reg catch_species biom_species effort
	est sto b
	reg spue_catch biom_species
	est sto c
	
	outreg2 [a b c] using "spue_catch", replace excel label
	
	
	
	// less explanatory power in model c
	
	reg consumption_species catch_species 
	est sto a
	reg consumption_species catch_species effort
	est sto b
	reg spue_cons catch_species
	est sto c

	outreg2 [a b c] using "spue_cons", replace excel label
	
estpost tabstat spue* biom_species catch_species consumption_species effort, statistics (p50 mean sd min max cv ) columns(statistics) casewise
esttab using "$temp/spue_sumstats", cells ("p50(fmt(2)) mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2)) cv(fmt(2))") 		///
	nostar replace label unstack nonumber ///
	title("Summary statistics of key variables") csv
	
	
	hist spue_catch, saving(spue_catch)
	hist catch_species, saving(catch)
	hist effort, saving(effort)
	graph combine catch.gph effort.gph spue_catch.gph 
	
	hist spue_cons, saving(spue_cons)
	hist consumption_species, saving(cons)
	hist effort, saving(effort)
	graph combine cons.gph effort.gph spue_cons.gph
