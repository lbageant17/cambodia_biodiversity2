** Fisheries Data transfer to stata format

** August 30 2016
** Liz Bageant

clear all
set more off

** I exported each file from Access into an excel file.  This dofile takes each
** excel file and converts it to stata. 

** Converting excel files to stata files

file open _filenames using "$excel/_filenames",read
file read _filenames line
while r(eof)==0 {
	import excel "$excel/`line'.xlsx",  firstrow case(lower) allstring clear
	saveold "$raw/`line'",replace
	file read _filenames line
	}
