Cambodia Biodiversity Paper
Liz Bageant
11/21/2022


---------------------
The analysis process for this project takes place in both R and Stata. We use the RStata package in R to call Stata scripts directly from R, so that the full analysis runs from a single master R file. 

Phase 1: 
Data prep in Stata. This step also sets up the output folder structure. 

Phase 2: 
Data analysis and visualization in R. Construction of indices and trait variables takes place here.

Phase 3: 
Summary statistics, regression analyses, Wald tests, and Shapley Own decompositions in Stata.

Phase 4: 
Visualization of regression tests and Shapley Owen decompositions in R. 
---------------------


---------------------
Understanding and using the master files will be essential to successfully replicating this analysis. 

The top level master file is _MASTER.R. This file executes the four phases described above, using RStata to run Stata files for Phases 1 and 3 and R files for Phases 2 and 4.

There is a date macro that is set in two places. This date allows the code to construct a date-specific folder with uniform underlying folder structure for explicit versioning of output (see dated folders in .../output folders for examples):
	0_initialize_macros.do
	_MASTER.R (line 36ish)
The dates entered in the above two files need to match. 
---------------------


---------------------
To run these files on your computer, you will need to do make the following one-time adjustments:

1. Ensure you have Stata and R on your computer. 
2. Update your Stata executable file path and version in _MASTER.R (lines 31-31 ish). More information here: https://www.rdocumentation.org/packages/RStata/versions/1.1.1
3. Adjust file paths for the 'umbrella' macro in the following locations:
	_PHASE_1_MASTER.do (line 17ish)
	_PHASE_3_MASTER.do (line 17ish)
	0_initialize_macros (line 15ish)
---------------------

 
