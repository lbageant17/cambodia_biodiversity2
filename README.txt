Cambodia Biodiversity Paper

The analysis process for this project takes place in both R and Stata. 

Phase 1: 
Data prep in Stata. 

Phase 2: 
Data analysis and visualization in R. 

Phase 3: 
Summary statistics and various regression analyses in Stata.

Phase 4: 
Visualization of regression tests in R. 

Understanding and using the master files will be essential to successfully replicating this analysis. 

The top level master file is _MASTER.R. This file executes the four phases described above, calling Stata files for Phases 1 and 3 and R files for Phases 2 and 4.


Not quite ideal:
Date macro is automatic in stata but manually entered in R file paths. This might be fixable. 

Stata macros need repeating in second master file because each time R calls a master file it opens a new stata window and closes it when done (closing it deletes all macros). Could solve by creating a single stata macro file (separate from master files) that is called each time R wants to run stata file. 


