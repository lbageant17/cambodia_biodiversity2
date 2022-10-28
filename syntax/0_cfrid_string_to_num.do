** October 8 2017
** Liz Bageant


** Simple module to create consistent CFR codes from string CFR variable.  Can be adjusted as needed.
cap gen cfrid=.
la var cfrid "CFR ID"
replace cfrid=1 if cfr=="Ang Chork"
replace cfrid=2 if cfr=="Anlous Dong"
replace cfrid=2 if cfr=="Anlos Dong" // note alternate spelling
replace cfrid=2 if cfr=="Anluos Dong" // note alternate spelling
replace cfrid=3 if cfr=="Aren"
replace cfrid=4 if cfr=="Bakong"
replace cfrid=5 if cfr=="Boeng Chheutrav"
replace cfrid=6 if cfr=="Boeng Daiphtaul"
replace cfrid=6 if cfr=="Boeng Daiptol" // note alternate spelling
replace cfrid=7 if cfr=="Boeng Kamhengsa"
replace cfrid=7 if cfr=="B. Kemhengsa" // note abbreviated spelling
replace cfrid=8 if cfr=="Boeng Kampeng"
replace cfrid=8 if cfr=="Boeng Kompeng" // note alternate spelling
replace cfrid=9 if cfr=="Boeng Kantout"
replace cfrid=9 if cfr=="Boeng Kantuot" // note alternate spelling
replace cfrid=10 if cfr=="Boeng Krong"
replace cfrid=10 if cfr=="Boeng krong" // note alternate spelling (capitalization)
replace cfrid=11 if cfr=="Boeng Prahauch"
replace cfrid=11 if cfr=="B. Prahauch" // note abbreviated spelling
replace cfrid=12 if cfr=="Boeng Prang"
replace cfrid=13 if cfr=="Boeng Preah Ponley" | cfr == "Boeng Preah"
replace cfrid=14 if cfr=="Boeng Rolum"
replace cfrid=14 if cfr=="B. Rolum"  // note abbreviated spelling
replace cfrid=15 if cfr=="Boeng Romlech"
replace cfrid=16 if cfr=="Boeng Thea"
replace cfrid=16 if cfr=="B. Thea" // note abbreviated spelling
replace cfrid=16 if cfr=="Boeng Thear" // note abbreviated spelling
replace cfrid=17 if cfr=="Boeng Thmor Koul" | cfr == "Boeng Thmor koul" | cfr == "Boeng Thmor"
replace cfrid=17 if cfr=="B. Thmorkoul"  // note abbreviation/alternate spelling
replace cfrid=18 if cfr=="Boeng Tramper"
replace cfrid=18 if cfr=="Boeng  Tramper" // note alternate spelling (space)
replace cfrid=18 if cfr=="Boeng Trampe" // note alternate spelling 
replace cfrid=19 if cfr=="Boeng Tramses"
replace cfrid=20 if cfr=="Damnak Kranh"
replace cfrid=20 if cfr=="Dom Nakragn" // note alternate spelling
replace cfrid=20 if cfr=="Dam0k Kranh" // note alternate spelling
replace cfrid=21 if cfr=="Entark Komar" | cfr=="Entark komar"
replace cfrid=21 if cfr=="Antark Komar" // note alternate spelling
replace cfrid=22 if cfr=="Kork Lhong"
replace cfrid=22 if cfr=="K. Lhong" // note abbreviation
replace cfrid=23 if cfr=="Krasaing Rithy"
replace cfrid=24 if cfr=="Kuch Noub"
replace cfrid=24 if cfr=="Kuch Nub" // note alternate spelling
replace cfrid=25 if cfr=="Lboeuk Keteyuos" | cfr== "Lboeuk keteyuos"
replace cfrid=25 if cfr=="L. Keteyuos" // note abbreviation
replace cfrid=25 if cfr=="L. Keteyous" // note abbreviation
replace cfrid=25 if cfr=="Lboeuk keteyuos" // note alternate spelling (capitalization)
replace cfrid=26 if cfr=="Obosmkak"
replace cfrid=26 if cfr=="Obosmakak" // note alternate spelling
replace cfrid=27 if cfr=="Otaky"
replace cfrid=27 if cfr=="Otaki"  // note alternate spelling
replace cfrid=28 if cfr=="Otamoan"
replace cfrid=29 if cfr=="Othom Sranal" | cfr== "Othom sranal"
replace cfrid=29 if cfr=="O. Sranal" // note abbreviation
replace cfrid=29 if cfr=="Othom Sra0l" // note abbreviation
replace cfrid=30 if cfr=="Preah Neang Korl" | cfr== "Preah Neang korl"
replace cfrid=30 if cfr=="P. Neangkorl" // abbreviation/alternate spelling
replace cfrid=30 if cfr=="Preah Neangkorl" // abbreviation/alternate spelling
replace cfrid=31 if cfr=="Pur Sdey" | cfr== "Pur sdey"
replace cfrid=31 if cfr=="Purdsey" // note alternate spelling
replace cfrid=31 if cfr=="Pursdey" // note alternate spelling
replace cfrid=31 if cfr=="Pusdey" // note alternate spelling
replace cfrid=32 if cfr=="Sla Slak"
replace cfrid=32 if cfr=="Slar Slak" // note alternate spelling
replace cfrid=32 if cfr=="Sla Slak " // note additional space
replace cfrid=33 if cfr=="Trapaing Kuy" | cfr== "Trapaing kuy"
replace cfrid=33 if cfr=="T. Kuy" // note abbreviation
replace cfrid=34 if cfr=="Trapaing Neang Noy"
replace cfrid=34 if cfr=="Trapaing Neangnoy" // note alternate spelling
replace cfrid=34 if cfr=="Trapaing NeangNoy" // note alternate spelling
replace cfrid=34 if cfr=="T. Neangnoy" // note abbreviation/alternate spelling
replace cfrid=35 if cfr=="Trapaing Thlok Meanchey"
replace cfrid=35 if cfr=="T. Thlokmeanchey" // note abbreviation/alternate spelling
replace cfrid=35 if cfr=="Trapaing Thlokmeanchey" // note abbreviation/alternate spelling
replace cfrid=36 if cfr=="Trapaing Thlong"
replace cfrid=36 if cfr=="T. Thlong" // note abbreviation
replace cfrid=37 if cfr=="Trapaing Veng" | cfr== "Trapaing veng"
replace cfrid=37 if cfr=="T. panveng" // note alternate spelling/abbreviation
replace cfrid=38 if cfr=="Tumnub Kandole" | cfr== "Tumnub kandole"
replace cfrid=38 if cfr=="T. Kandole" // note abbreviation
replace cfrid=39 if cfr=="Tumnub Mkak"
replace cfrid=39 if cfr=="T. Mkak" // note abbreviation
replace cfrid=40 if cfr=="Tumnub Rumdeng" | cfr=="Tumnub rumdeng"
replace cfrid=40 if cfr=="T. Rumdeng" // note abbreviation
/*cap la def cfr 1 "Ang Chork" 2 "Anlous Dong"  3 "Aren" 4 "Bakong" ///
	5 "Boeng Chheutrav" 6 "Boeng Daiphtaul" 7 "Boeng Kamhengsa" 8 "Boeng Kampeng" ///
	9 "Boeng Kantout" 10 "Boeng Krong" 11 "Boeng Prahauch" 12 "Boeng Prang" ///
	13 "Boeng Preah Ponley" 14 "Boeng Rolum" 15 "Boeng Romlech" 16 "Boeng Thea" ///
	17 "Boeng Thmor Koul" 18 "Boeng Tramper" 19 "Boeng Tramses" 20 "Damnak Kranh" ///
	21 "Entark Komar" 22 "Kork Lhong" 23 "Krasaing Rithy" 24 "Kuch Noub" ///
	25 "Lboeuk Keteyuos" 26 "Obosmkak" 27 "Otaky" 28 "Otamoan" 29 "Othom Sranal" ///
	30 "Preah Neang Korl" 31 "Pur Sdey" 32 "Sla Slak" 33 "Trapaing Kuy" ///
	34 "Trapaing Neang Noy"  35 "Trapaing Thlok Meanchey" ///
	36 "Trapaing Thlong" 37 "Trapaing Veng" 38 "Tumnub Kandole" 39 "Tumnub Mkak" 40 "Tumnub Rumdeng" 
la val cfrid cfr
numlabel,add*/

* generate consistent names for "cfr" variable
replace cfr = "Ang Chork" if cfrid == 1
replace cfr = "Anlous Dong" if cfrid == 2
replace cfr = "Aren " if cfrid == 3
replace cfr = "Bakong " if cfrid == 4
replace cfr = "Boeng Chheutrav" if cfrid == 5
replace cfr = "Boeng Daiphtaul" if cfrid == 6
replace cfr = "Boeng Kamhengsa" if cfrid == 7
replace cfr = "Boeng Kampeng" if cfrid == 8
replace cfr = "Boeng Kantout" if cfrid == 9
replace cfr = "Boeng Krong" if cfrid == 10
replace cfr = "Boeng Prahauch" if cfrid == 11
replace cfr = "Boeng Prang" if cfrid == 12
replace cfr = "Boeng Preah" if cfrid == 13
replace cfr = "Boeng Rolum" if cfrid == 14
replace cfr = "Boeng Romlech" if cfrid == 15
replace cfr = "Boeng Thea" if cfrid == 16
replace cfr = "Boeng Thmor" if cfrid == 17
replace cfr = "Boeng Tramper" if cfrid == 18
replace cfr = "Boeng Tramses" if cfrid == 19
replace cfr = "Damnak Kranh" if cfrid == 20
replace cfr = "Entark Komar" if cfrid == 21
replace cfr = "Kork Lhong" if cfrid == 22
replace cfr = "Krasaing Rithy" if cfrid == 23
replace cfr = "Kuch Noub" if cfrid == 24
replace cfr = "Lboeuk Keteyuos" if cfrid == 25
replace cfr = "Obosmkak " if cfrid == 26
replace cfr = "Otaky " if cfrid == 27
replace cfr = "Otamoan " if cfrid == 28
replace cfr = "Othom Sranal" if cfrid == 29
replace cfr = "Preah Neang Korl" if cfrid == 30
replace cfr = "Pur Sdey" if cfrid == 31
replace cfr = "Sla Slak" if cfrid == 32
replace cfr = "Trapaing Kuy" if cfrid == 33
replace cfr = "Trapaing Neang Noy" if cfrid == 34
replace cfr = "Trapaing Thlok Meanchey" if cfrid == 35
replace cfr = "Trapaing Thlong" if cfrid == 36
replace cfr = "Trapaing Veng" if cfrid == 37
replace cfr = "Tumnub Kandole" if cfrid == 38
replace cfr = "Tumnub Mkak" if cfrid == 39
replace cfr = "Tumnub Rumdeng" if cfrid == 40

replace cfr = strtrim(cfr)
