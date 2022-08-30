* This file corrects speciescode speciesgroup and speciesname variables
* following "Species code and group reconciliation.xlsx" 

	replace speciesgroup =  1 if speciescode == 1
	replace speciesgroup =  1 if speciescode == 2
	replace speciesgroup =  1 if speciescode == 3
	replace speciesgroup =  1 if speciescode == 4
	replace speciesgroup =  1 if speciescode == 5
	replace speciesgroup =  1 if speciescode == 6
	replace speciesgroup =  1 if speciescode == 7
	replace speciesgroup =  2 if speciescode == 8
	replace speciesgroup =  3 if speciescode == 9
	replace speciesgroup =  3 if speciescode == 9
	replace speciesgroup =  3 if speciescode == 10
	replace speciesname = "Anematichthys repasson" if speciesname == "Cyclocheilichthys repasson"
	replace speciesgroup =  3 if speciescode == 11
	replace speciesname = "Anematichthys armatus" if speciesname == "Cyclocheilichthys armatus"
	replace speciesgroup =  3 if speciescode == 12
	replace speciesgroup =  4 if speciescode == 13
	replace speciesgroup =  4 if speciescode == 14
	replace speciesgroup =  4 if speciescode == 15
	replace speciesgroup =  5 if speciescode == 16
	replace speciesgroup =  5 if speciescode == 17
	replace speciesgroup =  5 if speciescode == 18
	replace speciesgroup =  5 if speciescode == 19
	replace speciesgroup =  6 if speciescode == 20
	replace speciesgroup =  6 if speciescode == 21
	replace speciesgroup =  8 if speciescode == 22
	replace speciesgroup =  8 if speciescode == 22
	replace speciesgroup =  8 if speciescode == 23
	replace speciesgroup =  8 if speciescode == 23
	replace speciesgroup =  9 if speciescode == 24
	replace speciesgroup =  9 if speciescode == 25
	replace speciesgroup =  9 if speciescode == 26
	replace speciesgroup =  9 if speciescode == 27
	replace speciesgroup =  9 if speciescode == 28
	replace speciesname = "Rasbora aurotaenia" if speciescode == 29
	replace speciesgroup = 9 if speciescode == 29
	replace speciesgroup =  9 if speciescode == 30
	replace speciesgroup =  9 if speciescode == 31
	replace speciesgroup =  9 if speciescode == 32
	replace speciesgroup =  9 if speciescode == 33
	replace speciesgroup =  9 if speciescode == 34
	replace speciesgroup = 10 if speciescode == 36
	replace speciesgroup = 11 if speciescode == 37
	replace speciesgroup = 12 if speciescode == 38

	replace speciesgroup = 11 if speciescode == 39
	replace speciesgroup = 13 if speciescode == 40
	replace speciesgroup = 13 if speciescode == 41
	replace speciesgroup = 13 if speciescode == 42

	replace speciesgroup = 14 if speciescode == 43

	replace speciesgroup = 15 if speciescode == 44
	replace speciesgroup = 15 if speciescode == 45
	replace speciesgroup = 15 if speciescode == 46
	replace speciesgroup = 15 if speciescode == 47
	replace speciesgroup = 16 if speciescode == 48
	replace speciesgroup = 16 if speciescode == 49
	replace speciesgroup = 16 if speciescode == 50
	replace speciesgroup = 17 if speciescode == 51
	replace speciesgroup = 17 if speciescode == 52
	replace speciesname = "Trichogaster microlepis" if speciescode == 52
	replace speciesgroup = 17 if speciescode == 52
	replace speciesname = "Trichopodus trichopterus" if speciescode == 53
	replace speciesgroup = 17 if speciescode == 53

	replace speciesgroup =  8 if speciescode == 54
	replace speciesgroup = 10 if speciescode == 55
	replace speciesgroup =  9 if speciescode == 55
	replace speciesgroup = 18 if speciescode == 56
	replace speciesgroup = 18 if speciescode == 57
	replace speciesgroup = 18 if speciescode == 58
	replace speciesgroup = 19 if speciescode == 59
	replace speciesgroup = 19 if speciescode == 60
	replace speciesgroup = 19 if speciescode == 61
	replace speciesgroup = 20 if speciescode == 62
	replace speciesgroup = 21 if speciescode == 63
	replace speciesname = "Micronema hexapterus" if speciescode == 64
	replace speciesgroup = 22 if speciescode == 64
	replace speciesgroup = 22 if speciescode == 64
	replace speciesgroup = 22 if speciescode == 65
	replace speciesgroup = 22 if speciescode == 66
	replace speciesgroup = 23 if speciescode == 67
	replace speciesgroup = 25 if speciescode == 68
	replace speciesgroup = 25 if speciescode == 69
	replace speciesgroup = 26 if speciescode == 70
	replace speciesgroup = 26 if speciescode == 71
	replace speciesgroup = 27 if speciescode == 72
	replace speciesgroup = 27 if speciescode == 73
	replace speciesgroup = 27 if speciescode == 74
	replace speciesgroup = 27 if speciescode == 75
	replace speciesgroup = 28 if speciescode == 76
	replace speciesgroup = 28 if speciescode == 77
	replace speciesgroup = 28 if speciescode == 77
	replace speciesgroup = 29 if speciescode == 78
	replace speciesgroup = 30 if speciescode == 79
	replace speciesgroup = 31 if speciescode == 80
	replace speciesgroup = 32 if speciescode == 81
	replace speciesgroup = 32 if speciescode == 82
	replace speciesgroup = 32 if speciescode == 83
	replace speciesgroup = 32 if speciescode == 84
	replace speciesgroup = 32 if speciescode == 85
	replace speciesgroup = 33 if speciescode == 86
	replace speciesgroup = 34 if speciescode == 99
	replace speciesgroup = 34 if speciescode == 99
	replace speciesgroup =  4 if speciescode == 100
	replace speciesgroup = 34 if speciescode == 101
	replace speciesgroup = 34 if speciescode == 102
	replace speciesgroup = 35 if speciescode == 103
	replace speciesgroup = 36 if speciescode == 104
	replace speciesname = "Labeo chrysophekadion" if speciescode == 104
	replace speciesgroup = 37 if speciescode == 105
	replace speciesgroup = 10 if speciescode == 106
	replace speciesgroup = 38 if speciescode == 107
	replace speciesgroup = 38 if speciescode == 108
	replace speciesgroup = 39 if speciescode == 109
	replace speciesgroup = 24 if speciescode == 110
	replace speciesgroup = 40 if speciescode == 111
	replace speciesgroup = 41 if speciescode == 112
	replace speciesgroup = 42 if speciescode == 113
	replace speciesgroup = 42 if speciescode == 114
	replace speciesgroup = 10 if speciescode == 126
	replace speciesgroup = 10 if speciescode == 127
	replace speciesgroup =  7 if speciescode == 128
	replace speciesgroup = 22 if speciescode == 130
	replace speciesname = "Tetraodon cambodgiensis" if speciescode == 131


	replace speciesgroup = 43 if speciescode == 132
	replace speciesgroup = 43 if speciescode == 133
	replace speciesgroup = 49 if speciescode == 134
	replace speciesgroup = 49 if speciescode == 136
	replace speciesgroup = 43 if speciescode == 137
	replace speciesgroup =  9 if speciescode == 138
	replace speciesgroup =  1 if speciescode == 139
	replace speciesgroup =  1 if speciescode == 140
	replace speciesgroup = 45 if speciescode == 141
	replace speciesgroup = 45 if speciescode == 142
	replace speciesgroup = 18 if speciescode == 143
	replace speciesgroup = 46 if speciescode == 144
	replace speciesgroup = 34 if speciescode == 146
	replace speciesgroup = 34 if speciescode == 147
	replace speciesgroup = 47 if speciescode == 148
	replace speciesgroup = 48 if speciescode == 149
	replace speciesgroup =  4 if speciescode == 150
	replace speciesgroup =  2 if speciescode == 151
	replace speciesgroup = 27 if speciescode == 152
	replace speciesgroup = 27 if speciescode == 153
	replace speciesgroup = 27 if speciescode == 154
	replace speciesgroup = 61 if speciescode == 159
	replace speciesgroup = 62 if speciescode == 162
	replace speciesgroup = 64 if speciescode == 164
	replace speciesgroup = 58 if speciescode == 169
	replace speciesgroup = 57 if speciescode == 125
	replace speciesgroup = 58 if speciescode == 92

	replace speciesgroup = 58 if speciesname == "Corbicula moreletiana"
	replace speciescode = 92 if speciesname == "Corbicula moreletiana"
	replace speciesgroup = 57 if speciescode == 166
	replace speciesgroup = 57 if speciescode == 120
	replace speciesgroup = 51 if speciescode == 90
	replace speciesgroup = 51 if speciescode == 88
	replace speciesgroup = 51 if speciescode == 89
	replace speciesgroup = 52 if speciescode == 91
	replace speciesname = "Hoplobatrachus rugulosus" if speciesname == "Hoplobatrachus tigerinus"
	replace speciesgroup = 57 if speciescode == 119
	replace speciesgroup = 53 if speciescode == 117
	replace speciesgroup = 50 if speciescode == 87
	replace speciesgroup = 52 if speciescode == 87
	replace speciesgroup = 59 if speciescode == 168
	replace speciesgroup = 53 if speciescode == 94
	replace speciesgroup = 53 if speciescode == 94
	replace speciesgroup = 53 if speciescode == 94
	replace speciesgroup = 57 if speciescode == 124
	replace speciesgroup = 57 if speciescode == 122
	replace speciesgroup = 57 if speciescode == 123
	replace speciesgroup = 55 if speciescode == 98
	replace speciesgroup = 50 if speciescode == 93
	replace speciesgroup = 53 if speciescode == 93
	replace speciesgroup = 53 if speciescode == 93
	replace speciesgroup = 57 if speciescode == 121
	replace speciesgroup = 51 if speciescode == 157
	replace speciesgroup = 51 if speciescode == 118
	replace speciesgroup = 51 if speciescode == 160
	replace speciesgroup = 51 if speciescode == 178
	replace speciesgroup = 54 if speciescode == 95
	replace speciesname = "Somanniathelpusa sp." if speciesname == "Macrobrachium Rosenagii"
	replace speciesgroup = 56 if speciescode == 116
	replace speciesgroup = 51 if speciescode == 115

	replace speciescode = 95 if speciescode == 161
	replace speciescode = 95 if speciesname == "Somanniathelpusa sp."
	replace speciesgroup = 54 if speciesname == "Somanniathelpusa sp."
	replace speciesgroup = 43 if speciesname == "Tetraodon cambodgiensis"
	replace speciescode = 131 if speciesname == "Tetraodon cambodgiensis"
	
	* fixing many variations on species names using codes generated above
	replace speciesname = "Hemibagrus spilopterus/nemurus" if speciescode == 8
	replace speciesname = "Anematichthys/Cyclocheilichthys apogon" if speciescode == 9 // these are different names for the same species https://en.wikipedia.org/wiki/Beardless_barb
	replace speciesname = "Anematichthys/Cyclocheilichthys armatus" if speciescode == 11 // I believe these are also synonyms https://en.wikipedia.org/wiki/Anematichthys_armatus
	replace speciesname = "Labiobarbus siamensis" if speciescode == 20
	replace speciesname = "Labiobarbus leptocheilus" if speciescode == 21
	replace speciesname = "Henicorhynchus siamensis" if speciescode == 22 // synonyms
	replace speciesname = "Henicorhynchus lobatus" if speciescode == 23 // synonyms
	replace speciesname = "Rasbora myersi/dusonensis" if speciescode == 31
	replace speciesname = "Puntioplites proctozystron" if speciescode == 37
	replace speciesname = "Clarias macrocephalus" if speciescode == 41
	replace speciesname = "Trichopodus pectoralis" if speciescode == 51
	replace speciesname = "Trichopsis pumila" if speciescode == 57
	replace speciesname = "Ompok bimaculatus/Hemisilurus mekongensis" if speciescode == 67
	replace speciesname = "Macrognathus semiocellatus" if speciescode == 69
	replace speciesname = "Puntius orphoides" if speciescode == 99 // synonyms
	replace speciesname = "Osteochilus vittatus/lini/hasselti" if speciescode == 102
	replace speciesname = "Corica laciniata" if speciescode == 106
	replace speciesname = "Oxygaster pointoni" if speciescode == 107
	replace speciesname = "Ompok hypophthalmus/urbaini" if speciescode == 110
	replace speciesname = "Channa micropeltes" if speciescode == 111 
	replace speciesname = "Clupeichthys aesarnensis" if speciescode ==  126
	replace speciesname = "Leiocassis/Pseudomystus siamensis" if speciescode == 139
	replace speciesname = "Mystus wolffi" if speciescode == 140
	replace speciesname = "Osteochilus microcephalus" if speciescode == 147
	replace speciesname = "Parambassis wolffii" if speciescode == 49
	replace speciesname = "Enhydris enhydris" if speciescode == 88
	replace speciesname = "Pila polita" if speciescode == 93
	replace speciesname = "Somanniathelpusa sp." if speciescode == 95
	replace speciesname = "Systomus partipentazona" if speciescode == 103
	replace speciesname = "Xenochrophis piscator" if speciescode == 115
	
	* Recode 175 and 173 which are grouped into 102 in some datasets, so need to be treated as a single category for consistency.
	replace speciescode = 102 if speciescode == 173 | speciescode == 175
	replace speciesname = "Osteochilus vittatus/lini/hasselti" if speciescode == 102
	

	
