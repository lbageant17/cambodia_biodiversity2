traits_by_species.csv was provided by Sebastian Heilpern on 8/2/2022. 

Species: This list of species aligns with Column E of WorldFish_Cambodia_Names_withNotes.csv.

Description of variables related to fish catch. 
These come from the Cambodia WorldFish dataset used in other parts of this analysis. Sebastian suggested we re-calculate and check these values and don't necessarily use them at face value. Values in traits_by_species_csv_20220803.csv have not been altered or cross-checked. 

Total, Consumed and Sold: Total weight (kg) fish caught, consumed and sold, respectively. 

Numday: Number of days of survey data collection. Unclear how this was calculated.

Variables with suffix _daily: Average daily consumption calculated by dividing total/consumed/sold by Numday. Use with caution.

LongMig: all NA values

TG: Trophic guild.

TL: Total length (i.e. body size)

Units for variables with _content suffix
	Protein (Pr): g/100g
	Iron (Fe): mg/100g
	Zinc (Zn): mg/100g
	Calcium (Ca): mg/100g
	Vitamin A (VitA): mcg/100g
	Omega-3 (O3): mg/100g

Sebastian's description of variables with _MinBio suffix
"The amount of biomass a child under five would need to consume to cover 100% of their RDAs for that unit. Estimated as RDA*100/nutrient content, where RDA is detailed below."

	RDA values: FAO & WHO data--get citation from Sebastian
	Protein = 13 g
	Iron = 7.55 mg
	Zinc = 4.1 mg
	Calcium = 450 mg
	Vitamin A = 400 mcg
	Omega-3 = 285 mg

Sebastian's description of variables with _RDA_100g suffix
"The %RDA met with 100g y a child under 5, estimated using nutrient content divided by RDA."

NumRDA 100: The number of RDAs 100% met with 100g by a child under five. In theory this could range from 0 to 6, but in practice the Cambodia data do not contain any species that meet 0 RDAs or 6 RDAs at 100%. 
NumRDA 50: The number of RDAs 50% met with 100g by a child under five. This ranges from 1-6. 



