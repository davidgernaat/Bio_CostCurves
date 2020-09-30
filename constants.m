CONST	NC = 66663;
CONST 	NCW = 66896;
CONST 	NR27 = 27;	! 26 regions + empty
CONST	NR27T = 28;	! 27 s + world total
CONST 	NR17 = 17;	! 17 regions
CONST	NR17T = 18;	! 17 regions + world total
CONST	NPC = 100;	! Number of price categories?
CONST 	MAXCELL = 15000;
CONST   GJperkWh = 0.0036;
CONST 	NLC=20; ! of 19 in oude IMAGE
CONST 	EPS = 1.0e-6;
CONST	NS = 5;
CONST	NTRAD = 4;	! Number of secondary energy carriers for traditional biomass
					! 1. Charcoal
					! 2. Fuelwood
					! 3. Crop Residue
					! 4. Dung
CONST	NCROP = 5;	! Number of primary energy crops
					! 1. WOODYY
					! 2. MAIZE
					! 3. SUGARCANE
					! 4. OIL CROPS
					! 5. NON-WOODY
CONST LandDegCat=5;
CONST WatershortCat=4; 		!1= 0<0.1, 2 = >=0.1<0.2, 3 = >=0.2<0.4, 4= >=0.4

INTEGER	PC;		!counter for Price Categories
INTEGER	R;		!counter for regions
INTEGER C;		!counter for Cells
INTEGER CROP;		!counter for Energy Crops

CONST	DecliningFac = 1;
CONST	HaPerKm2	= 100;
CONST	MaxYieldNWood	= 49.642;	! Mg/ha/y, as taken from IMAGE 2.2
CONST	MaxYieldWoody	= 55.766;	! Mg/ha/y, as taken from IMAGE 2.2
CONST	MaxYieldMAIZE	= 24.401;	! Mg/ha/y, as taken from IMAGE 2.2
CONST	MaxYieldSUGAR	= 217.695;	! Mg/ha/y, as taken from IMAGE 2.2

CONST	LHVWood		= 16;		! MJ/kg Lower Heating value woody energy crops
CONST	LHVNonWoody	= 15.3;	     	! MJ/kg	    Lower Heating value non-woody energy crops (switchgrass & Miscanthus), ECN-phyllis database
CONST	LHVmaize 	= 16;		! MJ/kg Lower Heating value maize
CONST	LHVsugar 	= 12;		! MJ/kg Lower Heating value sugar cane, Hamelinck (2004) thesis, HHV = 17.35, converted to LHV
!CONST	LHVWheat	= 17;		! MJ/kg Lower Heating value wheat (Phyllis, ECN database)
CONST	LHVOilCrop	= 18;		! MJ/kg Lower Heating value Oilcrops (Phyllis, ECN database)

CONST	HHVWood		= 19.5;		! MJ/kg Higher Heating value woody energy crops Hamelinck Hoogwijkck LCA 2007
CONST	HHVNonWoody	= 16.5;	     ! MJ/kg	    Higher Heating value non-woody energy crops (switchgrass & Miscanthus), ECN-phyllis database
CONST	HHVmaize 	= 15.8;		! MJ/kg Higher Heating value maize Hamelinck Hoogwijkck LCA 2007
CONST	HHVsugar 	= 4.5;		! MJ/kg Higher Heating value sugar cane, van Vuuren (Hamelinck Hoogwijkck LCA 2007 give 17.975)
!CONST	HHVWheat	= 18.15;	! MJ/kg Higher Heating value wheat Hamelinck Hoogwijkck LCA 2007
CONST	HHVOilCrop	= 28.3;		! MJ/kg Higher Heating value Oilcrops Hamelinck Hoogwijkck LCA 2007

CONST	LHVMSW 		= 15;	
CONST	hours 		= 8760;
CONST	kWhtoGJ		= 0.0036;
CONST	kWperMW		= 1000;

REAL 	HValueWood;
REAL	HValueNWood;
REAL 	HValueMaize;
REAL 	HValueSugar;
!REAL 	HValueWheat;
REAL 	HValueOilCrop;


!!!!!!!!!!!!!!!!!!RESIDUES DECLARATIONS!!!!!!!!!!!!!!!!!!!!!!!!
CONST
Ha2km	= 100, 	! Ha per km^2
NRCT 	= 28,
NRC 	= 27,
RCC	= 9,		! Residue Cost Categories . 9 "on site" + 1 "process residues" (added later)
NBC2	= 20,
tstart 	= 2015,
tcum 	= 2100,

CROPT	= 18,					CROPT2	= 22,
! IMAGE Crops Input				! IMAGE Crops Input + Other Crops
! 1. Rainfed temperate cereals			! 1. Rainfed temperate cereals
! 2. Rainfed rice				! 2. Rainfed rice
! 3. Rainfed maize				! 3. Rainfed maize
! 4. Rainfed tropical cereals			! 4. Rainfed tropical cereals
! 5. Rainfed pulses				! 5. Rainfed pulses
! 6. Rainfed roots and tubers			! 6. Rainfed roots and tubers
! 7. Rainfed oil crops				! 7. Rainfed oil crops
! 8. Biofuel sugarcane				! 8. Biofuel sugarcane
! 9. Biofuel maize				! 9. Biofuel maize
! 10. Biofuel woody				! 10. Biofuel woody
! 11. Biofuel non-woody 			! 11. Biofuel non-woody 
! 12. Irrigated temperate cereals		! 12. Irrigated temperate cereals
! 13. Irrigated rice				! 13. Irrigated rice
! 14. Irrigated maize				! 14. Irrigated maize
! 15. Irrigated tropical cereals		! 15. Irrigated tropical cereals
! 16. Irrigated pulses				! 16. Irrigated pulses
! 17. Irrigated roots and tubers		! 17. Irrigated roots and tubers
! 18. Irrigated oil crops 			! 18. Irrigated oil crops 
						! 19. Other: fibre
						! 20. Other: Fruit
						! 21. Other: Vegetables
						! 22. Other: Sugarcane

CROPS	= 16,	! Agricultural crops producing residues
! 1. Temperate cereals 	(rainfed + irrigated)
! 2. Rice 		(rainfed + irrigated)
! 3. Maize		(rainfed + irrigated)
! 4. Tropical cereals	(rainfed + irrigated)
! 5. Pulses 		(rainfed + irrigated)
! 6. Roots and tubers 	(rainfed + irrigated)
! 7. Oil crops 		(rainfed + irrigated)
! 8. Biofuel sugarcane	(biofuel)
! 9. Biofuel maize	(biofuel)
! 10. Biofuel woody	(biofuel)
! 11. Biofuel non-woody (biofuel)
! 12. Fibre		(other crops)
! 13. Fruit		(other crops)
! 14. Vegetables	(other crops)
! 15. Sugarcane		(other crops)
! 16. Total

NFC	= 7,	! Crop numbers
! Agricultural Crops
! 1. Temperate Cereals
! 2. Rice
! 3. Maize
! 4. Tropical Cereals
! 5. Pulses
! 6. Roots and Tubers
! 7. Oil Crops

NRES	= 17,	
! RESIDUE TYPE
! 1. Temperate cereals 	(rainfed + irrigated)
! 2. Rice 		(rainfed + irrigated)
! 3. Maize		(rainfed + irrigated)
! 4. Tropical cereals	(rainfed + irrigated)
! 5. Pulses 		(rainfed + irrigated)
! 6. Roots and tubers 	(rainfed + irrigated)
! 7. Oil crops 		(rainfed + irrigated)
! 8. Biofuel sugarcane	(biofuel)
! 9. Biofuel maize	(biofuel)
! 10. Biofuel woody	(biofuel)
! 11. Biofuel non-woody (biofuel)
! 12. Fibre		(other crops)
! 13. Fruit		(other crops)
! 14. Vegetables	(other crops)
! 15. Sugarcane		(other crops)
! 16. Forestry
! 17. Total

NREST 	= 5;
! Residue Type Aggregated
! 1. Agriculture
! 2. Biofuel
! 3. Other
! 4. Forestry
! 5. Total

! Vegetation types:
!1. Agriculture			2. Extensive grassland		3. C. Plantations 		4. Regrowth forest (abandoned)
!5. Regrowth forest (timber)	6. Biofuel			7. Ice				8. tundra			
!9. Wooded Tundra		10. Boreal Forest		11. Cool conifer Forest		12. Temp. Mixed forest		
!13. Temp Decid. forest		14. Warm mixed forest		15. Grassland/steppe		16. Hot desert			
!17. Scrubland			18. Savanah			19. Tropical woodland		20. Tropical forest

! Biomass pools
! 1. Stems, 2. Branches, 3. Leaves, 4. Roots, 5. Soil Litter, 6. Soil Humus, 
! 7. Soil Charcoal, 8. Slow decaying Timber (sawlogs) 9. Fast decaying timber (pulp&paper)

! Forest Types
! 1. Primary,	2. Mature (cc),	3. Mature (sc),	4. Mature (wp), 5. Mature (ad), 6. Regrowth (cc), 7. Regrowth (sc)
! 8. Regrowth (wp), 9. Regrowth (ad)

! Livestock feed types
! 1. Extensive Grazing System	1. Food Crops 		1. non-dairy cattle	
! 2. Intensive Grazing System	2. Animal Products	2. Dairy Cattle
! 3. Total			3. Residues		3. Pigs
! 				4. Scavenging		4. Sheep & Goats
!				5. Grass & Fodder	5. Poultry
!				6. Total		6. Total