! ****************** CONSTANTS ******************
!Counters
INTEGER
R,	! Regions
CROP,	! Crops
PC;	! Price Categories


CONST
EPS	= 1E-6,
NC	= 66663,	! number of grid cells
NCW	= 66896,	! number of cells including water bodies
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
