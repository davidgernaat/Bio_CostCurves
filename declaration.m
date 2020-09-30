!!!!!!!!!!!!!!!!!!GENERAL ASSUMPTION!!!!!!!!!!!!!!!!!!!!!!!!
REAL		Impl_frac_inp[NLC](t) 		= FILE ("Data/impl_frac.dat");
INTEGER 	Bioreserve_old[NC](t)		= FILE ("Data/IMAGE/bioreserve.dat");
REAL 		Wetland[NC]			= FILE ("data/COPI_20080129_GLWDWetlands.dat");
REAL 		Protected_fraction[NC]		= FILE ("data/fraction_protected_area_2005.dat");
INTEGER 	Bioreserve_new[NC](t);
REAL		Bioreserve[NC](t);
REAL		BuildUp[NC] 			= FILE ("Data/IMAGE/BuildUp.dat");
REAL		alt[NC] 			= FILE ("Data/IMAGE/altitude.dat");
REAL		Popdens[NC] 			= FILE ("Data/IMAGE/popdens.dat"); !not used?
EXPORT INTEGER	Region[NC]			= FILE ("Data/IMAGE/Region27.dat");
INTEGER		Landdegrad[NC]			= FILE ("Data/IMAGE/gl_sev_WORLD.out");
REAL		Watershort[NC](t)		= FILE ("Data/IMAGE/water_Bratio.dat");
EXPORT REAL	Area[NC]			= FILE ("Data/Area.dat");
INTEGER		LandCover[NC](t)		= FILE ("Data/GLCT.dat");
INTEGER		AbonCropLand[NC](t) 		= FILE ("Data/GLCTRFA_biomass.dat");
INTEGER		Grazing[NC](t) 			= FILE ("Data/grazing.dat");
EXPORT REAL	Pop[NR27](t) 			= FILE ("Data/pop27.scn");	!not used? 
! Allow for multiple Bioreserve layers, declared as scenario inputs
INTEGER		Bioreserve_in1[NC];		
INTEGER		Bioreserve_in2[NC];		
INTEGER		Bioreserve_in3[NC];		

!!!!!!!!!!!!!FROM IMAGE run !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Energy crop productivity
! The relative productivity as percentage of the maximum productivity (see MaxYield).
REAL		GRMPPC[NC,19](t)		= FILE("Data/GRMPPC");	! T/Ha

!These maps are absolute maximum productivity
REAL 		GroundnutPot [NC](t)		= FILE ("Data/IMAGE/GroundnutPot.dat");		! kg/ha/yr
REAL 		SesamePot [NC](t)		= FILE ("Data/IMAGE/SesamePot.dat");		! kg/ha/yr
REAL 		SoyaPot [NC](t)			= FILE ("Data/IMAGE/SoyaPot.dat");		! kg/ha/yr
REAL 		SunflTempPot[NC](t)		= FILE ("Data/IMAGE/SunflTempPot.dat");		! kg/ha/yr
REAL 		SunflTropPot [NC](t)		= FILE ("Data/IMAGE/SunflTropPot.dat");		! kg/ha/yr

! ******************************************************************\
REAL		RelProdWoodyBiomass[NC](t); 	
REAL		RelProdNWoodBiomass[NC](t);	
REAL		RelProdMaizeBiomass[NC](t); 	
REAL		RelProdSugarBiomass[NC](t); 	

REAL		WoodyProductivity[NC](t);	! GJ/ha/y
REAL		NWoodProductivity[NC](t);	! GJ/ha/y
REAL		MaizeProductivity[NC](t);	! GJ/ha/y
REAL		SugarProductivity[NC](t);	! GJ/ha/y
REAL		OilcropProductivity[NC](t);	! GJ/ha/y

REAL		WoodyProductivityAbon[NC](t);		! in GJ/ha/y
REAL		NWoodProductivityAbon[NC](t);		! in GJ/ha/y
REAL		MaizeProductivityAbon[NC](t);		! in GJ/ha/y
REAL		SugarProductivityAbon[NC](t);		! in GJ/ha/y
REAL		OilcropProductivityAbon[NC](t);		! in GJ/ha/y
		
REAL		WoodyProductivityRest[NC](t);		! in GJ/ha/y
REAL		NWoodProductivityRest[NC](t);		! in GJ/ha/y
REAL		MaizeProductivityRest[NC](t);		! in GJ/ha/y
REAL		SugarProductivityRest[NC](t);		! in GJ/ha/y
REAL		OilcropProductivityRest[NC](t);		! in GJ/ha/y

REAL 		WOODYProdPerCellAbon[NC](t);
REAL 		NWOODProdPerCellAbon[NC](t);
REAL 		MAIZEProdPerCellAbon[NC](t);
REAL 		SUGARProdPerCellAbon[NC](t);
REAL 		OilcropProdPerCellAbon[NC](t);

REAL 		WOODYProdPerCellREST[NC](t);
REAL 		NWOODProdPerCellREST[NC](t);
REAL 		MAIZEProdPerCellREST[NC](t);
REAL 		SUGARProdPerCellREST[NC](t);
REAL 		OilcropProdPerCellREST[NC](t);

REAL 		WOODYProdPerCellTot[NC](t);
REAL 		NWOODProdPerCellTot[NC](t);
REAL 		MAIZEProdPerCellTot[NC](t);
REAL 		SUGARProdPerCellTot[NC](t);
REAL 		OilcropProdPerCellTot[NC](t);

REAL 		BuiltUpLand[NR27](t);
REAL 		BuitUpLandFrac[NR27](t);
REAL 		BuiltUpLandScale[NR27](t);
INTEGER 	AbonCropLandMult[NC](t);
INTEGER 	AbonTimberMult[NC](t);
INTEGER		AbonMult[NC](t);

REAL		AreaCorrect[NC](t);			! in km2
REAL		AreaReserve[NC](t);			! in km2	
REAL		AreaUrban[NC];				! in km2
REAL    	UrbanLand[NR27];
REAL		Areaaltitude[NC];
REAL		wghtfactAlt[NC];
	

INTEGER		InproductiveMult[NC](t);		
INTEGER		RestMult[NC](t);
REAL 		ImplementationMap[NC](t); ! Indication of area availbe for bio-energy based on implementation criteria
REAL 		ImplementationMap2[NC](t); 

REAL		TotArea[NR27];
REAL 		LandAbon[NC](t);! Available land at abandoned cropland for biofuels
REAL 		LandAbonArea[NC](t);! Total available land at abandoned cropland (not accounting for implementation fraction)
REAL 		LandREST[NC](t);! Available land at rest land for biofuels
REAL 		LandRestArea[NC](t);! Total available land at rest land (not accounting for implementation fraction)
REAL 		InproductiveLand2[NC](t);
REAL 		Aglandmap[NC](t);
REAL 		Agland[NR27](t);
REAL 		TotRestArea[NR27](t); 		!Km2	
REAL 		TotArea2[2](t);			!ha
REAL 		TotAbonArea[NR27](t);		!km2
REAL 		RegReserveArea[NR27](t);		!km2
REAL 		TotReserveArea(t);
REAL 		LandAbonAreaReg[NR27](t);
REAL 		LandRestAreaReg[NR27](t);
REAL 		InproductiveLandTotReg[NR27](t);
REAL 		LandOverviewWorld[6](t);
INTEGER 	OverviewMap[NC](t);
REAL 		LandDegMap[NC](t);
REAL 		LandDeg[LandDegCat,NC](t);
REAL 		Watershortindex[WatershortCat,NC](t);
REAL 		WetLandAct[NC];
REAL 		Forest[NC](t);
REAL 		ForestCor[NC](t);				
REAL 		ForestReg[NR27](t);
REAL 		ForestCorReg[NR27](t);
REAL		BioreserveLand[NR27](t);

INTEGER 	AvExtGrassland[NC](t);
INTEGER 	NotAvExtGrassland[NC](t);

REAL		Impl_frac[NLC](t);

! VARIABLES FOR EXTERNAL CROP YIELDS TO BE USED IN ISIMIP PROJECT. 
REAL 	ISIMIP_BGR_YIELD_in[NC](t);
REAL 	ISIMIP_BTR_YIELD_in[NC](t);
REAL 	ISIMIP_SUG_YIELD_in[NC](t);
REAL 	ISIMIP_MAI_YIELD_in[NC](t);
INTEGER ISIMIP_BGR_CHECK(t);
INTEGER ISIMIP_BTR_CHECK(t);
INTEGER ISIMIP_SUG_CHECK(t);
INTEGER ISIMIP_MAI_CHECK(t);
REAL	ISIMIP_BGR_YIELD[NC](t);
REAL	ISIMIP_BTR_YIELD[NC](t);
REAL	ISIMIP_SUG_YIELD[NC](t);
REAL	ISIMIP_MAI_YIELD[NC](t);

ISIMIP_BGR_YIELD[C]	= SWITCH(ISIMIP_BGR_YIELD_in[C] > 1000 ? 0 ELSE ISIMIP_BGR_YIELD_in[C]), C = 1 TO NC;	! In input data some grid cells have unreasonably high value (1e20). This seems to arise in the nc2m conversion process
ISIMIP_BTR_YIELD[C]	= SWITCH(ISIMIP_BTR_YIELD_in[C] > 1000 ? 0 ELSE ISIMIP_BTR_YIELD_in[C]), C = 1 TO NC;	! In input data some grid cells have unreasonably high value (1e20). This seems to arise in the nc2m conversion process
ISIMIP_SUG_YIELD[C]	= SWITCH(ISIMIP_SUG_YIELD_in[C] > 1000 ? 0 ELSE ISIMIP_SUG_YIELD_in[C]), C = 1 TO NC;	! In input data some grid cells have unreasonably high value (1e20). This seems to arise in the nc2m conversion process
ISIMIP_MAI_YIELD[C]	= SWITCH(ISIMIP_MAI_YIELD_in[C] > 1000 ? 0 ELSE ISIMIP_MAI_YIELD_in[C]), C = 1 TO NC;	! In input data some grid cells have unreasonably high value (1e20). This seems to arise in the nc2m conversion process
ISIMIP_BGR_CHECK = SWITCH( LSUM(C = 1 TO NC, ISIMIP_BGR_YIELD[C]) > EPS ? 1 ELSE 0);
ISIMIP_BTR_CHECK = SWITCH( LSUM(C = 1 TO NC, ISIMIP_BTR_YIELD[C]) > EPS ? 1 ELSE 0);
ISIMIP_SUG_CHECK = SWITCH( LSUM(C = 1 TO NC, ISIMIP_SUG_YIELD[C]) > EPS ? 1 ELSE 0);
ISIMIP_MAI_CHECK = SWITCH( LSUM(C = 1 TO NC, ISIMIP_MAI_YIELD[C]) > EPS ? 1 ELSE 0);

! Old/new IMAGE land classification

! 1 = Ag land			; Ag land		
! 2 = Ext. grassland		; Ext. grassland		
! 3 = C-plant			; C-plant
! 4 = Regrowth	(Abandoning)	; Regrowth (Abandoning)
! 5 = Regrowth	(Timber)	; Regrowth (Timber)
! 6 = Ice			; Biofuel
! 7 = Tundra			; Ice
! 8 = Wooded Tundra		; Tundra
! 9  = Boreal forest		; Wooded Tundra	
! 10 = Cool conifer		; Boreal forest	
! 11 = Temp. mixed		; Cool conifer
! 12 = Temp. decid		; Temp. mixed
! 13 = Warm mixed		; Temp. decid
! 14 = Grassland / steppe	; Warm mixed
! 15 = Hot desert		; Grassland / steppe
! 16 = Scrub			; Hot desert
! 17 = Savannah			; Scrub		
! 18 = Trop. woodland		; Savannah	
! 19 = Trop. forest		; Trop. woodland	
! 20				; Trop. forest

! IMAGE CROP NUMBERS (CHECK!)			
! 1. Grasses + Fodder				
! 2. Rainfed Temperate Cereals			
! 3. Rainfed Rice				
! 4. Rainfed Maize				
! 5. Rainfed Tropical Cereals
! 6. Rainfed Pulses
! 7. Rainfed Roots and Tubers
! 8. Rainfed OilCrops
! 9. Biofuels Sugarcane
! 10. Biofuels Maize
! 11. Biofuels Woody
! 12. Biofuels Non-Woody 
! 13. Irrigated Temperate Cereals
! 14. Irrigated Rice
! 15. Irrigated Maize
! 16. Irrigated Tropical Cereals
! 17. Irrigated Pulses
! 18. Irrigated Roots and Tubers
! 19. Irrigated OilCrops
