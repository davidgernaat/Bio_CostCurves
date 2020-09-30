! CALCULATIONS TO AGGREGATE RESULTS IN ORDER TO GET AN UNDERSTANDING OF TOTAL POTENTIAL
! ONLY FOR PRESENTATION PURPOSES
REAL	ManagementFactor_24[12,24](t)	= FILE("..\scenlib\BioI2T\SSP2_0run\MF.out");
REAL	ManagementFactor1[5,NR27](t);
REAL	ManagementFactor[5,NR27](t);
REAL	WOODYPotAbonGJ[NR27](t);
REAL	NWOODPotAbonGJ[NR27](t);
REAL	MAIZEPotAbonGJ[NR27](t);
REAL	SUGARPotAbonGJ[NR27](t);
REAL	OilCropPotAbonGJ[NR27](t);
REAL	WOODYPotRestGJ[NR27](t);
REAL	NWOODPotRestGJ[NR27](t);
REAL	MAIZEPotRestGJ[NR27](t);
REAL	SUGARPotRestGJ[NR27](t);
REAL	OilCropPotRestGJ[NR27](t);
REAL	WOODYPotTotGJ[NR27](t);
REAL	NWOODPotTotGJ[NR27](t);
REAL	MAIZEPotTotGJ[NR27](t);
REAL	SUGARPotTotGJ[NR27](t);
REAL	OilCropPotTotGJ[NR27](t);
REAL	BioPotAbonGJ[NR27](t);	
REAL	BioPotRestGJ[NR27](t);	
REAL	BioPotTotGJ[NR27](t);

REAL	BioPotAbonT[NR27](t);	
REAL	BioPotRestT[NR27](t);	
REAL	BioPotTotT[NR27](t);	

ManagementFactor1[1, R]  	= ManagementFactor_24[11,R], R = 1 to 24;	!WOODY
ManagementFactor1[2, R]  	= ManagementFactor_24[10,R] , R = 1 to 24;	! MAIZE
ManagementFactor1[3, R]  	= ManagementFactor_24[9,R] , R = 1 to 24;	! SUGAR
ManagementFactor1[4, R]  	= ManagementFactor_24[8,R] , R = 1 to 24;	! OILCR
ManagementFactor1[5, R]  	= ManagementFactor_24[12,R] , R = 1 to 24;	! NWOOD
ManagementFactor1[i, 25]  	= ManagementFactor1[i, 18], i = 1 to 5; 
ManagementFactor1[i, 26]  	= ManagementFactor1[i, 10], i = 1 to 5; 
ManagementFactor1[i, 27]  	= 0.0                    , i = 1 to 5;

ManagementFactor[i,R]	= SWITCH( FLAGMFConst = 1 AND t > 2015 ?
				MIN(ManagementFactor1[i,R],ManagementFactor1[i,r](2015))
			ELSE ManagementFactor1[i,R]), i = 1 TO 5, R = 1 TO 27;

! *** GJ ***
! NOTE: MULTIPLE OILCROPS BY 0.15 SINCE INFOR SEEMS WAY TOO HIGH. SAME IN TIMER
WOODYPotAbonGJ[R]	= LSUM(i = 1 TO NPC, ManagementFactor[1,R] * WOODYyieldcurveAbonSmth[R,i] * (WOODYMaxTechLandAbon[R] * HaPerKm2)/NPC), R = 1 TO 26;
NWOODPotAbonGJ[R]	= LSUM(i = 1 TO NPC, ManagementFactor[5,R] * NWOODyieldcurveAbonSmth[R,i] * (WOODYMaxTechLandAbon[R] * HaPerKm2)/NPC), R = 1 TO 26;
MAIZEPotAbonGJ[R]	= LSUM(i = 1 TO NPC, ManagementFactor[2,R] * MAIZEyieldcurveAbonSmth[R,i] * (WOODYMaxTechLandAbon[R] * HaPerKm2)/NPC), R = 1 TO 26;
SUGARPotAbonGJ[R]	= LSUM(i = 1 TO NPC, ManagementFactor[3,R] * SUGARyieldcurveAbonSmth[R,i] * (WOODYMaxTechLandAbon[R] * HaPerKm2)/NPC), R = 1 TO 26;
OilCropPotAbonGJ[R]	= LSUM(i = 1 TO NPC, 0.15 * ManagementFactor[4,R] * OilCropyieldcurveAbonSmth[R,i] * (WOODYMaxTechLandAbon[R] * HaPerKm2)/NPC), R = 1 TO 26;

WOODYPotAbonGJ[NR27] 	= LSUM(R = 1 TO 26, WOODYPotAbonGJ[R]);
NWOODPotAbonGJ[NR27] 	= LSUM(R = 1 TO 26, NWOODPotAbonGJ[R]);
MAIZEPotAbonGJ[NR27] 	= LSUM(R = 1 TO 26, MAIZEPotAbonGJ[R]);
SUGARPotAbonGJ[NR27] 	= LSUM(R = 1 TO 26, SUGARPotAbonGJ[R]);
OilCropPotAbonGJ[NR27] 	= LSUM(R = 1 TO 26, OilCropPotAbonGJ[R]);

WOODYPotRestGJ[R]	= LSUM(i = 1 TO NPC, ManagementFactor[1,R] * WOODYyieldcurveRestSmth[R,i] * (WOODYMaxTechLandRest[R] * HaPerKm2)/NPC), R = 1 TO 26;
NWOODPotRestGJ[R]	= LSUM(i = 1 TO NPC, ManagementFactor[5,R] * NWOODyieldcurveRestSmth[R,i] * (WOODYMaxTechLandRest[R] * HaPerKm2)/NPC), R = 1 TO 26;
MAIZEPotRestGJ[R]	= LSUM(i = 1 TO NPC, ManagementFactor[2,R] * MAIZEyieldcurveRestSmth[R,i] * (WOODYMaxTechLandRest[R] * HaPerKm2)/NPC), R = 1 TO 26;
SUGARPotRestGJ[R]	= LSUM(i = 1 TO NPC, ManagementFactor[3,R] * SUGARyieldcurveRestSmth[R,i] * (WOODYMaxTechLandRest[R] * HaPerKm2)/NPC), R = 1 TO 26;
OilCropPotRestGJ[R]	= LSUM(i = 1 TO NPC, 0.15 * ManagementFactor[4,R] * OilCropyieldcurveRestSmth[R,i] * (WOODYMaxTechLandRest[R] * HaPerKm2)/NPC), R = 1 TO 26;

WOODYPotRestGJ[NR27] 	= LSUM(R = 1 TO 26, WOODYPotRestGJ[R]);
NWOODPotRestGJ[NR27] 	= LSUM(R = 1 TO 26, NWOODPotRestGJ[R]);
MAIZEPotRestGJ[NR27] 	= LSUM(R = 1 TO 26, MAIZEPotRestGJ[R]);
SUGARPotRestGJ[NR27] 	= LSUM(R = 1 TO 26, SUGARPotRestGJ[R]);
OilCropPotRestGJ[NR27] 	= LSUM(R = 1 TO 26, OilCropPotRestGJ[R]);

WOODYPotTotGJ[R]	= WOODYPotAbonGJ[R] + WOODYPotRestGJ[R], R = 1 TO NR27;
NWOODPotTotGJ[R]	= NWOODPotAbonGJ[R] + NWOODPotRestGJ[R], R = 1 TO NR27;
MAIZEPotTotGJ[R]	= MAIZEPotAbonGJ[R] + MAIZEPotRestGJ[R], R = 1 TO NR27;
SUGARPotTotGJ[R]	= SUGARPotAbonGJ[R] + SUGARPotRestGJ[R], R = 1 TO NR27;
OilCropPotTotGJ[R]	= OilCropPotAbonGJ[R] + OilCropPotRestGJ[R], R = 1 TO NR27;

! TOTAL BIOMASS POTENTIAL
BioPotAbonGJ[R]		= MAX(WOODYPotAbonGJ[R],NWOODPotAbonGJ[R],MAIZEPotAbonGJ[R],SUGARPotAbonGJ[R],OilCropPotAbonGJ[R]), R = 1 TO NR27;
BioPotRestGJ[R]		= MAX(WOODYPotRestGJ[R],NWOODPotRestGJ[R],MAIZEPotRestGJ[R],SUGARPotRestGJ[R],OilCropPotRestGJ[R]), R = 1 TO NR27;
BioPotTotGJ[R]		= BioPotAbonGJ[R] + BioPotRestGJ[R], R = 1 TO NR27;

! *** t ***
REAL	BioPotSpec1[NR27,2,5](t),
	HHVSpec[5],
	BioPotAbonT1[NR27,5](t),
	BioPotRestT1[NR27,5](t);

BioPotSpec1[R,1,1]	= WOODYPotAbonGJ[R], R = 1 TO NR27;
BioPotSpec1[R,1,2]	= NWOODPotAbonGJ[R], R = 1 TO NR27;
BioPotSpec1[R,1,3]	= MAIZEPotAbonGJ[R], R = 1 TO NR27;
BioPotSpec1[R,1,4]	= SUGARPotAbonGJ[R], R = 1 TO NR27;
BioPotSpec1[R,1,5]	= OilCropPotAbonGJ[R], R = 1 TO NR27;

BioPotSpec1[R,2,1]	= WOODYPotRestGJ[R], R = 1 TO NR27;
BioPotSpec1[R,2,2]	= NWOODPotRestGJ[R], R = 1 TO NR27;
BioPotSpec1[R,2,3]	= MAIZEPotRestGJ[R], R = 1 TO NR27;
BioPotSpec1[R,2,4]	= SUGARPotRestGJ[R], R = 1 TO NR27;
BioPotSpec1[R,2,5]	= OilCropPotRestGJ[R], R = 1 TO NR27;

HHVSpec[1]	= HValueWood;
HHVSpec[2]	= HValueNWood;
HHVSpec[3]	= HValueMaize;
HHVSpec[4]	= HValueSugar;
HHVSpec[5]	= HValueOilCrop;

BioPotAbonT1[R,j]	= SWITCH( BioPotspec1[R,1,j] = BioPotAbonGJ[R] ?
				BioPotSpec1[R,1,j]/HHVSpec[j]
			ELSE 0), R = 1 TO NR27, j = 1 TO 5;

BioPotRestT1[R,j]	= SWITCH( BioPotspec1[R,2,j] = BioPotRestGJ[R] ?
				BioPotSpec1[R,2,j]/HHVSpec[j]
			ELSE 0), R = 1 TO NR27, j = 1 TO 5;

BioPotAbonT[R]		= LMAX(i = 1 TO 5, BioPotAbonT1[R,i]), R = 1 TO NR27;
BioPotRestT[R]		= LMAX(i = 1 TO 5, BioPotRestT1[R,i]), R = 1 TO NR27;
BioPotTotT[R]		= BioPotAbonT[R] + BioPotRestT[R], R = 1 TO NR27;

REAL	HHVAgr[NR27](t);

HHVAgr[R]	= SWITCH(BioPottotT[R] > EPS ? BioPotTotGJ[R]/BioPotTotT[R] ELSE 0), R = 1 TO NR27;

! MAPS
REAL	WOODYProductivityGJ[NC](t),
	NWOODProductivityGJ[NC](t),
	MAIZEProductivityGJ[NC](t),
	SUGARProductivityGJ[NC](t),
	OilCropProductivityGJ[NC](t),
	WOODYPotentialGJ[NC](t),
	NWOODPotentialGJ[NC](t),
	MAIZEPotentialGJ[NC](t),
	SUGARPotentialGJ[NC](t),
	OILCROPPotentialGJ[NC](t),
	WOODYProductivityT[NC](t),
	NWOODProductivityT[NC](t),
	MAIZEProductivityT[NC](t),
	SUGARProductivityT[NC](t),
	OilCropProductivityT[NC](t),
	BioProdGJ[NC](t),
	BioProdGJNoCons[NC](t),
	BioProdT[NC](t);
! *** GJ ***
WOODYProductivityGJ[C]		= WOODYProductivity[C] * ManagementFactor[1,Region[C]], C = 1 TO NC;
NWOODProductivityGJ[C]		= NWOODProductivity[C] * ManagementFactor[5,Region[C]], C = 1 TO NC;
MAIZEProductivityGJ[C]		= MAIZEProductivity[C] * ManagementFactor[2,Region[C]], C = 1 TO NC;
SUGARProductivityGJ[C]		= SUGARProductivity[C] * ManagementFactor[3,Region[C]], C = 1 TO NC;
OilCropProductivityGJ[C]	= OilCropProductivity[C] * ManagementFactor[4,Region[C]], C = 1 TO NC;

WOODYPotentialGJ[C]		= WOODYProductivityGJ[C] * ImplementationMap[C]* Area[C] * HaPerKm2, C = 1 TO NC;
NWOODPotentialGJ[C]		= NWOODProductivityGJ[C] * ImplementationMap[C]* Area[C] * HaPerKm2, C = 1 TO NC;
MAIZEPotentialGJ[C]		= MAIZEProductivityGJ[C] * ImplementationMap[C]* Area[C] * HaPerKm2, C = 1 TO NC;
SUGARPotentialGJ[C]		= SUGARProductivityGJ[C] * ImplementationMap[C]* Area[C] * HaPerKm2, C = 1 TO NC;
OILCROPPotentialGJ[C]		= OILCROPProductivityGJ[C] * ImplementationMap[C]* Area[C] * HaPerKm2, C = 1 TO NC;

BioProdGJ[C]		= MAX(WOODYProductivityGJ[C],NWOODProductivityGJ[C],MAIZEProductivityGJ[C],SUGARProductivityGJ[C],OilCropProductivityGJ[C]) 
					* ImplementationMap[C]* Area[C] * HaPerKm2, C = 1 TO NC;

BioProdGJNoCons[C]	= MAX(WOODYProductivityGJ[C],NWOODProductivityGJ[C],MAIZEProductivityGJ[C],SUGARProductivityGJ[C],OilCropProductivityGJ[C]) 
					* Area[C] * HaPerKm2, C = 1 TO NC;

! *** TONS ***
WOODYProductivityT[C] 	= SWITCH( HValueWood 	> EPS ? (WOODYProductivity[C] / HValueWood) * ManagementFactor[1,Region[C]], ELSE 0), C = 1 TO NC;
NWOODProductivityT[C] 	= SWITCH( HValueNWood 	> EPS ? (NWOODProductivity[C] / HValueNWood) * ManagementFactor[5,Region[C]], ELSE 0), C = 1 TO NC;
MAIZEProductivityT[C] 	= SWITCH( HValueMaize 	> EPS ? (MAIZEProductivity[C] / HValueMaize) * ManagementFactor[2,Region[C]], ELSE 0), C = 1 TO NC;
SUGARProductivityT[C] 	= SWITCH( HValueSugar 	> EPS ? (SUGARProductivity[C] / HValueSugar) * ManagementFactor[3,Region[C]], ELSE 0), C = 1 TO NC;
OilcropProductivityT[C]	= SWITCH( HValueOilCrop > EPS ? (OilCropProductivity[C] / HValueOilCrop) * ManagementFactor[4,Region[C]], ELSE 0), C = 1 TO NC;

BioProdT[C]		= MAX(WOODYProductivityT[C],NWOODProductivityT[C],MAIZEProductivityT[C],SUGARProductivityT[C],OilCropProductivityT[C]) 
					* ImplementationMap[C]* Area[C] * HaPerKm2, C = 1 TO NC;

! Land Exclusion
REAL 	LandCons1[NC,4](t),	!map of grid cells excluded because of: 1. Water Stress, 2. Degraded, 3. BioReserve, 4. Combination of 2 or more
	LandCons[NC](t),
	AreaCons[NC,5](t),	! map of grid bioenergy potential cells: 1. Available, 2. Water Stressed, 3. Degraded, 4. Bioreserve, 5. Combination of 2 or more constraints
	AreaConsR1[NR27,5](t),
	AreaConsR[NR27,5](t);
	
LandCons1[C,1]	= SWITCH(FLAGWater = 1 AND Watershort[C] >= 0.4 ? 1 ELSE 0), C = 1 TO NC;	! Cells with water shortage
LandCons1[C,2]	= SWITCH(LandDegMap[C] = 0 ? 1, ELSE 0), C = 1 TO NC;				! Cells which are degraded
LandCons1[C,3]	= SWITCH(BioReserve[C] > EPS ? 1 ELSE 0), C = 1 TO NC;				! Cells which are reserved
LandCons1[C,4]	= LSUM(i = 1 TO 3, LandCons1[C,i]), C = 1 TO NC;				! Cells with 1 or more of the above constraints

LandCons[C]	= SWITCH(LandCons1[C,4] > 1 ? 4, ELSE						! First mark cells with 1+ constraints
			SWITCH(LandCons1[C,1] > EPS ? 1, ELSE					! Then mark the rest...
				SWITCH(LandCons1[C,2] > EPS ? 2, ELSE
					SWITCH(LandCons1[C,3] > EPS ? 3, ELSE 0)
									)
								)
							), C = 1 TO NC;

! MAPS OF ABANDONED AND REST LANDS
INTEGER	BioLandType[NC](t);	! 1. Abon, 2. Rest

BioLandType[C]	= SWITCH( ImplementationMap[C] > EPS ?
			SWITCH( AbonMult[C] > EPS ? 
				1 
			ELSE
			SWITCH( RestMult[C] > EPS ? 
				2 
			ELSE 0))
		ELSE 0), C = 1 TO NC;
		
! *** MAPS OF DIFFERENT BIOMASS POTENTIALS ***
REAL	TheoPot1stGen[NC](t),	! GJ-Primary
	TheoPot2ndGen[NC](t),	! GJ-Primary
	TheoPot1stGen_NoMF[NC](t),	! GJ-Primary, No Management Factor
	TheoPot2ndGen_NoMF[NC](t),	! GJ-Primary, No Management Factor
	TechPot1stGen[NC](t),	! GJ-Primary
	TechPot2ndGen[NC](t),	! GJ-Primary
	TechPot1stGen_NoMF[NC](t),	! GJ-Primary, No Management Factor
	TechPot2ndGen_NoMF[NC](t),	! GJ-Primary, No Management Factor
	EconPot1stGen[NC](t),	! GJ-Primary
	EconPot2ndGen[NC](t),	! GJ-Primary
	EconPot1stGen_NoMF[NC](t),	! GJ-Primary, No Management Factor
	EconPot2ndGen_NoMF[NC](t),	! GJ-Primary, No Management Factor
	Pot1stGenTot[3](t),
	Pot2ndGenTot[3](t),
	Pot1stGenTot_NoMF[3](t),
	Pot2ndGenTot_NoMF[3](t);

! THEORETICAL POTENTIAL
TheoPot1stGen[C] = MAX(MAIZEProductivityGJ[C],SUGARProductivityGJ[C]) * Area[C] * HaPerKm2, C = 1 TO NC;
TheoPot2ndGen[C] = MAX(WOODYProductivityGJ[C],NWOODProductivityGJ[C]) * Area[C] * HaPerKm2, C = 1 TO NC;	

TheoPot1stGen_NoMF[C] = MAX(MAIZEProductivity[C],SUGARProductivity[C]) * Area[C] * HaPerKm2, C = 1 TO NC;
TheoPot2ndGen_NoMF[C] = MAX(WOODYProductivity[C],NWOODProductivity[C]) * Area[C] * HaPerKm2, C = 1 TO NC;	

! TECHNICAL POTENTIAL
TechPot1stGen[C] = ImplementationMap[C] * MAX(AbonMult[C],RestMult[C],0) * TheoPot1stGen[C], C = 1 TO NC;
TechPot2ndGen[C] = ImplementationMap[C] * MAX(AbonMult[C],RestMult[C],0) * TheoPot2ndGen[C], C = 1 TO NC;

TechPot1stGen_NoMF[C] = ImplementationMap[C] * MAX(AbonMult[C],RestMult[C],0) * TheoPot1stGen_NoMF[C], C = 1 TO NC;
TechPot2ndGen_NoMF[C] = ImplementationMap[C] * MAX(AbonMult[C],RestMult[C],0) * TheoPot2ndGen_NoMF[C], C = 1 TO NC;

! ECONOMIC POTENTIAL
! FIRST HAVE TO DETERMINE LABOUR AND CAPITAL COSTS
! TAKEN FROM TIMER_2015_SSPs, biofeed.m
REAL	GDPpc[NR27](t)				= FILE("data/gdp_pc.dat"),
	discountrate[NR27](t)			= FILE("data/disc.scn"),		! - 
        CobbDouglasCoeff[NCROP]           	= FILE("data/CDcoeff.dat"),             ! - The capital labour factor substitution elasticity in Cobb/Douglas	
        LabCD_ini[NR27,NCROP]             	= FILE("data/Lab_ini.dat"),             ! manyear/Ha The initial labour input 
        CapCD_ini_in[NR27,NCROP]             	= FILE("data/Cap_ini.dat"),             ! $/Ha The initial capital input 
	RegionSizeNet[NR27]			= FILE("data/RegionSizeNet.dat"),	! Ha Usable area	BioLabourCost[NR27](t),				! $/manyear         Price of labour in a specific region
	BioRiskFactor[NR27](t)               	= FILE("data/biorisk.scn"),		! ? [multiplier?]        	
	FeedstockCostTransp[NCROP](t)     	= FILE("data/FeedstockTransportCosts.dat"),        ! US$/GJ           The costs of transporting feedstock from the production site to the conversion site. Non-woody same as woody
	BioLabourCost[NR27](t),				! $/manyear	    
	Bio_PL_pk[NR27](t),				! $/manyear         Ratio between price of labour and price of capital
	Bio_Pl_PKCDcoeff[NR27,NCROP](t),             	! $/manyear         Optimal ratio between the use of capital and labour
	CapitalLaborRatio_i[NR27,NCROP],             	! $/manyear         Ratio between initial capital input and inital labout input
        CapCD_ini[NR27,NCROP],				! Deflated capital input
        LaborRequiredAbon[NR27,NCROP](t),            	! manyear/Ha        Required labour input on Abondoned agricultural land
        LaborRequiredRest[NR27,NCROP](t),            	! manyear/Ha        Required labout input on rest lands
        LaborHoursAbon[NR27,NCROP](t),			! hr/Ha.yr
        LaborHoursRest[NR27,NCROP](t),			! hr/Ha.yr
        CapitalRequiredAbon[NR27,NCROP](t),          	! $/Ha              Required capital input on Abondoned agricultural land
        CapitalRequiredRest[NR27,NCROP](t),          	! $/Ha              Required capital input on rest lands
	BioAnnuity[NR27](t),
        LandPrice[NR27](t),				! $/Ha         Price of land, using GDP per capita as an indicator for income
	ECropProductivity[NC,NCROP](t),			! GJ/Ha
	ECropProductivity_NoMF[NC,NCROP](t);		! GJ/Ha
		
BioLabourCost[R]             = SWITCH(GDPpc[R] > EPS ? (4.2028 * GDPpc[R] ** -0.1787) * GDPpc[R] ELSE 0), R = 1 to NR27;
Bio_Pl_Pk[R]                 = SWITCH(discountrate[R] > EPS ? BioLabourCost[R] / discountrate[R] ELSE 0), R = 1 to NR27;
Bio_Pl_PKCDcoeff[R,CROP]     = Bio_Pl_Pk[R] * (CobbDouglasCoeff[CROP] / (1-CobbDouglasCoeff[CROP])), R = 1 to NR27, CROP = 1 to NCROP;
CapitalLaborRatio_i[R,CROP]  = SWITCH(LabCD_ini[R,CROP] > EPS ? CapCD_ini[R,CROP] / LabCD_ini[R,CROP] ELSE 0), R = 1 to NR27, CROP = 1 to NCROP;
CapCD_ini[R,CROP]	     = CapCD_ini_in[R,CROP] * 1.12, R = 1 TO NR27, CROP = 1 TO NCROP; 	! 1.12 is deflator as used in TIMER	
LaborRequiredAbon[R,CROP]    = SWITCH(LabCD_ini[R,CROP] > EPS ?  
                                           POW(CapCD_ini[R,CROP] / LabCD_ini[R,CROP], CobbDouglasCoeff[CROP])    * 
                                           POW(Bio_Pl_PKCDcoeff[R,CROP]             , (-CobbDouglasCoeff[CROP])) * 
                                           LabCD_ini[R,CROP]
                                      ELSE 0), R = 1 to NR27, CROP = 1 to NCROP;
LaborRequiredRest[R,CROP]    =  SWITCH(LabCD_ini[R,CROP] > EPS ?
                                            POW(CapCD_ini[R,CROP]/(LabCD_ini[R,CROP]), CobbDouglasCoeff[CROP])    *
                                            POW(Bio_Pl_PKCDcoeff[R,CROP]             , (-CobbDouglasCoeff[CROP])) *
                                            LabCD_ini[R,CROP]
                            	           ELSE 0), R = 1 to NR27, CROP = 1 to NCROP;
LaborHoursAbon[R,CROP]	= LaborRequiredAbon[R,CROP] * 8760, R = 1 TO NR27, CROP = 1 TO NCROP;	! hr/Ha.yr
LaborHoursRest[R,CROP]	= LaborRequiredRest[R,CROP] * 8760, R = 1 TO NR27, CROP = 1 TO NCROP;	! hr/Ha.yr	
CapitalRequiredAbon[R,CROP]  = Bio_Pl_PKCDcoeff[R,CROP] * LaborRequiredAbon[R,CROP], R = 1 to NR27, CROP = 1 to NCROP;
CapitalRequiredRest[R,CROP]  = Bio_Pl_PKCDcoeff[R,CROP] * LaborRequiredRest[R,CROP], R = 1 to NR27, CROP = 1 to NCROP;
BioAnnuity[R]                = SWITCH(discountrate[R] > 0.001 ? discountrate[R] / (1.0 - (1.0 + discountrate[R]) ** (-15)) ELSE 1), R=1 to NR27; ! Economic lifetime=15 years
LandPrice[R] 	        = SWITCH(RegionSizeNet[R] > EPS ? 1.4*(2.83 * ((GDPpc[R] * Pop[R]) / RegionSizeNet[R]) ** 0.5 ) ELSE EPS), R = 1 to NRC;

! Determine prices per crop
ECropProductivity[C,1]	= WOODYProductivityGJ[C], C = 1 TO NC;
ECropProductivity[C,2]	= MAIZEProductivityGJ[C], C = 1 TO NC;
ECropProductivity[C,3]	= SUGARProductivityGJ[C], C = 1 TO NC;
ECropProductivity[C,4]	= OilCropProductivityGJ[C], C = 1 TO NC;
ECropProductivity[C,5]	= NWOODProductivityGJ[C], C = 1 TO NC;

ECropProductivity_NoMF[C,1]	= WOODYProductivity[C], C = 1 TO NC;
ECropProductivity_NoMF[C,2]	= MAIZEProductivity[C], C = 1 TO NC;
ECropProductivity_NoMF[C,3]	= SUGARProductivity[C], C = 1 TO NC;
ECropProductivity_NoMF[C,4]	= OilCropProductivity[C], C = 1 TO NC;
ECropProductivity_NoMF[C,5]	= NWOODProductivity[C], C = 1 TO NC;


REAL	EnergyCropPrice[NC,NCROP](t);	! $/GJ
REAL	EnergyCropPrice_NoMF[NC,NCROP](t);	! $/GJ No Management factor

EnergyCropPrice[C,CROP]	= SWITCH(ECropProductivity[C,CROP] > EPS ?
				((LandPrice[Region[C]] * BioRiskFactor[Region[C]])/ECropProductivity[C,CROP]) +								! Land Cost
				(BioRiskFactor[Region[C]] * BioAnnuity[Region[C]] * AbonMult[C] * CapitalRequiredAbon[Region[C],CROP]/ECropProductivity[C,CROP]) +  	! Capital Costs (Abandoned)
				(BioRiskFactor[Region[C]] * BioAnnuity[Region[C]] * RestMult[C] * CapitalRequiredRest[Region[C],CROP]/ECropProductivity[C,CROP]) +  	! Capital Costs (Rest)
				(BioRiskFactor[Region[C]] * AbonMult[C] * BioLabourCost[Region[C]] * LaborRequiredAbon[Region[C],CROP]/ECropProductivity[C,CROP]) +  	! Labour Costs (Abandoned)
				(BioRiskFactor[Region[C]] * RestMult[C] * BioLabourCost[Region[C]] * LaborRequiredRest[Region[C],CROP]/ECropProductivity[C,CROP]) +  	! Labour Costs (Rest)
				FeedstockCostTransp[CROP]
			ELSE 0), C = 1 TO NC, CROP = 1 TO NCROP;

EnergyCropPrice_NoMF[C,CROP]	= SWITCH(ECropProductivity_NoMF[C,CROP] > EPS ?
				((LandPrice[Region[C]] * BioRiskFactor[Region[C]])/ECropProductivity_NoMF[C,CROP]) +								! Land Cost
				(BioRiskFactor[Region[C]] * BioAnnuity[Region[C]] * AbonMult[C] * CapitalRequiredAbon[Region[C],CROP]/ECropProductivity_NoMF[C,CROP]) +  	! Capital Costs (Abandoned)
				(BioRiskFactor[Region[C]] * BioAnnuity[Region[C]] * RestMult[C] * CapitalRequiredRest[Region[C],CROP]/ECropProductivity_NoMF[C,CROP]) +  	! Capital Costs (Rest)
				(BioRiskFactor[Region[C]] * AbonMult[C] * BioLabourCost[Region[C]] * LaborRequiredAbon[Region[C],CROP]/ECropProductivity_NoMF[C,CROP]) +  	! Labour Costs (Abandoned)
				(BioRiskFactor[Region[C]] * RestMult[C] * BioLabourCost[Region[C]] * LaborRequiredRest[Region[C],CROP]/ECropProductivity_NoMF[C,CROP]) +  	! Labour Costs (Rest)
				FeedstockCostTransp[CROP]
			ELSE 0), C = 1 TO NC, CROP = 1 TO NCROP;


! Maps of potentials at a given price constraint
REAL	BioPrice = 10,	!$/GJ bniomass price constraint
	EconPotPerCrop[NC,NCROP](t);
	
EconPotPerCrop[C,CROP]	= SWITCH(EnergyCropPrice[C,CROP] <= BioPrice ?
				ECropProductivity[C,CROP]
			ELSE 0), C = 1 TO NC, CROP = 1 TO NCROP;

EconPot1stGen[C]	= SWITCH( TechPot1stGen[C] > EPS ? MAX(EconPotPerCrop[C,2], EconPotPerCrop[C,3]) * ImplementationMap[C] * Area[C] * HaPerKm2, ELSE 0), C = 1 TO NC;
EconPot2ndGen[C]	= SWITCH( TechPot1stGen[C] > EPS ? MAX(EconPotPerCrop[C,1], EconPotPerCrop[C,5]) * ImplementationMap[C] * Area[C] * HaPerKm2, ELSE 0), C = 1 TO NC;

! Maps of biomass prices
REAL	BioPrice1stGen[NC](t),BioPrice2ndGen[NC](t);
REAL	BioPrice1stGen_NoMF[NC](t),BioPrice2ndGen_NoMF[NC](t);
	
BioPrice1stGen[C]	= SWITCH(MAIZEProductivityGJ[C] > SUGARProductivityGJ[C] ? EnergyCropPrice[C,2] ELSE EnergyCropPrice[C,3]), C = 1 TO NC;
BioPrice2ndGen[C]	= SWITCH(WOODYProductivityGJ[C] > NWOODProductivityGJ[C] ? EnergyCropPrice[C,1] ELSE EnergyCropPrice[C,5]), C = 1 TO NC;

BioPrice1stGen_NoMF[C]	= SWITCH(MAIZEProductivity[C] > SUGARProductivity[C] ? EnergyCropPrice_NoMF[C,2] ELSE EnergyCropPrice_NoMF[C,3]), C = 1 TO NC;
BioPrice2ndGen_NoMF[C]	= SWITCH(WOODYProductivity[C] > NWOODProductivity[C] ? EnergyCropPrice_NoMF[C,1] ELSE EnergyCropPrice_NoMF[C,5]), C = 1 TO NC;

! TOTAL POTENTIALS
Pot1stGenTot[1]	= LSUM(C = 1 TO NC, TheoPot1stGen[C]);
Pot1stGenTot[2]	= LSUM(C = 1 TO NC, TechPot1stGen[C]);
Pot1stGenTot[3]	= LSUM(C = 1 TO NC, EconPot1stGen[C]);

Pot2ndGenTot[1]	= LSUM(C = 1 TO NC, TheoPot2ndGen[C]);
Pot2ndGenTot[2]	= LSUM(C = 1 TO NC, TechPot2ndGen[C]);
Pot2ndGenTot[3]	= LSUM(C = 1 TO NC, EconPot2ndGen[C]);