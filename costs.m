! ****************** COSTS ******************
! Costs have the following componenets:
! Determine Cost Supply Curves on gric cell basis (ecological potential)
! 	1. Labour costs. Regional, $/manyear, from TIMER-Bioenergy. 
!		Divide by GJ/yr for each grid cell to get maps of labour costs, $/GJ. Thus lower yields have higher costs (affects management etc).
!		50% higher for forestry residues (?)
! 	2. Factor for distance from waterways + population density?
!
! For Process Residues:
! 1. Standard price from literature
!
! Difference between ecological potential and Available potential is removed from lowest cost classes

BioLabourCost[i]	= SWITCH(Region[i] > EPS ? BioLabourCost_in[Region[i]] ELSE 0), i = 1 TO NC;
	
TranspDist[i]	= SWITCH( PopD[i] > 200 ?
			20,
		ELSE TranspDistLookup(PopD[i])), i = 1 TO NC;
!********** AGRICULTURE RESIDUES ******************
! 1. Labour Costs
! 2. Harvest Cost
! 3. Operations Cost
! 4. Transport Cost
! Labour Costs

AgrLabCost[i]	= !SWITCH( AgrEcoPotEn[i,CROPS] > EPS ? 
		!	BioLabourCost[i]/AgrEcoPotEn[i,CROPS],
		!ELSE 0), i = 1 TO NC;	! $/GJ
		0, i =1 TO NC;
		
! ***HARVEST COSTS (PER KM^2)
! SOURCE: 2014 Iowa Farm Custom rate Survey
! 1. Chopping = 2854 $/km^2
! 2. Raking = 1569.113 $/km^2
! NOTE: All costs include fuel, repairs, depreciation, interest, labour, and all other machinery costs for the tractor and implement
AgrHarvestCost[i]	= SWITCH( AgrEcoPotEnpkm[i,CROPS] > EPS ? 
				(2854 + 1569.113)/AgrEcoPotEnpkm[i,CROPS],
			ELSE 0), i = 1 TO NC;


REAL	AgrHarvestCostCrop[NC,CROPS-1](t);	! $-2005/GJ Harvest Costs per crop based on Gallagher 2003
REAL	AgrHarvestCost2[NC](t);			! $-2005/GJ per grid cell

AgrHarvestCostCrop[i,CROP]	= SWITCH(AgrEcoPotEnpkm[i,CROP] > EPS AND BioLabourCost_in[2] > EPS ? 
				(4788.77 * (BioLabourCost[i]/BioLabourCost_in[2])) / AgrEcoPotEnpkm[i,CROP],
				!4788.77 / AgrEcoPotEnpkm[i,CROP],
			ELSE 0), i = 1 TO NC, CROP = 1 TO CROPS-1;
			
AgrHarvestCost2[i]	= SWITCH( LSUM(j = 1 TO CROPS-1, CropFrac[i,j]) > EPS ?
				LSUM(CROP = 1 TO CROPS-1, AgrHarvestCostCrop[i,CROP] * CropFrac[i,CROP])/LSUM(j = 1 TO CROPS-1, CropFrac[i,j])
			  ELSE 0), i = 1 TO NC;
		
! ***OPERATIONS COSTS (PER TON)
! 1. Baling 	= 22.5 $/t 	= 1.25 $/GJ 	(2014 Iowa Farm Custom rate Survey)
! 2. Nitrogen 	= 10.72		= 0.6		(Edwards, 2011, Estimating a value for corn stover)
! 3. Phosphate	= 3.06		= 0.17		(Edwards, 2011, Estimating a value for corn stover)
! 4. Potash	= 12.2		= 0.7		(Edwards, 2011, Estimating a value for corn stover)
! Total				= 2.72
AgrOperCost[i]	= SWITCH(AgrEcoPotTot[i] > EPS ? 1.25 + 0.6 + 0.17 + 0.7, ELSE 0), i = 1 TO NC;


REAL	AgrOperCostCrop[NC,CROPS-1](t);	! $/GJ  Hauling (0.07$/GJ) and fertilizer Costs based on Gallagher 2003
REAL	AgrOperCost2[NC](t);		! $/GJ
REAL	AgrFertCost[CROPS-1]	= 0.36, 0.38, 0.38, 0.37, 0.37, 0.37, 0.37, 	! $/GJ Food crops
				0.37, 0.37, 0.37, 0.37,				! $/GJ Biofuel crops
				0.37, 0.37, 0.37, 0.37;				! $/GJ Other Crops
AgrOperCostCrop[i,CROP]	= 0.07 + AgrFertCost[CROP], i = 1 TO NC, CROP = 1 TO CROPS-1;

AgrOperCost2[i]	= SWITCH( LSUM(j = 1 TO CROPS-1, CropFrac[i,j]) > EPS ?
				LSUM(CROP = 1 TO CROPS-1, AgrOperCostCrop[i,CROP] * CropFrac[i,CROP])/LSUM(j = 1 TO CROPS-1, CropFrac[i,j])
		ELSE 0), i = 1 TO NC;

! ***STORAGE COSTS 
! Storage and drying = 1.149$/GJ, Thompson & Tyner 2014
REAL	AgrStorCost[NC](t);

AgrStorCost[i]	= SWITCH( LSUM(j = 1 TO CROPS-1, CropFrac[i,j]) > EPS ? 1.149 ELSE 0), i = 1 TO NC;

! *** TRANSPORT COST 
! Transport Distance relative to population density
! Minimum transport distance is 20km
! Increasing linearly when popdens < 200, so that at a popdens of 100 distance is 60km. Approximate calibration for Iowa which is where data point comes from.
! Assume 0.2$/t per km transport cost (2014 Iowa Farm Custom rate Survey). ->  ~0.012 $/GJ/km
AgrTranspCost[i]	= SWITCH( AgrEcoPotEn[i,CROPS] > EPS ?
			TranspDist[i] * 0.0119
		ELSE 0), i = 1 TO NC;

! Set Cost Per Grid Cell ($/GJ)
AgrCost[i]	= AgrLabCost[i] + AgrHarvestCost[i] + AgrOperCost[i] + AgrTranspCost[i], i = 1 TO NC;


REAL 	AgrCost2[NC](t);	! $/GJ  based on Gallagher 2003	
!NOTE GRID CELLS HAVE ALREADY BEEN WIEGHTED ON CROP FRACTIONS SO IT IS OK TO JUST ADD $/GJ FOR EACH CROP PER GRID CELL
AgrCost2[i]	= AgrTranspCost[i] + AgrHarvestCost2[i] + AgrOperCost2[i] + AgrStorCost[i], i = 1 TO NC;
		
! Cost Components
REAL	CostComponentAgr[NC,4](t),	! $/GJ. 1. Harvest, 2. Operations, 3. Transport, 4. Total
	CostComponentFrac[NC,3](t);	! -

CostComponentAgr[i,1]	= AgrHarvestCost2[i], i = 1 TO NC;
CostComponentAgr[i,2]	= AgrOperCost2[i]+ AgrStorCost[i], i = 1 TO NC;
CostComponentAgr[i,3]	= AgrTranspCost[i], i = 1 TO NC;
CostComponentAgr[i,4]	= LSUM(j = 1 TO 3, CostComponentAgr[i,j]), i = 1 TO NC;

CostComponentFrac[i,j]	= SWITCH( CostComponentAgr[i,4] > EPS ?
				CostComponentAgr[i,j]/CostComponentAgr[i,4],
			ELSE 0), i = 1 TO NC, j = 1 TO 3;			

!********** FORESTRY RESIDUES ******************
! 1. Labour Costs (?)
! 2. Chipping/Compressing
! 3. Forwarding
! 4. Transport
! 5. Additional (Stumpage/overhead/other)

! Based on Junginger 2005. cost comparable to but slightly higher than Erikson 2010 

! 1. Labour Costs	NOT USED!!!
ForLabCost[i]	= SWITCH( ForEcoPotTot[i] > 0.0 ? 
			(BioLabourCost[i] * 1.5)/ForEcoPotTotEn[i], 
		ELSE 0), i = 1 TO NC;	! $/GJ

! 2. Chipping/Compressing
! Junginger 2005
REAL	ForChipCost_in = 1.5;	! $-GJ

ForChipCost[i]	=	SWITCH(BioLabourCost_in[11] > EPS AND ForEcoPotTot[i] > EPS ?
				MAX(0.5,ForChipCost_in * (BioLabourCost[i]/BioLabourCost_in[11]))
			ELSE 0), i = 1 TO NC;
			!ForChipCost_in, i = 1 TO NC;

! 3. Forwarding
! Junginger 2005, Eriksson 2010
REAL 	ForForwCost_in = 0.86;

ForForwCost[i]	=	SWITCH(BioLabourCost_in[11] > EPS AND ForEcoPotTot[i] > EPS ?
				MAX(0.3,ForForwCost_in * (BioLabourCost[i]/BioLabourCost_in[11]))
			ELSE 0), i = 1 TO NC;
			!ForForwCost_in, i = 1 TO NC;

! 4. Transport
! Same formulation as with agriculture
! According to Junginger 2005, transport costs ($/GJ/km) decrease with increasing distance:
! <40km = 0.229 Euro/GJ/km, 40-80km = 0.0134 Euro/GJ/km, >80km= 0.0115 Euro/GJ/km, 
! Assume  ~0.012 $/GJ/km 
ForTranspCost[i]	= SWITCH( ForEcoPotTot[i] > EPS AND ForEcoPotTot[i] > EPS ?
			TranspDist[i] * 0.0119
		ELSE 0), i = 1 TO NC;

! 5. Additional (Stumpage/overhead/other)
! Assume  ~0.6 $/GJ (Junginger 2005,   Eriksson 2010)
REAL 	ForAddiCost_in = 0.5;

ForAddiCost[i]	=	SWITCH(BioLabourCost_in[11] > EPS AND ForEcoPotTot[i] > EPS ?
				MAX(0.3,ForAddiCost_in * (BioLabourCost[i]/BioLabourCost_in[11]))
			ELSE 0), i = 1 TO NC;
			!ForAddiCost_in, i = 1 TO NC;

! Set Cost Per Grid Cell ($/GJ)
ForCost[i]	= ForChipCost[i] + ForForwCost[i] + ForTranspCost[i] + ForAddiCost[i], i = 1 TO NC;

! Cost Components
REAL	CostComponentFor[NC,4](t);	! $/GJ. 1. Chip+Forward, 2. Additional, 3. Transport, 4. Total
	
CostComponentFor[i,1]	= ForChipCost[i] + ForForwCost[i], i = 1 TO NC;
CostComponentFor[i,2]	= ForAddiCost[i], i = 1 TO NC;
CostComponentFor[i,3]	= ForTranspCost[i], i = 1 TO NC;
CostComponentFor[i,4]	= LSUM(j = 1 TO 3, CostComponentFor[i,j]), i = 1 TO NC;

!********** TOTAL RESIDUES ******************
ResidueCost[i]	= SWITCH( AgrEcoPotTot[i] > EPS OR ForEcoPotTot[i] > EPS ? 
			MAX(AgrCost2[i],ForCost[i])
		ELSE 0), i = 1 TO NC;

ResidueProcCost[R]	= 2, R = 1 TO NRC; 

!********** COST CURVES******************
! Set TOTAL Potential per grid cell (GJ)
REAL	ResCostCurvePot[NC](t);		! GJ Total ecological residue potential. Basis of cost curve
ResCostCurvePot[i]	= ForEcoPotTotEn[i] + AgrEcoPotEn[i,CROPS], i = 1 TO NC;		! GJ

! Determine total regional potential use for cost curve (based on ecological potential)
REAL 	ResCostCurvePotR1[NRC](t),	! GJ
	ResCostCurvePotR[NRC](t);	! GJ
ResCostCurvePotR1[R]	= 0, R = 1 TO NRC;
sumarray (ResCostCurvePotR1, NC, NRC, Region, ResCostCurvePot);						! Grid-to-region
ResCostCurvePotR[R]	= ResCostCurvePotR1[R], R = 1 TO 26;
ResCostCurvePotR[NRC]	= LSUM(R = 1 TO 26, ResCostCurvePotR1[R]);

! Create Cost Curve
REAL	ResCostCurve1[NRC,RCC](t);	! Cost categories, descending order - ecological potentials
REAL	ResCostCurve2[NRC,RCC](t);	! Reverse order of categories, sort in ascending order of costs
INTEGER	Pointer[NRC,RCC](t);		! (GJ) Each cost category has equal potential 
REAL	ResPotCurve1[NRC,RCC](t);	! Potentials of cost curve - ecological potentials
ResCostCurve1[R,PC]	= 0.0, R = 1 TO NRC, PC = 1 TO RCC;
Pointer[R,PC]		= 0.0, R = 1 TO NRC, PC = 1 TO RCC;
calcCostCurve  (Region,NRC,ResidueCost,ResCostCurvePot,RCC,ResCostCurve1,Pointer);
ResCostCurve2[R,PC]	= ResCostCurve1[R,RCC-PC+1], R = 1 TO NRC, PC = 1 TO RCC;
ResPotCurve1[R,PC]	= ResCostCurvePotR[R]/RCC, R = 1 TO NRC, PC = 1 TO RCC;

! Determine Difference from  Available Potential
REAL	ResEconUse[NRC](t),		! GJ Residues in lowest cost categories used in competiing uses
	ResTechCat1[NRC](t),		! (Real) Number of categories used for feed/fuel
	ResPotCurve2[NRC,RCC](t);	! GJ Residues per cost category. Demand for feed/fuel removed from lowest categories 
INTEGER	ResTechCat[NRC](t);		! Cost category in which technical potential becomes available

ResEconUse[R]		= MAX(0,ResCostCurvePotR[R] - ResiduesTotal[R,NRES,4]), R = 1 TO NRC;
ResTechCat1[R]		= SWITCH( ResPotCurve1[R,1] > 0 ? 
				ResEconUse[R]/ResPotCurve1[R,1] + 1, 
			ELSE 1), R = 1 TO NRC;
ResTechCat[R]		= MAX(1,MIN(RCC, FLOOR(ResTechCat1[R]))), R = 1 TO NRC;

ResPotCurve2[R,PC]	= SWITCH( PC < ResTechCat[R] ?					! If cost category used for feed/fuel
				0							! = 0
			ELSE 	SWITCH( PC = ResEconUse[R] ?				! If category is where residues become technically available
				(1-(ResTechCat1[R] - ResTechCat[R])) * ResPotCurve1[R,PC]	! = Remove fraction of category use for feed/fuel
				ELSE	ResPotCurve1[R,PC]				! Else use potential from cost curve
				)
			), R = 1 TO NRC, PC = 1 TO RCC;
			
! Add Process Residues To cost Curves
INTEGER	ProcCat1[NRC,RCC](t);
INTEGER ProcCat2[NRC](t);

	! First determine which category process residues go into

ProcCat1[R,PC]	= SWITCH(ResidueProcCost[R] > ResCostCurve2[R,PC] ?
			1
		ELSE 0), R = 1 TO NRC, PC = 1 TO RCC;	! All categories which are cheaper that process residues get a 1, else 0
ProcCat2[R]	= LSUM(PC = 1 TO RCC, ProcCat1[R,PC]) + 1, R = 1 TO NRC;	! Determine cost category process residues get in

ResCostCurve[R,PC]	= SWITCH( PC < ProcCat2[R] ?
				ResCostCurve2[R,PC],
			ELSE	SWITCH( PC = ProcCat2[R] ?
					ResidueProcCost[R],
				ELSE ResCostCurve2[R,PC-1]
				)
			), R = 1 TO 26, PC = 1 TO RCC+1;

ResCostcurve[NRC,PC]	= 100 * PC/(RCC+1), PC = 1 TO RCC+1;	! Add dummy cost curve in order to avoid 0s

ResPotCurve[R,PC]	= SWITCH( PC < ProcCat2[R] ?
				ResPotCurve2[R,PC],
			ELSE	SWITCH( PC = ProcCat2[R] ?
					AgrProcPotREnTot[R] + ForProcPot[R],
				ELSE ResPotCurve2[R,PC-1]
				)
			), R = 1 TO NRC, PC = 1 TO RCC+1;

