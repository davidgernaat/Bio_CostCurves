! This module determines the availability of agricultural and forestry residues based on IMAGE projections
! Developed by: Vassilis Daioglou
! Date:		April-August 2014
! Literature:	See database "Residues Database.xlsx"
! Publication:	Daioglou, Stehfest, Wicke, Faaij, van Vuuren, 2015, Projections of the availability and cost reduction from agriculture and forestry, GCB Bioenergy
! Used by:	Biomass tool, B2T27.m

! ****************** METHODOLOGY ******************
! THEORETICAL POTENTIAL (TheoPot)
! 1. Agricultural residues: Use relationship for residues-to-product ratio (RPR)with respect to yield, for all crops.
! Include process residues praction, independent of yield
! 2. Forestry: Use residue-fraction of total weight 
! ECOLOGICAL POTENTIAL (EcoPot)
! Determine requirements of residues to maintain soil conditions
! Based on removable fractions based on literature review.
! Suggested Improvement: Relate to SOC levels of soils.
! TECHNICAL POTENTIAL (TechPot)
! Take away other uses as determined from IMAGE/TIMER
! 1. Agricultural residues: Take away demand for animal bedding/feed/fodder
! 2. Agriculture and Forestry residues: Traditional fuel use
!
! All Calculations done on a grid cell and regional level
! ********************* DEFINITIONS ****************************
#INCLUDE Residues_Settings.m

#ifdef ResiduesStandAlone
	extern C void sumarray (export float *, int, int, int *, float *);
	extern C void sumpartdim(export float **, int, int, int, int *, float **);
	extern C void calcCostCurve  (int *,int, float *,float *,int,export float **, export int **);

	T.MIN     = 1971.0;
	T.MAX     = 2100.0;
	T.STEP    = 10.0;
	T.SAMPLE  = 10.0;
	T.METHOD  = RK2;
	#INCLUDE const.m

	MODULE Main;
	BEGIN

#else
	MODULE Residues;
	BEGIN
#endif
! ****************** DECLARATION O VARIABLES AND INPUTS ******************
DOUBLE 	pd;				! population density dimension for transport distance lookup table

REAL
! Theoretical Potential
	FracOthC[NRC](t),		! Fraction of grid cell which is "other crops", i.e. non-NFC
	FracOthCSpec[NRC,4],		! Fraction of FracOthC_in which is 1. Fibre, 2. Fruit, 3. Vegetable, 4. Sugarcane. Based on FAO data extracted on 2/7/14
	FracHarv[NRC](t),
	CropFrac_Act[NC,CROPT](t),	! Actual fraction of grid cell for crops, accounting for "other crops"
	CropFracCheck1[NC](t),		! Check for input crop fractions
 	FracOthC1[NC](t),		! Fraction other crop, per agriculture grid cell
	CropFracCheck[NC](t),		! Check for crop fractions after including other crops
	CropFracTot[NC](t),		! Fraction of grid cell which has crops
	CropProdAct[NC,CROPT](t),	! t, Actual Production of crops
	CropProdR1[NRC,CROPT](t),	! t-wet
	CropProdR[NRC,NFC+5](t),	! t-wet
		CropProd2[NC,CROPT](t),		!t-wet other formulation
		CropProdR2[NRC,CROPT](t),	!t-wet
		CropProdRAct[NRC,NFC+5](t),	!t-wet
	AgrRPR[NC,CROPT](t),		! kg-res/kg-product. Potential, i.e. crop may not exist on grid cell
	AgrTheoPot1[NC,CROPT2](t),	! t-wet potential theoretical agricultural residues from field operations. Rainfed, Irrigated, Biofuel and Other Crops
	AgrTheoPotTot[NC](t),		! t-wet Potential theoretical agri-residues from field operation, all crops.
	AgrTheoPotTotpkm[NC](t),	! t-wet/km
	AgrTheoPotTotR1[NRC](t),	! t-wet
	AgrTheoPotTotR[NRC](t),		! t-wet
	AgrTheoPotpkm[NC,CROPS](t),	! t-wet
	AgrTheoPot[NC,CROPS](t),	! t-wetPotential theoretical agri-residues from field operations. NFC + Biofuel Crops
	AgrTheoPotMS[NC,CROPS-1](t),	! - MS of agri-crops in theoretical potential	
	AgrTheoPotR1[NRC,CROPS](t),	! t Agricultural theoretical potential of residues per crop (NFC+Biofuel crops)
	AgrTheoPotR[NRC,CROPS](t),	! t Same but with totals for Crops
	AgrTheoPotDryR[NRC,CROPS](t),	! t-dry
	AgrTheoPotREn[NRC,CROPS](t),	! GJ theoretical potential of agricultural residues from land operations
	
	ForTheoPotTotpkm[NC](t),
	ForTheoPotTot[NC](t),		! t
	ForTheoPotTotR1[NRC](t),
	ForTheoPotTotR[NRC](t),
	ForTheoPotTotREn[NRC](t),	! GJ
! Agricultural Process Residues
	AgrProcPot1[NC,CROPT](t),	! t-wet potential agricultural process residues 
	AgrProcPot[NC,CROPS](t),	! t-wet potential agricultural process residues. NFC + Biofuel Crops + Other crops
	AgrProcPotR1[NRC,CROPS](t),	! t-wet Agricultural potential of process residues per crop (NFC+Biofuel crops)
	AgrProcPotR[NRC,CROPS](t),	! t-wet Same but with totals for Crops
	AgrProcPottotR[NRC](t),		! t-wet
	AgrProcPotDryR[NRC,CROPS](t),	! t-dry
	AgrProcPotREn[NRC,CROPS](t),	! GJ
	AgrProcPotREnTot[NRC](t),	! GJ
! Forestry Process Residues
	ForProcPot1[NRC,2](t),		! GJ 1. P&P, 2. Sawlogs+veneer
	ForProcPot[NRC](t),		! GJ
! Sustainable Potential
	TotC[NC](t),			! kt
	SOC[NC](t),			! kt/km^2
	SOCtot[NC](t),			! kt SOC
	SOCtotR[NRC,9](t),
	AgrEcoPotTotpkm[NC](t),		! t-wet/km^2
	AgrEcoPotTot[NC](t),		! t-wet
	AgrEcoPot[NC,CROPS](t),		! t-wet
	AgrEcoPotEn[NC,CROPS](t),	! GJ
	AgrEcoPotpkm[NC,CROPS](t),	! t/km^2
	AgrEcoPotEnpkm[NC,CROPS](t),	! GJ/km^2
	AgrEcoPotR1[NRC,CROPS](t),	! t-wet
	AgrEcoPotTotR1[NRC](t),		! t-wet
	AgrEcoPotR[NRC,CROPS](t),	! t-wet
	AgrEcoPotTotR[NRC](t),		! t-wet Agricultural ecological potential of residues per crop (NFC+Biofuel crops)
	AgrEcoPotDry[NC,CROPS](t),	! t-dry map
	AgrEcoPotDryR[NRC,CROPS](t),	! t-dry	regional
	AgrEcoPotREn[NRC,CROPS](t),	! GJ
	AgrEcoPotMS[NRC,CROPS-1](t),	! - MS of agricultural crops in eco-potential (WET TERMS)
	AgrEcoUseR[NRC,CROPS,2](t),	! GJ, Residue use for 1.SOC, 2. Erosion. Per crop and total.
	ForEcoPotTotpkm[NC](t),
	ForEcoPotTotEnpkm[NC](t),	! GJ
	ForEcoPotTot[NC](t),
	ForEcoPotTotEn[NC](t),		! GJ
	ForEcoPotTotREn[NRC](t),	! GJ
	ForEcoPotTotR1[NRC](t),
	ForEcoPotTotR[NRC](t),	! t Forestry ecological potential of residues 
! Technical Potential
	FeedDemCrop[NRC,CROPS](t),	! t-dry Assume relative use of crops for feed based on thier availability
	FeedDem[NRC](t),		! t-dry
	FeedDemCropActEn[NRC,CROPS](t),	! GJ
	FeedDemActEn[NRC](t),		! GJ
	FeedDemFrac[NRC](t),		! Actual residues used for feed / IMAGE based results. Indicates how image may overestimate the availability of residues for feed.
	WoodHarvest[NRC,3](t),	! MgC/yr (cc, sc, wp)
	WoodHarvestMS[NRC,3](t),
	WoodDem[NRC,2](t),		! t of (round)wood demand for 1. pulpwood/particles, 2. sawlogs/veneer
 	WoodProd[NRC,3](t),		! t-wet (felling)wood production per cc, sc, wp
	TradFuelUse[NRC](t),		! GJ taditional fuel use which is residues
	TradFuelUseAct[NRC](t),		! GJ Actual traditional fuel use (in case TIMER values > than available potential)
	CropFrac[NC,CROPS-1](t),	! % of cell which is a specific agricultural crop, NFC + biofuel crops
!	AgrTechPot1[NRC,CROPS](t),	! t, agri-crops-residues Ecological potential + process residues
	AgrTechPotTotDryR[NRC](t),	! t-dry, agri-crops-residues available potential (accounting for feed)
	AgrTechPotDryR[NRC,CROPS](t),	! t-dry, per crop,  agri-crops-residues available potential (accounting for feed)
	AgrTechPotREn[NRC,CROPS](t),	! GJ	
! Costs
	BioLabourCost[NC](t),	! $/yr
	AgrLabCost[NC](t),	! $/GJ
	AgrHarvestCost[NC](t),	! $/GJ
	AgrOperCost[NC](t),	! $/GJ
	AgrTranspCost[NC](t),	! $/GJ 
	AgrCost[NC](t),		! $/GJ
	ForLabCost[NC](t),	! $/yr
	ForchipCost[NC](t),	! $/GJ
	ForForwCost[NC](t),	! $/GJ
	ForTranspCost[NC](t),	! $/GJ
	ForAddiCost[NC](t),	! $/GJ
	ForCost[NC](t),		! $/GJ
	TranspDist[NC](t),	
	ResidueCost[NC](t),		! $/GJ
	ResidueProcCost[NRC](t),	! $/GJ
	ResCostCurve[NRC,RCC+1](t),	! Final cost curve. RCC categories +1 for process residues
	ResPotCurve[NRC,RCC+1](t);	! Final Potential curve
! Outputs
REAL	ResiduesTotal[NRC,NRES,5](t);	! GJ all potentials

! ****DECLARATIONS FOR STANDALONE RUNNING****
#ifdef ResiduesStandAlone
	REAL
		Area[NC]				= FILE("../data/Residues/Area.dat"),		! km^2, size of grd cell
		Pop[NRC](t)				= FILE("../data/Residues/POP.scn");
	INTEGER
		Region[NC]                    		= FILE("../data/Residues/region27.dat");   	! cell-region conversion, input from IMAGE
#else
	IMPORT REAL
		Area[NC],
		Pop[NRC](t);
	IMPORT INTEGER
		Region[NC];

#endif

! ****INPUTS - MAPS****
REAL
	CropYield_in[NC,CROPT](t)		= FILE("../data/Residues/GRAPC.dat"),		! T-wet/km^2, crop yield
	CropFrac_in[NC,CROPT+1](t)		= FILE("../data/Residues/GFrac"),		! % fraction of grid cell which is crop. NOTE: 1st category is pasture! Sums to 1 (does not account "other crops")
	FracHarv_in[26](t)			= FILE("../data/Residues/FRHARV.out"),		! Harvest fraction
	CropProd_in[NC,CROPT](t)		= FILE("../data/Residues/GCropProd.dat"),		! T-wet, Volume of crop produced
	GBiomass_in[NC,9](t)			= FILE("../data/Residues/GBIOMASS.dat"),		! t/km^2 Carbon pools. 6+7 are soil pools (NOTE: Communication with jelle on 28/4/14 said that the unit is mg/km^2)
	PopD[NC](t)				= FILE("../data/Residues/gpopd.dat"),		! cap/km^2 (?)
	CellDist[NC]				= FILE("../data/Residues/celldist.dat");		! # of cells distance to water
INTEGER
	LandCoverType[NC](t)			= FILE("../data/Residues/GLCT.dat"),		! Land cover type
	PotVeg[NC](t)				= FILE("../data/Residues/GPotVeg.dat"),		! Potential vegetation
	ForCov[NC](t)				= FILE("../data/Residues/GForCov.dat"),		! forest coverage
	GForMan[NC,2](t)			= FILE("../data/Residues/GForMan.dat");

! ****INPUTS - REGIONAL****
REAL
	!AgriProcessRatio[NFC]		= FILE("input/AgriProcessRatio.dat");	! kg-res/kg-product from processing of agricultural products
	SE_Trad[NRC,NS,NTRAD](t)	= FILE("../data/Residues/SE_Trad.out"),		! GJ
	FracOthC_in[29](t)		= FILE("../data/Residues/FROTHC.scn"),		! Fraction of grid cell which is "Other crops"
	FracOthCspec_in[24,4]		= FILE("../data/Residues/FracOthCSpec.dat"),	! Fraction of FracOthC_in which is 1. Fibre, 2. Fruit, 3. Vegetable, 4. Sugarcane. Based on FAO data extracted on 2/7/14
	FeedDem_in[3,6,6,27](t)		= FILE("../data/Residues/TFeed.out"),		! kt-dm/yr feed for livestock
	WoodHarvest_in[5,4,27](t)	= FILE("../data/Residues/BiomHarvWood.out"),	! MgC/yr (cc, sc, wp)
	WoodDem_in[3,27](t)		= FILE("../data/Residues/WoodProd.out"),		! k-m^3/yr demand of wood products: 1. Pulp & particles, 2. Sawlogs/veneer/other, 3. Fuelwood/charcoal
	BioLabourCost_in[NRC](t)	= FILE("../data/Residues/BioLabourCost_TIMER.out"),! $/yr labour costs. Based on cobb-douglas production function (Labour/Capital) in TIMER: biofeed.m
	ExoResidues[NRC](t)		= FILE("../data/Residues/ExoResidues.dat");	! GJ, Exogenous residues from animal/human waste, household/municipal waste etc. (Berndes et al., 2003; Yamamoto et al, 1999; Smeets et al, 2007)
	
! ****INPUT - OTHER****
REAL	MoistureContent[NRES]			= FILE("../data/Residues/MoistureContent.dat");	! - Moisture content of different residue sources
REAL	TranspDistLookup(pd)			= FILE("../data/Residues/TranspDistLookup.dat");	! km Transport distance as a function of population density lookup table
REAL	ForResPotperVeg[20,3]			= FILE("../data/Residues/ForResPotperVeg.dat");	! Residue generation per year per forest type for 1. Clear Cut (Natural vegetation), Selective Cut, Wood Plantation (kg/Ha)
REAL	ForEcoFrac[20,3]			= FILE("../data/Residues/ForEcoFrac.dat");		! Residue removal rate (%) per forest type for 1. Clear Cut (Natural vegetation), Selective Cut, Wood Plantation 
REAL	RPRFac[2,CROPT]				= FILE("../data/Residues/RPRFac.dat");
REAL	RSRFac[4]				= FILE("../data/Residues/RSR.dat");		! t-wet/Ha residues for 1. Fribre, 2. Fruit, 3. Vegetables, 4. Sugarcane
REAL	CROPLHV[CROPS-1]			= 17.01, 14.09, 16.99, 16.114, 16.24, 14, 18.04,!Food Crops: temperate cereals, rice, maize, tropical cereals, pulses, roots and tubers, oil crops
						  16.66, 18.00, 18.00, 18.00, 			! Biofuel crops: sugarcane, maize, woody, non-woody 
						  16.57, 16.31, 13.61, 16.66;			! Other Crops: Fibre, fruit, vagetables, sugarcane
REAL	CROPHHV[CROPS-1]			= 18.26, 14.99, 18.165, 17.402, 17.46, 14.4, 17.61,
						  17.84, 18.00, 18.00, 18.00,
						  17.82, 18.10, 14.51, 17.84;	
REAL	FORLHV					= 16;
REAL	FORHHV					= 18;	
REAL	AgriProcessRatio[CROPT]			= 0.0, 0.2, 0.3, 0.0, 0.0, 0.0, 0.1,	
						  0.0,0.0,0.0,0.0,
						  0.0, 0.2, 0.3, 0.0, 0.0, 0.0, 0.1;
!REAL	AgriProcessRatio[CROPT]			= 0.0, 0.0, 0.0, 0.0, 0.0, 0.00, 0.0,	
!						  0.0,0.0,0.0,0.0,
!						  0.0, 0.0, 0.0, 0.0, 0.0, 0.00, 0.0;
REAL	EroConstAgr				= 200;		! t-wet/km^2 required to avoid dangerous erosion
REAL	ForProcessRatio[2]			= 0, 0.3;	! Koopmans 1997. Approx 20-50% of input lof is process residue, approx 50% available. Agrees with Yamamoto (2001), Smeets (2007) and Lauri (2014)
REAL	WoodDensity				= 0.5;	! t/m^3, http:/cedarstripkayak.wordpress.com/lumber-selection/162-2/
REAL	SOCConst				= 0; 	!SOC minimum constraint. kt/km^2 (*10 to get t/Ha)		DEACTIVATED

! **** INCLUDE MODULES ****
#INCLUDE agr.m
#INCLUDE for.m
#INCLUDE costs.m
#INCLUDE output.m
! ****************** PREPARATION ******************
! FRACTION OF "OTHER" CROPS
! Fraction (%) of grid cell which is "other crops"
FracOthC[R]		= FracOthC_in[R], R = 1 TO 26;
!FracOthC[25]		= FracOthC_in[18];
!FracOthC[26]		= FracOthC_in[10];
FracOthC[NRC]		= 0;

! Fraction (%) of other crops which are: 1. Fibre, 2. Fruit, 3. Vegetable, 4. Sugarcane
FracOthCSpec[R,CROP]	= FracOthCSpec_in[R,CROP], R = 1 TO 24, CROP = 1 TO 4;
FracOthCSpec[25,CROP]	= FracOthCSpec_in[18,CROP], CROP = 1 TO 4;
FracOthCSpec[26,CROP]	= FracOthCSpec_in[10,CROP], CROP = 1 TO 4;
FracOthCSpec[NRC,CROP]	= 0, CROP = 1 TO 4;

! Actual crop fractions
FracHarv[R]	= FracHarv_in[R], R = 1 TO 26;
FracHarv[NRC]	= 0;
CropFrac_Act[C,CROP]	= FracHarv[Region[C]] * CropFrac_in[C,CROP+1] * (1-FracOthC[Region[C]]), C = 1 TO NC, CROP = 1 TO CROPT; ! NOTE: CropFrac_in[C,1] is pasture, thus have to skip.

! Total Crop Fractions (all crops)
CropFracCheck1[i]	= LSUM(CROP = 1 TO CROPT, CropFrac_Act[i,CROP]), i = 1 TO NC;
FracOthC1[i]		= SWITCH(CropFracCheck1[i] > EPS ?
				FracOthC[Region[i]]
			ELSE 0), i = 1 TO NC;

CropFrac[i,CROP]	= CropFrac_Act[i,CROP] + CropFrac_Act[i,CROP+11], i = 1 TO NC, CROP = 1 TO NFC;
CropFrac[i,CROP]	= CropFrac_Act[i,CROP], i = 1 TO NC, CROP = NFC+1 TO NFC+4;
CropFrac[i,CROP]	= FracOthCSpec[Region[i],CROP-11] * FracOthC1[i], i = 1 TO NC, CROP = NFC+5 TO CROPS-1;

CropFracCheck[i]	= LSUM(CROP = 1 TO CROPS-1, CropFrac[i,CROP]), i = 1 TO NC;
CropFracTot[i]		= MIN(1,CropFracCheck[i]), i = 1 TO NC;

! DEMAND FOR FEED
! t-dry
! To be removed for agricultural ecological potential
FeedDem[R] 	= (FeedDem_in[3,3,6,R])*1000, R = 1 TO 26;				! Regions 1:9 the same
{
FeedDem[10]	= SWITCH ((Pop[10]+Pop[26]) > 0.0 ?				! Separate S.Africa from "Rest S.Africa" by population weight
			((Pop[10]/(Pop[10]+Pop[26])) * FeedDem_in[3,3,6,10])*1000
		ELSE 0);
FeedDem[R]	= (FeedDem_in[3,3,6,R])*1000, R = 11 TO 17;				! Regions 11:17 the same
FeedDem[18]	= SWITCH ((Pop[18]+Pop[25]) > 0.0 ?				! Separate india from "Rest S.Asia" by population weight
			((Pop[18]/(Pop[18]+Pop[25])) * FeedDem_in[3,3,6,18])*1000
		ELSE 0);
FeedDem[R]	= (FeedDem_in[3,3,6,R])*1000, R = 19 TO 24;				! Regions 19:24 the same
FeedDem[25]	= (FeedDem_in[3,3,6,18]*1000) - FeedDem[18];				! Remainder of Rest S.Asia allocated there
FeedDem[26]	= (FeedDem_in[3,3,6,10]*1000) - FeedDem[10];				! Remainder of Rest S.Africa allocated there
}
FeedDem[NRC]	= LSUM(R = 1 TO 26, FeedDem[R]);
FeedDemCrop[R,CROP]	= FeedDem[R] * AgrEcoPotMS[R,CROP], R = 1 TO 26, CROP = 1 TO CROPS-1;
FeedDemCrop[R,CROPS]	= LSUM(CROP = 1 TO CROPS-1, FeedDemCrop[R,CROP]), R = 1 TO 26;
FeedDemCrop[NRC,CROP]	= LSUM(R = 1 TO 26, FeedDemCrop[R, CROP]), CROP = 1 TO CROPS;

! WOOD HARVEST
! 1. cc, 2. sc, 3. wp
WoodHarvest[R,i] 	= WoodHarvest_in[i,4,R], R = 1 TO 26, i = 1 TO 3;					! Regions 1:9 the same
{WoodHarvest[10,i]	= SWITCH ((Pop[10]+Pop[26]) > 0.0 ?				! Separate S.Africa from "Rest S.Africa" by population weight
			(Pop[10]/(Pop[10]+Pop[26])) * WoodHarvest_in[i,4,10] 
		ELSE 0), i = 1 TO 3;
WoodHarvest[R,i]	= WoodHarvest_in[i,4,R], R = 11 TO 17, i = 1 TO 3;				! Regions 11:17 the same
WoodHarvest[18,i]	= SWITCH ((Pop[18]+Pop[25]) > 0.0 ?				! Separate india from "Rest S.Asia" by population weight
			(Pop[18]/(Pop[18]+Pop[25])) * WoodHarvest_in[i,4,18] 
		ELSE 0), i = 1 TO 3;
WoodHarvest[R,i]	= WoodHarvest_in[i,4,R], R = 19 TO 24, i = 1 TO 3;				! Regions 19:24 the same
WoodHarvest[25,i]	= (WoodHarvest_in[i,4,18]) - WoodHarvest[18,i], i = 1 TO 3;				! Remainder of Rest S.Asia allocated there
WoodHarvest[26,i]	= (WoodHarvest_in[i,4,10] ) - WoodHarvest[10,i], i = 1 TO 3;				! Remainder of Rest S.Africa allocated there
}
WoodHarvest[NRC,i]	= LSUM(R = 1 TO 26, WoodHarvest[R,i]), i = 1 TO 3;

WoodHarvestMS[R,i]	= SWITCH( LSUM(j = 1 TO 3, WoodHarvest[R,j]) > EPS ?
				WoodHarvest[R,i]/LSUM(j = 1 TO 3, WoodHarvest[R,j]),
			ELSE 1/3), R = 1 TO NRC, i = 1 TO 3;


! DEMAND FOR WOOD PRODUCTS
! NOTE: DOES NOT INCLUDE FUELWOOD. MAYBE SHOULD BE INCLUDED
! WoodDem_in: 1000m^3/yr
! WoodDem: t/yr
! 1. P&P, 2. Sawlags/veneer, 3. Fuelwood (not used here)
WoodDem[R,i] 	= WoodDem_in[i,R] * WoodDensity * 1000, R = 1 TO 26, i = 1 TO 2;					! Regions 1:9 the same
{WoodDem[10,i]	= SWITCH ((Pop[10]+Pop[26]) > 0.0 ?				! Separate S.Africa from "Rest S.Africa" by population weight
			(Pop[10]/(Pop[10]+Pop[26])) * WoodDem_in[i,10] * WoodDensity * 1000
		ELSE 0), i = 1 TO 2;
WoodDem[R,i]	= WoodDem_in[i,R] * WoodDensity * 1000, R = 11 TO 17, i = 1 TO 2;				! Regions 11:17 the same
WoodDem[18,i]	= SWITCH ((Pop[18]+Pop[25]) > 0.0 ?				! Separate india from "Rest S.Asia" by population weight
			(Pop[18]/(Pop[18]+Pop[25])) * WoodDem_in[i,18] * WoodDensity * 1000
		ELSE 0), i = 1 TO 2;
WoodDem[R,i]	= WoodDem_in[i,R] * WoodDensity * 1000, R = 19 TO 24, i = 1 TO 2;				! Regions 19:24 the same
WoodDem[25,i]	= (WoodDem_in[i,18] * WoodDensity * 1000) - WoodDem[18,i], i = 1 TO 2;				! Remainder of Rest S.Asia allocated there
WoodDem[26,i]	= (WoodDem_in[i,10] * WoodDensity * 1000) - WoodDem[10,i], i = 1 TO 2;				! Remainder of Rest S.Africa allocated there
}
WoodDem[NRC,i]	= LSUM(R = 1 TO 26, WoodDem[R,i]), i = 1 TO 2;

!  WOOD PRODUCED PER CC, SC, WP 
! Appendix 1, Table 20 of Arets et al: ~1.3m^3 of felling volume per m^3 of roundwood (EU)
WoodProd[R,i]	= (LSUM(j = 1 TO 2, WoodDem[R,j]) * WoodHarvestMS[R,i]) * 1.3, R = 1 TO NRC, i = 1 TO 3;	


!  TRADITIONAL FUEL USE
TradFuelUse[R]		= SE_Trad[R,3,3], R = 1 TO 26;
TradFuelUse[NRC]	= LSUM(R = 1 TO 26, TradFuelUse[R]);

! SOIL ORGANIC CARBON
TotC[i]		= (LSUM(j = 1 TO 9, GBiomass_in[i,j])/1000) * Area[i], i = 1 TO NC;
SOC[i]		= (GBiomass_in[i,6] + GBiomass_in[i,7])/1000, i = 1 TO NC;	! 	 kt/km^2 (Ref: http:/www.csiro.au/Outcomes/Environment/Australian-Landscapes/soil-carbon.aspx)
SOCtot[i]	= SOC[i] * Area[i], i = 1 TO NC; !kt
! Total SOC per region
SOCtotR[R,i]	= 0, R = 1 TO NRC, i = 1 TO 9;
sumpartdim (SOCtotR, NC, NRC, 9, Region, GBiomass_in);

! Potential Crop Production
REAL
CropPot[NC,CROPT](t);
CropPot[C,CROP]	= CropYield_in[C,CROP] * FracHarv[Region[C]], C = 1 TO NC, CROP = 1 TO CROPT;

END;