! Module Biomass for TIMER
! Monique Hoogwijk January 2004
! Detlef van Vuuren 2005-2008
! Jasper van Vliet
! Vassilis Daioglou 2014-2015 - Added non-woody crops, calculation of residues
!
! This module describes the yield area curve of woody and non-woody short rotation energy crops, maize and sugar cane.
! In TIMER, these curve will be converted into cost-supply curves
! The study of the potential assessment and the costs
! is described in Hoogwijk, Faaij, Eickhout, de Vries, Turkenburg and in Hoogwijk, Faaij, de Vries, Turkenburg, 2004 
! as well as in thesis Hoogwijk (2004) 
!
! Furthermore, this module decribes the available potential of agricultural and forestry residues
! These include harvest and process residues.
! Potentials based on agricultural and forestry production volumes and intensities
! Theoretical, ecological and available potentials determined 
! Cost curves determined based on harvest, collection and trasnport costs
! Method described in: Daioglou, Stehfest, Wicke, Faaij, van Vuuren, 2015, Projections of the availability and cost reduction from agriculture and forestry, GCB Bioenergy
! Residues are complemented by an exogenously set potential for human/animal waste, household and municipal waste. Base on literature (Berndes et al., 2003; Yamamoto et al, 1999; Smeets et al, 2007)

extern C void sumarray (export float *, int, int, int *, float *);
extern C void sumpartdim(export float **, int, int, int, int *, float **);
extern C void calcCostCurve  (int *,int, float *,float *,int,export float **, export int **);
! calcCostCurve(regio-kaart, aantal regios, te sorteren (x-as), te sorteren (y-as), aantal klassen, output variabele, pointer loos)
! calcCostCurve(integer, integer, real, real, integer, real, integer)


#INCLUDE constants.m
#INCLUDE residues/residues.m

! Time and Solution Declarations
t.min = 1970;
t.max = 2100;
t.step = 10;
t.sample = 10;
t.method = RK2;

! Beggining of main module
MODULE Main;
BEGIN;

! Assumption:
! All four types of energy crops are assumed to be planted on either rest land (REST) or abandoned agricultual land (Abon)
! Crops
! WOODY = woody
! NWOOD = Non-Woody (grasses)
! MAIZE = Maize
! SUGAR = sugarcane

! FLAGS

INTEGER IsTimerRun = 0;		!1=istimerrun 0=isnottimerrun
				!Use different Bioreservemaps than standard for timer, 1=no 0=yes
INTEGER	FLAGBioNoCons = 0;	!0 = bioenergy constraints are in place, 1 = Bioenergy planted EVERYWEHRE as dictated by implementation factors(including agri-land and forests)
INTEGER FLAGDegr = 0;		!1=on 0=off excludes degradated areas
INTEGER FLAGWater = 0;		!1=on 0=off excludes areas with water stress above 0.4
INTEGER FLAGReserve = 1;	!0=0 , 1= old map, 2 = new map, scenario, 3 = new map, stay constant in 2005, 4 = fractional map
INTEGER FLAGWetland = 0;	!1=on 0=off
INTEGER FLAGMFConst = 0;	!1=0 MF constant at 2015 level, 1=Management factor as in input file
INTEGER LT20 = 1; ! LT20 = 1 implies 20 land types in IMAGE, thus bio-energy as separate land type (at 6). The IMAGE 2.2 version and
		  ! older did not have bio-energy as separate type. While in most cases IMAGE input maps will not have bio-energy
		  ! included - still the index of each land type is shifted one upwards from IMAGE 2.3 onwards. 
		  ! LT20 = 0 sets the IMAGE 2.2 land use types.
		  ! LT20 is used in the variable impl_frac.
INTEGER	FLAGHHV = 1;		! 1=HHV 0=LLV
INTEGER FLAGABONFOREST = 1; !1= Includes abon forest land in abon category; 0 = Does not include abon forest land.
INTEGER FLAGEXTGR_COR = 1;	! Correction that assumes that part of extensive grassland are available for bio-energy use 
				! 0 = all extensive grassland is excluded, 1 = part of extensive grassland is included.	
INTEGER FLAGYield2005 = 0;	! 0 = Use normal yield projections, 1 = lock yields to 2005 levels
INTEGER ISIMIP = 0;		! 0 = Default IMAGE yields, 1 = ISIMIP yields
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Declaration of Variables!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#INCLUDE declaration.m
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Modules for TIMER residue curves!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Define residues module
Residues	res;	! Connect to residues calculations

! Data transfers
res.Pop		= Pop;
res.Area	= Area;
res.Region	= Region;

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!EQUATIONS for ENERGY CROPS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#INCLUDE init_maps.m
#INCLUDE scenland_info.m

! **************** AVAILABILITY FOR BIOENERGY PER GRID CELL ***************************************

ImplementationMap[C]	=
		SWITCH(FLAGBioNoCons = 1 ? Impl_frac_inp[LandCover[C]] ELSE	! No constraint condition, else:
		SWITCH(FLAGWater = 1 AND Watershort[C] >= 0.4 ? 0 ELSE		! Exclude if water short
		SWITCH(NotAvExtGrassland[C]=1 ? 0 ELSE				! Exclude grassland deemed unavailable
		SWITCH(LandCover[C] = 1 ? 0 ELSE				! Exclude if it is agriculture
		SWITCH( InproductiveMult[C] = 0 OR wghtfactAlt[C] = 0 ? 0 ELSE	! Exclude if unproductive or too high altitude
		SWITCH(	AbonMult[C]>=0.5 ? LandDegMap[C]*(1-WetLandAct[C])*(1-BioReserve[C])*(1-Buildup[C])*Impl_frac_inp[6],		! If abandoned exclude reserves
			AvExtGrassland[C]=1 ? LandDegMap[C]*(1-WetLandAct[C])*(1-BioReserve[C])*(1-Buildup[C])*Impl_frac_inp[2] ELSE	! Else if grassland exclude reserves
		SWITCH( LandCover[C]>6 ? LandDegMap[C]*(1-WetLandAct[C])*(1-BioReserve[C])*(1-Buildup[C])*Impl_frac_inp[LandCover[C]] ELSE 0))))))), C = 1 to NC;	! If landcover > 6, exclude reserves			

ImplementationMap2[C]	=
		SWITCH(FlagBioNoCons = 1 ? Impl_frac_inp[LandCover[C]] ELSE
		SWITCH(FLAGWater = 1 AND Watershort[C] >= 0.4 ? 1 ELSE
		SWITCH(NotAvExtGrassland[C]=1 ? 2 ELSE
		SWITCH(LandCover[C] = 1 ? 3 ELSE
		SWITCH( InproductiveMult[C] = 0 OR wghtfactAlt[C] = 0 ? 4 ELSE
		SWITCH(	AbonMult[C]>=0.5 ? 5,
			AvExtGrassland[C]=1 ? 6 ELSE
		SWITCH( LandCover[C]>6 ? 7 ELSE 0))))))), C = 1 to NC;			

! CHECKS
REAL AbonMultCheck[NC](t);	! Checks to see if land is really abandoned

AbonMultCheck[C]	= SWITCH( AbonMult[C] = 1 AND LandCover[C] = 1 ? 2	! If both abandoned and agri = 2
				ELSE	SWITCH(AbonMult[C] = 1 ? 1		! If just abandoned = 1
					ELSE 0)), C = 1 TO NC;
REAL	AbonProb[NC](t);
REAL	AbonProbCount[NR27](t);

AbonProb[C]	= SWITCH(AbonMultCheck[C] = 2 ? 1 ELSE 0), C = 1 TO NC;
AbonProbCount[R]	= 0, R = 1 TO NR27;
sumarray (AbonProbCount, NC, NR27, region, AbonProb);
AbonProbCount[NR27] = LSUM(R = 1 TO 26, AbonProbCount[R]);

! END CHECKS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!CROP EQUATIONS FOR TIMER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

HValueWood 	= SWITCH(FLAGHHV = 1 ? HHVWood ELSE LHVWood);
HValueNWood	= SWITCH(FLAGHHV = 1 ? HHVNonWoody ELSE LHVNonWoody);
HValueMaize 	= SWITCH(FLAGHHV = 1 ? HHVMaize ELSE LHVMaize);
HValueSugar 	= SWITCH(FLAGHHV = 1 ? HHVSugar ELSE LHVSugar);
HValueOilCrop 	= SWITCH(FLAGHHV = 1 ? HHVOilCrop ELSE LHVOilCrop);

! Modules for TIMER crop curves:
#INCLUDE productivity_maps.m
#INCLUDE timer_curves.m
#INCLUDE output.m

! Other Modules
#INCLUDE carbon_consequences.m ! Not active

END;