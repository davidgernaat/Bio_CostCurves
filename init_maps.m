! Different fraction per land use class available for bio-energy


Impl_frac[i] = SWITCH(LT20 = 1 ? Impl_frac_inp[i] ELSE
			SWITCH(i <= 5 ? Impl_frac_inp[i],
			       i >5 AND i <= NLC -1 ? Impl_frac_inp[i+1] ELSE Impl_frac_inp[i])), i = 1 to NLC;

! Necessary equation for use for IMAGE 2.2 / IMAGE 2.3+ land cover category system


AbonCropLandMult[C] = 	SWITCH(AbonCropLand[C] = 1 ? 1 ELSE 0), C = 1 to NC;
AbonTimberMult[C] = 	SWITCH(AbonCropLand[C] = 2 ? 1 ELSE 0), C = 1 to NC;

! Excluded areas (urban, reserves, high altitudes etc).

REAL	AltitudeCutoff = 3000;
REAL	ImprodCutoff = 100;


BioReserve_new[C]	= MAX(0, MIN(BioReserve_in1[C]+BioReserve_in2[C]+BioReserve_in3[C],1)), C = 1 TO NC;

BioReserve= SWITCH(FLAGReserve = 0 ? 0,
		   FLAGReserve = 1 ? Bioreserve_old, 
		   FLAGReserve = 2 ? BioReserve_new,
		   FLAGReserve = 3 ? SWITCH(t <= 2005 ? 
		                           BioReserve_new 
		                     ELSE 
		                           BioReserve_new(2005)),
		   FLAGReserve = 4 ? Protected_fraction	
		   ELSE BioReserve_new);

REAL BioReserve2[NC](t);

BioReserve2[C] = MAX(Bioreserve_old[C],BioReserve_new[C]), C = 1 to NC;

AreaReserve[C] = MIN(1,Bioreserve[C])*Area[C], C = 1 to NC;

wghtfactAlt[C]	= SWITCH (Alt[C] > AltitudeCutoff ? 0 ELSE 1), C = 1 to NC;
AreaAltitude[C] = wghtfactAlt[C]  * Area[C], C = 1 to NC;

AreaUrban[C] = Buildup[C] * Area[C], C = 1 to NC;


AreaCorrect[C] = (AreaAltitude[C] - AreaUrban[C]) * SWITCH ( Bioreserve[C] < 0.01 ? 1 ELSE 0 ), C = 1 to NC;

InproductiveMult[C] = SWITCH( MAX(WOODYProductivity[C],NWOODProductivity[C],MAIZEProductivity[C],SUGARProductivity[C]) < ImprodCutoff ? 
				0 
			ELSE 1), C = 1 to NC; ! 0 = inproductive

! **** Extensive grasslands.... partly available.*****

! Extensive grasslands have been subdivided into 3 types by Bouwman et al.
! Here it is assumed that type 1 and type 2 or not available for biofuels
! type 3 is available.

AvExtGrassland[C] = SWITCH(FLAGEXTGR_COR = 1 ? 
			SWITCH( LandCover[C] = 2 AND Grazing[C] = 3 ? 1 ELSE 0) ELSE 0), C = 1 to NC;
NotAvExtGrassland[C] = SWITCH(FLAGEXTGR_COR = 1 ? 
				SWITCH( LandCover[C] = 2 ?
					SWITCH(AvExtGrassland[C] = 1 ? 
						0 
					ELSE 1) 
				ELSE 0) 
			ELSE 	SWITCH( LandCover[C] = 2 ? 
					1 
				ELSE 0)), C = 1 to NC;

!JVV 21-5-7 WAB biomassa considering landdegradation
! Based on GLASOD map.
! Here land degradation reduced to 3 categories.
! 1 not severely degraded; 2 degraded, 3 severely degraded.
! Last category assumed to be not available for biofuels.

LandDegMap[C]=SWITCH 	(FLAGDegr = 0 ? 1,
			 FLAGDegr = 1 ? SWITCH(Landdegrad[C]<3 ? 1,
					Landdegrad[C] = 3 ? 1
					ELSE 0),
			 FLAGDegr = 2 ? SWITCH(Landdegrad[C]<3 ? 1,
					Landdegrad[C] = 3 ? 0.5
					ELSE 0)
			 ELSE 1), C = 1 to NC;


LandDeg[I,C]= SWITCH (Landdegrad[C]=I-1 ? 1 ELSE 0), C = 1 to NC, I = 1 to LandDegCat;

!!JVV 21-5-7 WAB biomassa considering watershortages

CONST WatershortThreshold = 0.4;

Watershortindex[1,C]= SWITCH (Watershort[C]< 0.1 ? 1 ELSE 0), C = 1 to NC;
Watershortindex[2,C]= SWITCH (Watershort[C]>=0.1 AND Watershort[C]<0.2 ? 1 ELSE 0), C = 1 to NC;
Watershortindex[3,C]= SWITCH (Watershort[C]>=0.2 AND Watershort[C]<0.4 ? 1 ELSE 0), C = 1 to NC;
Watershortindex[4,C]= SWITCH (Watershort[C]>=0.4 ? 1 ELSE 0), C = 1 to NC;


WetLandAct = SWITCH(FLAGWetland = 1 ? 
		SWITCH(Area > 0 ? MAX(0,MIN(1,WetLand/Area)) ELSE 0) ELSE 0);

! Forest : Area = forest, 0 = not forest.


Forest[C] = SWITCH(LT20 = 1 ? SWITCH((LandCover[C] >= 9 AND LandCover[C] <= 14) or (LandCover[C] >= 19 AND LandCover[C] <= 20) ? 
				Area[C] ELSE 0) ELSE 0), C = 1 to NC;


ForestCor[C] = SWITCH(AreaReserve[C] > 0 ? 0 ELSE Forest[C]), C = 1 to NC;


ForestReg[R] = 0,R = 1 to NR27;
sumarray (ForestReg, NC, NR27, region, Forest);

ForestCorReg[R] = 0,R = 1 to NR27;
sumarray (ForestCorReg, NC, NR27, region, ForestCor);


! 2 categories of land are available. Abandoned ag / forest land (Abon) and other land (rest).
! The abon category can include abandoned forestry land based on FLAGABONFOREST.
! Restland is all land with potential that is not abon-land

AbonMult[C] = SWITCH(FLAGABONFOREST = 1 ? MAX(AbonCropLandMult[C], AbonTimberMult[C]) ELSE AbonCropLandMult[C]), C = 1 to NC;

RestMult[C] = SWITCH(AbonCropLandMult[C] > 0.1 OR  AbonTimberMult[C] > 0.1 ? 0 ELSE 
                    SWITCH( ImplementationMap[C] > 0.001 ? 
                          1 
                    ELSE 
                          0)
              ), C = 1 to NC;
              
LandAbon[C] = AbonMult[C] * ImplementationMap[C] * Area[C], C = 1 to NC;
LandAbonArea[C] =  AbonMult[C] * Area[C], C = 1 to NC;

LandREST[C] = RestMult[C]*ImplementationMap[C]* Area[C], C = 1 to NC;
LandRestArea[C] = RestMult[C]*Area[C], C = 1 to NC;

TotRestArea[R]= 0, R= 1 to NR27;
sumarray (TotRestArea, NC, NR27, region, LandREST);
TotArea2[1]=HaPerKm2*LSUM(R = 1 to NR27-1, TotRestArea[R]);
TotAbonArea[R]= 0, R = 1 to NR27;
sumarray (TotAbonArea, NC, NR27, region, LandAbon);
TotArea2[2]=HaPerKm2*LSUM(R = 1 to NR27-1, TotAbonArea[R]);

UrbanLand[R]= 0, R= 1 to NR27;
sumarray (UrbanLand, NC, NR27, region, AreaUrban);

! CHECK VARIABLE
INTEGER	AbonCheck[NC](t);

AbonCheck[C]	= SWITCH(AbonMult[C] = 1 AND LandCover[C] = 1 ? 1 ELSE 0), C = 1 TO NC;