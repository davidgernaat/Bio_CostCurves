!! ABONDONED LAND !!

!Calculate the Yield curves for Abandoned-lands
REAL 	WOODYyieldcurveAbon[NR27,NPC](t);
WOODYyieldcurveAbon[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
INTEGER	WOODYPointerAbon[NR27,NPC](t);
WOODYPointerAbon[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
calcCostCurve  (region,NR27,WOODYProductivityAbon,LandAbon,NPC,WOODYyieldcurveAbon,WOODYPointerAbon);

REAL 	NWOODyieldcurveAbon[NR27,NPC](t);
NWOODyieldcurveAbon[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
INTEGER	NWOODPointerAbon[NR27,NPC](t);
NWOODPointerAbon[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
calcCostCurve  (region,NR27,NWOODProductivityAbon,LandAbon,NPC,NWOODyieldcurveAbon,NWOODPointerAbon);

REAL 	MAIZEyieldcurveAbon[NR27,NPC](t);
MAIZEyieldcurveAbon[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
INTEGER	MAIZEPointerAbon[NR27,NPC](t);
MAIZEPointerAbon[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
calcCostCurve  (region,NR27,MAIZEProductivityAbon,LandAbon,NPC,MAIZEyieldcurveAbon,MAIZEPointerAbon);

REAL 	SUGARyieldcurveAbon[NR27,NPC](t);
SUGARyieldcurveAbon[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
INTEGER	SUGARPointerAbon[NR27,NPC](t);
SUGARPointerAbon[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
calcCostCurve  (region,NR27,SUGARProductivityAbon,LandAbon,NPC,SUGARyieldcurveAbon,SUGARPointerAbon);

REAL 	OilcropyieldcurveAbon[NR27,NPC](t);
OilcropyieldcurveAbon[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
INTEGER	OilcropPointerAbon[NR27,NPC](t);
OilcropPointerAbon[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
calcCostCurve  (region,NR27,OilcropProductivityAbon,LandAbon,NPC,OilcropyieldcurveAbon,OilcropPointerAbon);

! Calculate maximum available land
REAL	WOODYMaxTechLandAbon[NR27](t);
WOODYMaxTechLandAbon[R] = 0,R = 1 to NR27;
sumarray (WOODYMaxTechLandAbon, NC, NR27, region, LandAbon);

!Sum the potentials on Abandoned-lands from cells to regions
REAL WOODYTotPotentialAbon[NR27](t);
WOODYTotPotentialAbon[R] = 0,R = 1 to NR27;
sumarray (WOODYTotPotentialAbon, NC, NR27, region, WOODYProdPerCellAbon);

REAL NWOODTotPotentialAbon[NR27](t);
NWOODTotPotentialAbon[R] = 0,R = 1 to NR27;
sumarray (NWOODTotPotentialAbon, NC, NR27, region, NWOODProdPerCellAbon);

REAL MAIZETotPotentialAbon[NR27](t);
MAIZETotPotentialAbon[R] = 0,R = 1 to NR27;
sumarray (MAIZETotPotentialAbon, NC, NR27, region, MAIZEProdPerCellAbon);

REAL SUGARTotPotentialAbon[NR27](t);
SUGARTotPotentialAbon[R] = 0,R = 1 to NR27;
sumarray (SUGARTotPotentialAbon, NC, NR27, region, SUGARProdPerCellAbon);

REAL OilcropTotPotentialAbon[NR27](t);
OilcropTotPotentialAbon[R] = 0,R = 1 to NR27;
sumarray (OilcropTotPotentialAbon, NC, NR27, region, OilcropProdPerCellAbon);

!Smooth the Yield Curves for Abandonend-lands
REAL WOODYyieldcurveAbonSmth[NR27,NPC](t);
WOODYyieldcurveAbonSmth[R,PC] = SWITCH(WOODYTotPotentialAbon[R] < 1e7 ? 0 ELSE
				SWITCH(	
				PC = 1 ? LSUM(j = 1 TO 2, WOODYyieldcurveAbon[R,j])/2,
				PC = 2 ? LSUM(j = 1 TO 3, WOODYyieldcurveAbon[R,j])/3,
				PC = 3 ? LSUM(j = 1 TO 5, WOODYyieldcurveAbon[R,j])/5,
				PC = NPC-2 ? LSUM(j = 0 TO 4, WOODYyieldcurveAbon[R,NPC-j])/5,
				PC = NPC-1 ? LSUM(j = 0 TO 2, WOODYyieldcurveAbon[R,NPC-j])/3,
				PC = NPC ? WOODYyieldcurveAbon[R,NPC],
				ELSE LSUM(j = 1 TO 7, WOODYyieldcurveAbon[R,PC+4-j])/7)), R = 1 to NR27, PC = 1 to NPC;

REAL NWOODyieldcurveAbonSmth[NR27,NPC](t);
NWOODyieldcurveAbonSmth[R,PC] = SWITCH(NWOODTotPotentialAbon[R] < 1e7 ? 0 ELSE
				SWITCH(	
				PC = 1 ? LSUM(j = 1 TO 2, NWOODyieldcurveAbon[R,j])/2,
				PC = 2 ? LSUM(j = 1 TO 3, NWOODyieldcurveAbon[R,j])/3,
				PC = 3 ? LSUM(j = 1 TO 5, NWOODyieldcurveAbon[R,j])/5,
				PC = NPC-2 ? LSUM(j = 0 TO 4, NWOODyieldcurveAbon[R,NPC-j])/5,
				PC = NPC-1 ? LSUM(j = 0 TO 2, NWOODyieldcurveAbon[R,NPC-j])/3,
				PC = NPC ? NWOODyieldcurveAbon[R,NPC],
				ELSE LSUM(j = 1 TO 7, NWOODyieldcurveAbon[R,PC+4-j])/7)), R = 1 to NR27, PC = 1 to NPC;

REAL MAIZEyieldcurveAbonSmth[NR27,NPC](t);
MAIZEyieldcurveAbonSmth[R,PC] = SWITCH(MAIZETotPotentialAbon[R] < 1e7 ? 0 ELSE
				SWITCH(	
				PC = 1 ? LSUM(j = 1 TO 2, MAIZEyieldcurveAbon[R,j])/2,
				PC = 2 ? LSUM(j = 1 TO 3, MAIZEyieldcurveAbon[R,j])/3,
				PC = 3 ? LSUM(j = 1 TO 5, MAIZEyieldcurveAbon[R,j])/5,
				PC = NPC-2 ? LSUM(j = 0 TO 4, MAIZEyieldcurveAbon[R,NPC-j])/5,
				PC = NPC-1 ? LSUM(j = 0 TO 2, MAIZEyieldcurveAbon[R,NPC-j])/3,
				PC = NPC ? MAIZEyieldcurveAbon[R,NPC],
				ELSE LSUM(j = 1 TO 7, MAIZEyieldcurveAbon[R,PC+4-j])/7)), R = 1 to NR27, PC = 1 to NPC;
				
REAL SUGARyieldcurveAbonSmth[NR27,NPC](t);
SUGARyieldcurveAbonSmth[R,PC] = SWITCH(SUGARTotPotentialAbon[R] < 1e7 ? 0 ELSE
				SWITCH(	
				PC = 1 ? LSUM(j = 1 TO 2, SUGARyieldcurveAbon[R,j])/2,
				PC = 2 ? LSUM(j = 1 TO 3, SUGARyieldcurveAbon[R,j])/3,
				PC = 3 ? LSUM(j = 1 TO 5, SUGARyieldcurveAbon[R,j])/5,
				PC = NPC-2 ? LSUM(j = 0 TO 4, SUGARyieldcurveAbon[R,NPC-j])/5,
				PC = NPC-1 ? LSUM(j = 0 TO 2, SUGARyieldcurveAbon[R,NPC-j])/3,
				PC = NPC ? SUGARyieldcurveAbon[R,NPC],
				ELSE LSUM(j = 1 TO 7, SUGARyieldcurveAbon[R,PC+4-j])/7)), R = 1 to NR27, PC = 1 to NPC;

REAL OilcropyieldcurveAbonSmth[NR27,NPC](t);
OilcropyieldcurveAbonSmth[R,PC] = SWITCH(OilcropTotPotentialAbon[R] < 1e7 ? 0 ELSE
				SWITCH(	
				PC = 1 ? LSUM(j = 1 TO 2, OilcropyieldcurveAbon[R,j])/2,
				PC = 2 ? LSUM(j = 1 TO 3, OilcropyieldcurveAbon[R,j])/3,
				PC = 3 ? LSUM(j = 1 TO 5, OilcropyieldcurveAbon[R,j])/5,
				PC = NPC-2 ? LSUM(j = 0 TO 4, OilcropyieldcurveAbon[R,NPC-j])/5,
				PC = NPC-1 ? LSUM(j = 0 TO 2, OilcropyieldcurveAbon[R,NPC-j])/3,
				PC = NPC ? OilcropyieldcurveAbon[R,NPC],
				ELSE LSUM(j = 1 TO 7, OilcropyieldcurveAbon[R,PC+4-j])/7)), R = 1 to NR27, PC = 1 to NPC;

!! REST LAND !!

!Calculate the Yield curves for Rest-lands

REAL 	WOODYyieldcurveREST[NR27,NPC](t);
WOODYyieldcurveREST[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
INTEGER	WOODYPointerREST[NR27,NPC](t);
WOODYPointerREST[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
calcCostCurve  (region,NR27,WOODYProductivityREST,LandREST,NPC,WOODYyieldcurveREST,WOODYPointerREST);

REAL 	NWOODyieldcurveREST[NR27,NPC](t);
NWOODyieldcurveREST[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
INTEGER	NWOODPointerREST[NR27,NPC](t);
NWOODPointerREST[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
calcCostCurve  (region,NR27,NWOODProductivityREST,LandREST,NPC,NWOODyieldcurveREST,NWOODPointerREST);

REAL 	MAIZEyieldcurveREST[NR27,NPC](t);
MAIZEyieldcurveREST[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
INTEGER	MAIZEPointerREST[NR27,NPC](t);
MAIZEPointerREST[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
calcCostCurve  (region,NR27,MAIZEProductivityREST,LandREST,NPC,MAIZEyieldcurveREST,MAIZEPointerREST);

REAL 	SUGARyieldcurveREST[NR27,NPC](t);
SUGARyieldcurveREST[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
INTEGER	SUGARPointerREST[NR27,NPC](t);
SUGARPointerREST[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
calcCostCurve  (region,NR27,SUGARProductivityREST,LandREST,NPC,SUGARyieldcurveREST,SUGARPointerREST);

REAL 	OilcropyieldcurveREST[NR27,NPC](t);
OilcropyieldcurveREST[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
INTEGER	OilcropPointerREST[NR27,NPC](t);
OilcropPointerREST[R,i] = 0, R = 1 to NR27, i = 1 to NPC;
calcCostCurve  (region,NR27,OilcropProductivityREST,LandREST,NPC,OilcropyieldcurveREST,OilcropPointerREST);

! Calculate maximum available land
REAL	WOODYMaxTechLandREST[NR27](t);
WOODYMaxTechLandREST[R] = 0,R = 1 to NR27;
sumarray (WOODYMaxTechLandREST, NC, NR27, region, LandREST);

!Sum the potentials on Rest-lands from cells to regions
REAL WOODYTotPotentialREST[NR27](t);
WOODYTotPotentialREST[R] = 0,R = 1 to NR27;
sumarray (WOODYTotPotentialREST, NC, NR27, region, WOODYProdPerCellREST);

REAL NWOODTotPotentialREST[NR27](t);
NWOODTotPotentialREST[R] = 0,R = 1 to NR27;
sumarray (NWOODTotPotentialREST, NC, NR27, region, NWOODProdPerCellREST);

REAL MAIZETotPotentialREST[NR27](t);
MAIZETotPotentialREST[R] = 0,R = 1 to NR27;
sumarray (MAIZETotPotentialREST, NC, NR27, region, MAIZEProdPerCellREST);

REAL SUGARTotPotentialREST[NR27](t);
SUGARTotPotentialREST[R] = 0,R = 1 to NR27;
sumarray (SUGARTotPotentialREST, NC, NR27, region, SUGARProdPerCellREST);

REAL OilcropTotPotentialREST[NR27](t);
OilcropTotPotentialREST[R] = 0,R = 1 to NR27;
sumarray (OilcropTotPotentialREST, NC, NR27, region, OilcropProdPerCellREST);

!Smooth the Yield Curves for Rest-lands

REAL WOODYyieldcurveRESTSmth[NR27,NPC](t);
WOODYyieldcurveRESTSmth[R,PC] = SWITCH(WOODYTotPotentialREST[R] < 1e7 ? 0 ELSE
                                SWITCH(
                                PC = 1 ? LSUM(j = 1 TO 2, WOODYyieldcurveREST[R,j])/2,
				PC = 2 ? LSUM(j = 1 TO 3, WOODYyieldcurveREST[R,j])/3,
				PC = 3 ? LSUM(j = 1 TO 5, WOODYyieldcurveREST[R,j])/5,
				PC = NPC-2 ? LSUM(j = 0 TO 4, WOODYyieldcurveREST[R,NPC-j])/5,
				PC = NPC-1 ? LSUM(j = 0 TO 2, WOODYyieldcurveREST[R,NPC-j])/3,
				PC = NPC ? WOODYyieldcurveREST[R,NPC],
				ELSE LSUM(j = 1 TO 7, WOODYyieldcurveREST[R,PC+4-j])/7)), R = 1 to NR27, PC = 1 to NPC;

REAL NWOODyieldcurveRESTSmth[NR27,NPC](t);
NWOODyieldcurveRESTSmth[R,PC] = SWITCH(NWOODTotPotentialREST[R] < 1e7 ? 0 ELSE
                                SWITCH(
                                PC = 1 ? LSUM(j = 1 TO 2, NWOODyieldcurveREST[R,j])/2,
				PC = 2 ? LSUM(j = 1 TO 3, NWOODyieldcurveREST[R,j])/3,
				PC = 3 ? LSUM(j = 1 TO 5, NWOODyieldcurveREST[R,j])/5,
				PC = NPC-2 ? LSUM(j = 0 TO 4, NWOODyieldcurveREST[R,NPC-j])/5,
				PC = NPC-1 ? LSUM(j = 0 TO 2, NWOODyieldcurveREST[R,NPC-j])/3,
				PC = NPC ? NWOODyieldcurveREST[R,NPC],
				ELSE LSUM(j = 1 TO 7, NWOODyieldcurveREST[R,PC+4-j])/7)), R = 1 to NR27, PC = 1 to NPC;


REAL MAIZEyieldcurveRESTSmth[NR27,NPC](t);
MAIZEyieldcurveRESTSmth[R,PC] = SWITCH(MAIZETotPotentialREST[R] < 1e7 ? 0 ELSE
				SWITCH(	
				PC = 1 ? LSUM(j = 1 TO 2, MAIZEyieldcurveREST[R,j])/2,
				PC = 2 ? LSUM(j = 1 TO 3, MAIZEyieldcurveREST[R,j])/3,
				PC = 3 ? LSUM(j = 1 TO 5, MAIZEyieldcurveREST[R,j])/5,
				PC = NPC-2 ? LSUM(j = 0 TO 4, MAIZEyieldcurveREST[R,NPC-j])/5,
				PC = NPC-1 ? LSUM(j = 0 TO 2, MAIZEyieldcurveREST[R,NPC-j])/3,
				PC = NPC ? MAIZEyieldcurveREST[R,NPC],
				ELSE LSUM(j = 1 TO 7, MAIZEyieldcurveREST[R,PC+4-j])/7)), R = 1 to NR27, PC = 1 to NPC;

REAL SUGARyieldcurveRESTSmth[NR27,NPC](t);
SUGARyieldcurveRESTSmth[R,PC] = SWITCH(SUGARTotPotentialREST[R] < 1e7 ? 0 ELSE
				SWITCH(	
				PC = 1 ? LSUM(j = 1 TO 2, SUGARyieldcurveREST[R,j])/2,
				PC = 2 ? LSUM(j = 1 TO 3, SUGARyieldcurveREST[R,j])/3,
				PC = 3 ? LSUM(j = 1 TO 5, SUGARyieldcurveREST[R,j])/5,
				PC = NPC-2 ? LSUM(j = 0 TO 4, SUGARyieldcurveREST[R,NPC-j])/5,
				PC = NPC-1 ? LSUM(j = 0 TO 2, SUGARyieldcurveREST[R,NPC-j])/3,
				PC = NPC ? SUGARyieldcurveREST[R,NPC],
				ELSE LSUM(j = 1 to 7, SUGARyieldcurveREST[R,PC+4-j])/7)), R = 1 to NR27, PC = 1 to NPC;

REAL OilcropyieldcurveRESTSmth[NR27,NPC](t);
OilcropyieldcurveRESTSmth[R,PC] = SWITCH(OilcropTotPotentialREST[R] < 1e7 ? 0 ELSE
				SWITCH(	
				PC = 1 ? LSUM(j = 1 TO 2, OilcropyieldcurveREST[R,j])/2,
				PC = 2 ? LSUM(j = 1 TO 3, OilcropyieldcurveREST[R,j])/3,
				PC = 3 ? LSUM(j = 1 TO 5, OilcropyieldcurveREST[R,j])/5,
				PC = NPC-2 ? LSUM(j = 0 TO 4, OilcropyieldcurveREST[R,NPC-j])/5,
				PC = NPC-1 ? LSUM(j = 0 TO 2, OilcropyieldcurveREST[R,NPC-j])/3,
				PC = NPC ? OilcropyieldcurveREST[R,NPC],
				ELSE LSUM(j = 1 to 7, OilcropyieldcurveREST[R,PC+4-j])/7)), R = 1 to NR27, PC = 1 to NPC;