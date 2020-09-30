RegReserveArea[R] = 0, R = 1 to NR27;
sumarray(RegReserveArea, NC, NR27, region, AreaReserve);

TotReserveArea = HaPerKm2*LSUM(R = 1 to NR27-1, RegReserveArea[R]);

InproductiveLand2[C] = SWITCH(Forest[C] = 0 AND LandAbon[C] = 0 AND AreaReserve[C] = 0 AND LandRest[C] = 0 AND
		Aglandmap[C] = 0 ? area[C] ELSE 0), C = 1 to NC;

TotArea[R] = 0,R = 1 to NR27;
sumarray (TotArea, NC, NR27, region, area);

Aglandmap[C] = 	 SWITCH(LandCover[C] = 1 ? Area[C] ELSE 0), C = 1 to NC;


Agland[R] = 0,R = 1 to NR27;
sumarray (Agland, NC, NR27, region, Aglandmap);

LandAbonAreaReg[R] = 0, R = 1 to NR27;
LandRestAreaReg[R] = 0, R = 1 to NR27;
InproductiveLandTotReg[R] = 0, R = 1 to NR27;

sumarray (LandAbonAreaReg, NC, NR27, region, LandAbon);
sumarray (LandRestAreaReg, NC, NR27, region, LandREST);
sumarray (InproductiveLandTotReg, NC, NR27, region, InproductiveLand2);

LandOverviewWorld[i] = 0, i = 1 to 6;

! Category 1 = Reserve
! Category 2 = Forest
! Category 3 = Agriculture land
! Category 4 = Inproductive
! Category 5 = Abondoned crop land or timber land
! Category 6 = Rest land

OverviewMap[C] = SWITCH(AreaReserve[C] > 0 ? 1 ELSE
			SWITCH(Forest[C] > 1 ? 2 ELSE
				SWITCH(LandCover[C] = 1 OR NotAvExtGrassland[C] = 1 ? 3 ELSE 
					SWITCH(InproductiveMult[C] = 0 ? 4 ELSE
						SWITCH(AbonMult[C] >= 1 ? 5 ELSE 6))))), C = 1 to NC;

sumarray (LandOverviewWorld, NC, 6, OverviewMap, area);