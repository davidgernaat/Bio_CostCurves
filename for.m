! ****************** THEORETICAL POTENTIAL ******************
! *** MAPS OF FOREST MANAGEMENT ***
INTEGER	ManagedFor[NC](t);	! Types of Regrowth forests, 1. Regrowth CC, 2. Regrowth SC, 3. Regrowth WP
REAL	ForCC[NC](t),
	ForSC[NC](t),
	ForWP[NC](t);
!ManagedFor[i]	= MAX(0, MIN(ForCov[i]- 5, 3)), i = 1 TO NC;
ManagedFor[i]	= SWITCH(ForCov[i] < 5 ?
			0,
		ELSE	SWITCH( ForCov[i] = 6 ?
				1,
			ELSE	SWITCH( ForCov[i] = 7 ?
					2,
				ELSE SWITCH( ForCov[i] = 8 ?
						3,
					ELSE 0)
				)
			)
		), i = 1 TO NC;

ForCC[i]	= SWITCH(ManagedFor[i] = 1 ? 1, ELSE 0), i = 1 TO NC;
ForSC[i]	= SWITCH(ManagedFor[i] = 2 ? 1, ELSE 0), i = 1 TO NC;
ForWP[i]	= SWITCH(ManagedFor[i] = 3 ? 1, ELSE 0), i = 1 TO NC;

REAL		CountCC[NRC](t),	! # of cells per region with a specific forest management
		CountSC[NRC](t),
		CountWP[NRC](t);
		
CountCC[R]	= 0, R = 1 TO NRC;
sumarray (CountCC, NC, NRC, Region, ForCC);
CountSC[R]	= 0, R = 1 TO NRC;
sumarray (CountSC, NC, NRC, Region, ForSC);
CountWP[R]	= 0, R = 1 TO NRC;
sumarray (CountWP, NC, NRC, Region, ForWP);

! *** WOOD PRODUCTION PER GRID CELL ***
REAL	WoodProdPerC[NRC,3](t);

WoodProdPerC[R,1]	= SWITCH( CountCC[R] > EPS ?
				WoodProd[R,1]/CountCC[R],
			ELSE 0), R = 1 TO NRC;

WoodProdPerC[R,2]	= SWITCH( CountSC[R] > EPS ?
				WoodProd[R,2]/CountSC[R],
			ELSE 0), R = 1 TO NRC;
			
WoodProdPerC[R,3]	= SWITCH( CountWP[R] > EPS ?
				WoodProd[R,3]/CountWP[R],
			ELSE 0), R = 1 TO NRC;

REAL	WoodProdSpec[NC](t);	! t wood produced (cc,sc,wp)
WoodProdSpec[i]	= SWITCH( Region[i] > EPS AND ManagedFor[i] > EPS ?
			WoodProdPerC[Region[i],ManagedFor[i]],
		ELSE 0), i = 1 TO NC;

REAL 	WoodProdSpecR1[NRC](t),	! t
	WoodProdSpecR[NRC](t);
WoodProdSpecR1[R]	= 0, R = 1 TO NRC;
sumarray (WoodProdSpecR1, NC, NRC, Region, WoodProdSpec);
WoodProdSpecR[R]	= WoodProdSpecR1[R], R = 1 TO 26;
WoodProdSpecR[NRC]	= LSUM(R = 1 TO 26, WoodProdSpecR[R]);

! *** RESIDUE ALLOCATION ***
!ForTheoPotTot[i]	= SWITCH( PotVeg[i] > EPS AND ManagedFor[i] > EPS ?
!			WoodProdSpec[i] * ForResPotPerVeg[Potveg[i],ManagedFor[i]],
!		ELSE 0), i = 1 TO NC;
!Corrected: Correcting for units. ForResPotPerVeg (kg-res/kg-tree) and WoodProdPerSpec (kg-wood)
ForTheoPotTot[i]	= SWITCH( PotVeg[i] > EPS AND ManagedFor[i] > EPS AND (1 - ForResPotPerVeg[Potveg[i],ManagedFor[i]]) > EPS ?
			(WoodProdSpec[i] * ForResPotPerVeg[Potveg[i],ManagedFor[i]]) / (1 - ForResPotPerVeg[Potveg[i],ManagedFor[i]]),
		ELSE 0), i = 1 TO NC;

ForTheoPotTotR1[R]	= 0, R = 1 TO NRC;
sumarray (ForTheoPotTotR1, NC, NRC, Region, ForTheoPotTot);
ForTheoPotTotR[R]	= ForTheoPotTotR1[R], R = 1 TO 26;
ForTheoPotTotR[NRC]	= LSUM(R = 1 TO 26, ForTheoPotTotR[R]);

ForTheoPotTotpkm[i]	= SWITCH(Area[i] > EPS ?
				ForTheoPotTot[i]/Area[i],
			ELSE 0), i = 1 TO NC;

ForTheoPotTotREn[R]	= ForTheoPotTotR[R] * (1-MoistureContent[NRES-1]) * ForHHV, R = 1 TO NRC;

! Process Residues
ForProcPot1[R,i]	= WoodDem[R,i] * ForProcessRatio[i] * FORHHV, R = 1 TO 26, i = 1 TO 2;
ForProcPot1[NRC,i]	= LSUM(R = 1 TO 26, ForProcPot1[R,i]), i = 1 TO 2;
ForProcPot[R]		= LSUM(i = 1 TO 2, ForProcPot1[R,i]), R = 1 TO NRC;

! ****************** ECOLOGICAL POTENTIAL ******************
! Ecological potential is based on a removable fraction of the forest residues. 
! This is based on calculations of forest production (tons) and requirement to stay on the field (tons).
! Forest residues are produced over a long period, we cannot calculate annualy how many residues should remain, but rather a fraction of the total ammount
! In order to make sure that ecological uses do not change when forest productivity changes, have to adapt code
! Thus running modes: 1. Normal, calculation based on data. 2. Sensitivity, use residue mass from the normal running mode
REAL	
ForEcoPotTotpkm1[NC](t),	! Normal run
ForEcoPotTotEnpkm1[NC](t),	! Normal run
ForEcoPotTot1[NC](t),		! Normal run
ForEcoPotTotEn1[NC](t),		! Normal run
ForEcoPotTotpkm2[NC](t),	! Sensitivty run
ForEcoPotTotEnpkm2[NC](t),	! Sensitivty run
ForEcoPotTot2[NC](t),		! Sensitivty run
ForEcoPotTotEn2[NC](t);		! Sensitivty run
! NORMAL
! Removable fraction varies per biome
! Based on 10t/Ha remaining fraction (Graham et al 1994)
! Translated to frqaction of theo pot based on productivities of Buck (2013) and van Dijk (2013)

ForEcoPotTotpkm1[i]		= SWITCH( ManagedFor[i] > EPS ? 
					ForTheoPotTotpkm[i] * ForEcoFrac[Potveg[i],ManagedFor[i]],
				ELSE 0), i = 1 TO NC;
ForEcoPotTotEnpkm1[i]		= SWITCH( ForEcoPotTotpkm1[i] > EPS ?
					ForEcoPotTotpkm1[i] * (1-MoistureContent[NRES-1]) * ForHHV,
				ELSE 0), i = 1 TO NC;

ForEcoPotTot1[i]			= SWITCH( ManagedFor[i] > EPS ?
					ForTheoPotTot[i] * ForEcoFrac[Potveg[i],ManagedFor[i]],	
				ELSE 0), i = 1 TO NC;

ForEcoPotTotEn1[i]		= ForEcoPotTot1[i] * (1 - MoistureContent[NRES-1]) * ForHHV, i = 1 TO NC;

! SENSITIVITY
REAL	ForEcoReqTot_Base[NC](t);		! Data supplied from scenario file
REAL	ForEcoReqTotpkm_Base[NC](t);

ForEcoReqTotpkm_Base[i]	= SWITCH( Area[i] > EPS ? ForEcoReqTot_Base[i]/Area[i] ELSE 0), i = 1 TO NC;

ForEcoPotTotpkm2[i]		= SWITCH( ManagedFor[i] > EPS ? 			! Only calculate if forest is managed
					SWITCH( ForEcoReqTotpkm_Base[i] > EPS ?
						MAX(0, ForTheoPotTotpkm[i] - ForEcoReqTotpkm_Base[i])	! If forest was also managed in baseline, use baseline ecological requirement
					ELSE ForTheoPotTotpkm[i] * ForEcoFrac[Potveg[i],ManagedFor[i]]  ! Else calculate new ecological requirement
					)
				ELSE 0), i = 1 TO NC;					! If not managed ignore
	
ForEcoPotTotEnpkm2[i]		= SWITCH( ForEcoPotTotpkm2[i] > EPS ?
					ForEcoPotTotpkm2[i] * (1-MoistureContent[NRES-1]) * ForHHV,
				ELSE 0), i = 1 TO NC;

ForEcoPotTot2[i]			= SWITCH( ManagedFor[i] > EPS ?
					SWITCH( ForEcoReqTot_Base[i] > EPS ?
						MAX(0, ForTheoPotTot[i] - ForEcoReqTot_Base[i])
					ELSE ForTheoPotTot[i] * ForEcoFrac[Potveg[i],ManagedFor[i]]
					)
				ELSE 0), i = 1 TO NC;

ForEcoPotTotEn2[i]		= ForEcoPotTot2[i] * (1 - MoistureContent[NRES-1]) * ForHHV, i = 1 TO NC;

!Results
ForEcoPotTotpkm[i]	= SWITCH( ForEcoReqTot_Base[i] > EPS? ForEcoPotTotpkm2[i] ELSE ForEcoPotTotpkm1[i]), i = 1 TO NC;
ForEcoPotTotEnpkm[i]	= SWITCH( ForEcoReqTot_Base[i] > EPS? ForEcoPotTotEnpkm2[i] ELSE ForEcoPotTotEnpkm1[i]), i = 1 TO NC;
ForEcoPotTot[i]		= SWITCH( ForEcoReqTot_Base[i] > EPS? ForEcoPotTot2[i] ELSE ForEcoPotTot1[i]), i = 1 TO NC;
ForEcoPotTotEn[i]	= SWITCH( ForEcoReqTot_Base[i] > EPS? ForEcoPotTotEn2[i] ELSE ForEcoPotTotEn1[i]), i = 1 TO NC;

! Regional Results
ForEcoPotTotR1[R] = 0, R = 1 TO NRC;
sumarray (ForEcoPotTotR1, NC, NRC, Region, ForEcoPotTot);
ForEcoPotTotR[R]	= ForEcoPotTotR1[R], R = 1 TO 26;
ForEcoPotTotR[NRC]	= LSUM(R = 1 TO 26, ForEcoPotTotR1[R]);

! Forest Residues in energy terms
ForEcoPotTotREn[R]	= ForEcoPotTotR[R] * (1-MoistureContent[NRES-1]) * ForHHV, R = 1 TO NRC;

! Determine amount of residues needed to remain on field for eco potential
! This is for the sensitivity scenarios, we want to maintain the ammount of residues remaining (and not make it a fraction of theoretical potential)
REAL
ForEcoReqTot[NC](t);

ForEcoReqTot[i]	= MAX(0,ForTheoPotTot[i] - ForEcoPotTot[i]), i = 1 TO NC;