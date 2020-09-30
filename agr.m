! ****************** THEORETICAL POTENTIAL ******************
! AGRICULTURAL RESIDUES 
! On field, per grid cell 
! Determine RPR:
AgrRPR[i,CROP]		= SWITCH(CropYield_in[i,CROP] > EPS ? 
				MAX(0, RPRFac[1,CROP] * LOG(CropYield_in[i,CROP]) + RPRFac[2,CROP]) 
			ELSE 0), i = 1 TO NC, CROP = 1 TO CROPT;
! Potential of crops based on RPR 
AgrTheoPot1[i,CROP]		= CropProd2[i,CROP] * AgrRPR[i,CROP], i = 1 TO NC, CROP = 1 TO 7;	! t-wet - Rainfed crops
AgrTheoPot1[i,CROP]		= 0				    , i = 1 TO NC, CROP = 8;		! t-wet - Sugarcane Biofuel Crops. Set to 0 since bagasse is consumed
AgrTheoPot1[i,CROP]		= CropProd2[i,CROP] * AgrRPR[i,CROP], i = 1 TO NC, CROP = 9 TO CROPT;	! t-wet - Irrigated Crops
! Potential of crops based on RSR
AgrTheoPot1[i,CROPT+CROP]	= CropFrac[i,CROP+11] * Area[i] * (RSRFac[CROP] * Ha2km), i = 1 TO NC, CROP = 1 TO 3;	!t-wet
! Potential of Other Crop: Sugarcane, which is based on RPR. In brackets: t of sugarcane produced per grid cell
AgrTheoPot1[i,CROPT2]		= (CropFrac[i,CROPS-1] * Area[i] * CropYield_in[i,8]) * AgrRPR[i,8], i = 1 TO NC;	!t

! Reorganise crop categories from CROPT -> CROPS, t
AgrTheoPot[i,CROP]	= AgrTheoPot1[i,CROP] + AgrTheoPot1[i,CROP+11], i = 1 TO NC, CROP = 1 TO NFC;	! Food crops (1-7)
AgrTheoPot[i,CROP]	= AgrTheoPot1[i,CROP], i = 1 TO NC, CROP = NFC+1 TO NFC+4;			! Biofuel crops (8-11)
AgrTheoPot[i,CROP]	= AgrTheoPot1[i,CROP+7], i = 1 TO NC, CROP = NFC+5 TO CROPS-1;			! Other Crops	(12-15)
AgrTheoPot[i,CROPS]	= LSUM(CROP = 1 TO CROPS-1, AgrTheoPot1[i,CROP]), i = 1 TO NC;
AgrTheoPotMS[i,CROP]	= SWITCH( AgrTheoPot[i,CROPS] > EPS ? AgrTheoPot[i,CROP]/AgrTheoPot[i,CROPS], ELSE 0.0), i = 1 TO NC, CROP = 1 TO CROPS-1;

! Total
AgrTheoPotTot[i]	= AgrTheoPot[i,CROPS], i = 1 TO NC;	! t				

! Residues per Crop per km
AgrTheoPotpkm[i,CROP]	= SWITCH( (CropFrac[i,CROP] * Area[i]) > EPS ?
				AgrTheoPot[i,CROP]/(CropFrac[i,CROP] * Area[i])
			ELSE 0), i = 1 TO NC, CROP = 1 TO CROPS-1;

AgrTheoPotpkm[i,CROPS]	= SWITCH( Area[i] > 0.0 ? 		! t/km^2
				AgrTheoPotTot[i] / Area[i] 
			ELSE 0), i = 1 TO NC;

! Total (wet) per km
AgrTheoPotTotpkm[i]	= AgrTheoPotpkm[i,CROPS], i = 1 TO NC;	! t-wet/km^2

! Total Per Region, t-wet
AgrTheoPotTotR1[R]	= 0, R = 1 TO NRC;
sumarray (AgrTheoPotTotR1, NC, NRC, Region, AgrTheoPotTot);		! Grid-to-region
AgrTheoPotTotR[R]	= AgrTheoPotTotR1[R], R = 1 TO 26;
AgrTheoPotTotR[NRC]	= LSUM(R = 1 TO 26, AgrTheoPotTotR1[R]);	! t-wet
! Per Crop, Per region, t
AgrTheoPotR1[R,CROP]	= 0, R = 1 TO NRC, CROP = 1 TO CROPS;
sumpartdim (AgrTheoPotR1, NC, NRC, CROPS, Region, AgrTheoPot);		! Grid-to-region
AgrTheoPotR[R,CROP]	= AgrTheoPotR1[R,CROP], R = 1 TO 26, CROP = 1 TO CROPS-1;	! t-wet
AgrTheoPotR[NRC,CROP]	= LSUM(R = 1 TO 26, AgrTheoPotR[R,CROP]), CROP = 1 TO CROPS-1;
AgrTheoPotR[R,CROPS]	= LSUM(CROP = 1 TO CROPS-1, AgrTheoPotR[R,CROP]), R = 1 TO NRC;

! Dry Potential
AgrTheoPotDryR[R,CROP]	= AgrTheoPotR[R,CROP] * (1-MoistureContent[CROP]), R = 1 TO 26, CROP = 1 TO CROPS-1;
AgrTheoPotDryR[NRC,CROP]= LSUM(R = 1 TO 26, AgrTheoPotDryR[R,CROP]), CROP = 1 TO CROPS-1;
AgrTheoPotDryR[R,CROPS]	= LSUM(CROP = 1 TO CROPS-1, AgrTheoPotDryR[R,CROP]), R = 1 TO NRC;

! Theoretical Potential in Energy Terms (GJ)
AgrTheoPotREn[R,CROP]	= AgrTheoPotDryR[R,CROP] * CROPHHV[CROP], R = 1 TO NRC, CROP = 1 TO CROPS-1;
AgrTheoPotREn[R,CROPS]	= LSUM(CROP = 1 TO CROPS-1, AgrTheoPotREn[R,CROP]), R = 1 TO NRC;


! PROCESS RESIDUES
AgrProcPot1[i,CROP]	= MAX(0,CropProd2[i,CROP] * AgriProcessRatio[CROP]), i = 1 TO NC, CROP = 1 TO CROPT;
! Reorganise crop categories from CROPT -> CROPS, t-wet
AgrProcPot[i,CROP]	= AgrProcPot1[i,CROP] + AgrProcPot1[i,CROP+11], i = 1 TO NC, CROP = 1 TO NFC;		! Food crops
AgrProcPot[i,CROP]	= AgrProcPot1[i,CROP], i = 1 TO NC, CROP = NFC+1 TO NFC+4;				! Biofuel crops
AgrProcPot[i,CROP]	= 0, i = 1 TO NC, CROP = NFC+5 TO CROPS-1;						! other crops
AgrProcPot[i,CROPS]	= LSUM(CROP = 1 TO CROPS-1, AgrProcPot[i,CROP]), i = 1 TO NC;

! Per Crop, Per region, t-wet
AgrProcPotR1[R,CROP] 	= 0, R = 1 TO NRC, CROP = 1 TO CROPS;
sumpartdim (AgrProcPotR1, NC, NRC, CROPS, Region, AgrProcPot);							! Grid-to-region
AgrProcPotR[R,CROP]	= AgrProcPotR1[R,CROP], R = 1 TO 26, CROP = 1 TO CROPS-1;
AgrProcPotR[NRC,CROP]	= LSUM(R = 1 TO 26, AgrProcPotR[R,CROP]), CROP = 1 TO CROPS-1;
AgrProcPotR[R,CROPS]	= LSUM(CROP = 1 TO CROPS-1, AgrProcPotR[R,CROP]), R = 1 TO NRC;
AgrProcPotTotR[R]	= AgrProcPotR[R,CROPS], R = 1 TO NRC;

! Dry potential
AgrProcPotDryR[R,CROP]	= AgrProcPotR[R,CROP] * (1-MoistureContent[CROP]), R = 1 TO 26, CROP = 1 TO CROPS-1;
AgrProcPotDryR[NRC,CROP]= LSUM(R = 1 TO 26, AgrProcPotDryR[R,CROP]), CROP = 1 TO CROPS-1;
AgrProcPotDryR[R,CROPS]	= LSUM(CROP = 1 TO CROPS-1, AgrProcPotDryR[R,CROP]), R = 1 TO NRC;

! Process Residues in Energy temrs (GJ)
AgrProcPotREn[R,CROP]	= AgrProcPotDryR[R,CROP] * CROPHHV[CROP], R = 1 TO NRC, CROP = 1 TO CROPS-1;
AgrProcPotREn[R,CROPS]	= LSUM(CROP = 1 TO CROPS-1, AgrProcPotREn[R,CROP]), R = 1 TO NRC;

AgrProcPotREnTot[R]	= AgrProcPotREn[R,CROPS], R = 1 TO 26;
AgrProcPotREnTot[NRC]	= LSUM(R = 1 TO 26, AgrProcPotREnTot[R]);

! ****************** ECOLOGICAL POTENTIAL ******************

AgrEcoPotpkm[i,CROP]		= MAX(0, AgrTheoPotpkm[i,CROP] - EroConstAgr), i = 1 TO NC, CROP = 1 TO CROPS;
AgrEcoPot[i,CROP]		= AgrEcoPotpkm[i,CROP] * CropFrac[i,CROP] * Area[i], i = 1 TO NC, CROP =  1 TO CROPS-1;
AgrEcoPot[i,CROPS]		= LSUM(CROP = 1 TO CROPS-1, AgrEcoPot[i,CROP]), i = 1 TO NC;
AgrEcoPotTot[i]			= AgrEcoPot[i,CROPS], i = 1 TO NC;
AgrEcoPotTotpkm[i]		= SWITCH( (Area[i] * CropFracTot[i]) > EPS ?
					AgrEcoPot[i,CROPS]/(Area[i] * CropFracTot[i]) 
				ELSE 0), i = 1 TO NC;

! DECOMPOSITION OF ELEMENTS
! Assume that fist residues are used for SOC control. If they meet this, they are used for erosion
	REAL	AgrEcoUse1[NC,CROPS](t),	! GJ, Resdues used for SOC
		AgrEcoUse2[NC,CROPS](t),	! GJ, Resdues used for erosion
		AgrEcoUseR1[NRC,CROPS](t),	! GJ, Residue for SOC, regional
		AgrEcoUseR2[NRC,CROPS](t);	! GJ, Residue for erosion, regional
	

AgrEcoUse1[i,CROP]		= SWITCH( SOC[i] > SOCConst ?
						0
					ELSE AgrTheoPot[i,CROP] * (1-MoistureContent[CROP]) * CROPHHV[CROP]), i = 1 TO NC, CROP = 1 TO CROPS-1;
AgrEcoUse1[i,CROPS]		= LSUM(CROP = 1 TO CROPS-1, AgrEcoUse1[i,CROP]), i = 1 TO NC;
AgrEcoUse2[i,CROP]		= SWITCH( SOC[i] > SOCConst ?
						(AgrTheoPot[i,CROP] - AgrEcoPot[i,CROP]) * (1-MoistureContent[CROP]) * CROPHHV[CROP]
					ELSE 0), i = 1 TO NC, CROP = 1 TO CROPS-1;
AgrEcoUse2[i,CROPS]		= LSUM(CROP = 1 TO CROPS-1, AgrEcoUse2[i,CROP]), i = 1 TO NC;

AgrEcoUseR1[R,CROP]	= 0, R = 1 TO NRC, CROP = 1 TO CROPS;
AgrEcoUSeR2[R,CROP]	= 0, R = 1 TO NRC, CROP = 1 TO CROPS;
sumpartdim(AgrEcoUseR1, NC, NRC, CROPS, Region, AgrEcoUse1);
sumpartdim(AgrEcoUseR2, NC, NRC, CROPS, Region, AgrEcoUse2);
	
AgrEcoUseR[R,CROP,1]	= AgrEcoUseR1[R,CROP], R = 1 TO 26, CROP = 1 TO CROPS-1;
AgrEcoUseR[R,CROP,2]	= AgrEcoUseR2[R,CROP], R = 1 TO 26, CROP = 1 TO CROPS-1;
AgrEcoUseR[R,CROPS,i]	= LSUM(CROP = 1 TO CROPS-1, AgrEcoUseR[R,CROP,i]), R = 1 TO 26, i = 1 TO 2;
AgrEcoUseR[NRC,CROP,i]	= LSUM(R = 1 TO 26, AgrEcoUseR[R,CROP,i]), CROP = 1 TO CROPS, i = 1 TO 2;

! END OF DECOMPOSITION
! Regional
AgrEcoPotTotR1[R] = 0, R = 1 TO NRC;
sumarray (AgrEcoPotTotR1, NC, NRC, Region, AgrEcoPotTot);
AgrEcoPotTotR[R]	= AgrEcoPotTotR1[R], R = 1 TO 26;
AgrEcoPotTotR[NRC]	= LSUM(R = 1 TO 26, AgrEcoPotTotR1[R]);

! Per region and crop			
AgrEcoPotR1[R,CROP]	= 0, R = 1 TO NRC, CROP = 1 TO CROPS;
sumpartdim(AgrEcoPotR1, NC, NRC, CROPS, Region, AgrEcoPot);
AgrEcoPotR[R,CROP]	= AgrEcoPotR1[R,CROP], R = 1 TO 26, CROP = 1 TO CROPS;
AgrEcoPotR[NRC,CROP]	= LSUM(R = 1 TO 26, AgrEcoPotR1[R,CROP]), CROP = 1 TO CROPS;

AgrEcoPotMS[R,CROP]	= SWITCH( AgrEcoPotR[R,CROPS] > 0 ?
				AgrEcoPotR[R,CROP]/AgrEcoPotR[R,CROPS] 
			ELSE 0), R = 1 TO NRC, CROP = 1 TO CROPS-1;

! Dry Potential
	! Map
AgrEcoPotDry[i,CROP]	= AgrEcoPot[i,CROP] * (1-MoistureContent[CROP]), i = 1 TO NC, CROP = 1 TO CROPS-1;
AgrEcoPotDry[i,CROPS]	= LSUM(CROP = 1 TO CROPS-1, AgrEcoPotDry[i,CROP]), i = 1 TO NC;

	! Regional
AgrEcoPotDryR[R,CROP]	= AgrEcoPotR[R,CROP] * (1-MoistureContent[CROP]), R = 1 TO 26, CROP = 1 TO CROPS-1;
AgrEcoPotDryR[NRC,CROP]	= LSUM(R = 1 TO 26, AgrEcoPotDryR[R,CROP]), CROP = 1 TO CROPS-1;
AgrEcoPotDryR[R,CROPS]	= LSUM(CROP = 1 TO CROPS-1, AgrEcoPotDryR[R,CROP]), R = 1 TO NRC;

! Ecological potential in Energy terms
AgrEcoPotEn[i,CROP]	= AgrEcoPotDry[i,CROP] * CROPHHV[CROP], i = 1 TO NC, CROP = 1 TO CROPS-1;
AgrEcoPotEn[i,CROPS]	= LSUM(CROP = 1 TO CROPS-1, AgrEcoPotEn[i,CROP]), i = 1 TO NC;
	
AgrEcoPotEnpkm[i,CROP]	= SWITCH( CropFrac[i,CROP] * Area[i] > EPS ? 
				AgrEcoPotEn[i,CROP]/(CropFrac[i,CROP] * Area[i])
			ELSE 0), i = 1 TO NC, CROP = 1 TO CROPS-1;

AgrEcoPotEnpkm[i,CROPS]	= SWITCH( CropFracTot[i] * Area[i] > 0.0 ? 		
				AgrEcoPotEn[i,CROPS] / (CropFracTot[i] * Area[i]) 
			ELSE 0), i = 1 TO NC;

AgrEcoPotREn[R,CROP]	= AgrEcoPotDryR[R,CROP] * CROPHHV[CROP], R = 1 TO NRC, CROP = 1 TO CROPS-1;
AgrEcoPotREn[R,CROPS]	= LSUM(CROP = 1 TO CROPS-1, AgrEcoPotREn[R,CROP]), R = 1 TO NRC;

! ****************** TECHNICAL POTENTIAL ******************
! Take away demand of agricultural residues for feed
!AgrTechPot1[R,CROP]	= AgrEcoPotR[R,CROP], R = 1 TO NRC, CROP = 1 TO CROPS;
AgrTechPotDryR[R,CROP]	= MAX(0, AgrEcoPotDryR[R,CROP] - FeedDemCrop[R,CROP]), R = 1 TO 26, CROP = 1 TO CROPS-1;
AgrTechPotDryR[R,CROPS]	= LSUM(CROP = 1 TO CROPS-1, AgrTechPotDryR[R,CROP]), R = 1 TO 26;
AgrTechPotDryR[NRC,CROP]	= LSUM(R = 1 TO 26, AgrTechPotDryR[R,CROP]), CROP = 1 TO CROPS;

AgrTechPotTotDryR[R]	= AgrTechPotDryR[R,CROPS], R = 1 TO NRC;

AgrTechPotREn[R,CROP]	= AgrTechPotDryR[R,CROP] * CROPHHV[CROP], R = 1 tO NRC, CROP = 1 TO CROPS-1;
AgrTechPotREn[R,CROPS]	= LSUM(CROP = 1 TO CROPS-1, AgrTechPotREn[R,CROP]), R = 1 TO NRC;

! Since there is a chance that  FeedDem > AgrEcoPot, have to determine how many residues are ACTUALLY used for feed demand
FeedDemCropActEn[R,CROP]	= (AgrEcoPotDryR[R,CROP] - AgrTechPotDryR[R,CROP]) * CROPHHV[CROP], R = 1 TO 26, CROP = 1 TO CROPS-1;
FeedDemCropActEn[R,CROPS]	= LSUM(CROP = 1 TO CROPS-1, FeedDemCropActEn[R,CROP]), R = 1 TO 26;
FeedDemCropActEn[NRC,CROP]	= LSUM(R = 1 TO 26, FeedDemCropActEn[R,CROP]), CROP = 1 TO CROPS;

FeedDemActEn[R]			= FeedDemCropActEn[R,CROPS], R = 1 TO NRC;

FeedDemFrac[R]			= SWITCH( FeedDemCrop[R,CROPS]> EPS ?
					LSUM(i = 1 TO CROPS-1, AgrEcoPotDryR[R,i] - AgrTechPotDryR[R,i])/FeedDemCrop[R,CROPS]
				ELSE 0), R = 1 TO NRC;