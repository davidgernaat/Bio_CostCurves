! Biofuel crop yields (T/Ha)
RelProdWoodyBiomass[C]	= GRMPPC[C,11], C = 1 TO NC;
RelProdNWoodBiomass[C]	= GRMPPC[C,12], C = 1 TO NC;
RelProdMaizeBiomass[C]	= GRMPPC[C,10], C = 1 TO NC;
RelProdSugarBiomass[C]	= GRMPPC[C,9], C = 1 TO NC;

! Unit is in GJ per ha (multiplied with Heating Value)

WOODYProductivity[C] = 	SWITCH( ISIMIP = 1 AND ISIMIP_BTR_CHECK = 1?	! IF ISIMIP Yield data is bieng used, use that dierectly 
				SWITCH( t > 2005 AND FLAGYield2005 = 1?
					LAST(WOODYProductivity[C], 0.0)
				ELSE 
					ISIMIP_BTR_Yield[C] * HValueWood)
			ELSE
			SWITCH(RelProdWoodyBiomass[C] * MaxYieldWoody * HValueWood > 100 ? 	! ELSE use standard IMAGE method
                             RelProdWoodyBiomass[C] * MaxYieldWoody * HValueWood 
                       	ELSE 
                             0.0
                       )), C = 1 to NC;

NWOODProductivity[C] = SWITCH( ISIMIP = 1 AND ISIMIP_BGR_CHECK = 1?	! IF ISIMIP Yield data is bieng used, use that dierectly 
				SWITCH( t > 2005 AND FLAGYield2005 = 1?
					LAST(NWOODProductivity[C], 0.0)
				ELSE 
					ISIMIP_BGR_Yield[C] * HValueNWood)
			ELSE
			SWITCH(RelProdNWoodBiomass[C] * MaxYieldNWood * HValueNWood > 100 ? 
                       	      RelProdNWoodBiomass[C] * MaxYieldNWood * HValueNWood 
                       	ELSE 
                             0.0
                       )), C = 1 to NC;

MAIZEProductivity[C] = SWITCH( ISIMIP = 1 AND ISIMIP_MAI_CHECK = 1?	! IF ISIMIP Yield data is bieng used, use that dierectly 
				SWITCH( t > 2005 AND FLAGYield2005 = 1?
					LAST(MAIZEProductivity[C], 0.0)
				ELSE 
					ISIMIP_MAI_Yield[C] * HValuemaize)
			ELSE
			SWITCH(RelProdMaizeBiomass[C] * MAXYieldMAIZE  * HValuemaize > 50 ? 
                             RelProdMaizeBiomass[C] * MAXYieldMAIZE  * HValuemaize 
                       	ELSE 
                             0.0
                       	)), C = 1 to NC;
SUGARProductivity[C] = 	SWITCH( ISIMIP = 1 AND ISIMIP_SUG_CHECK = 1?	! IF ISIMIP Yield data is bieng used, use that dierectly 
				SWITCH( t > 2005 AND FLAGYield2005 = 1?
					LAST(SUGARProductivity[C], 0.0)
				ELSE 
					ISIMIP_SUG_Yield[C] * HValuesugar)
			ELSE
			SWITCH(RelProdSugarBiomass[C] * MAXYieldSUGAR  * HValuesugar > 150 ? 
                             RelProdSugarBiomass[C] * MAXYieldSUGAR  * HValuesugar 
                       	ELSE 
                             0.0
                       )), C = 1 to NC;

OilcropProductivity[C] = SWITCH( ISIMIP = 1 ? 0.0 ELSE
			SWITCH(MAX(GroundnutPot[C], SesamePot[C], SoyaPot[C], SunflTempPot[C], SunflTropPot[C]) * HValueOilcrop > 100 ? 
                             MAX(GroundnutPot[C], SesamePot[C], SoyaPot[C], SunflTempPot[C], SunflTropPot[C]) * HValueOilcrop / 1000
                       ELSE 
                             0.0
                       )), C = 1 to NC;!JVV cutoff value nog bepalen?

WOODYProductivityAbon[C] = AbonMult[C] * WOODYProductivity[C], C = 1 to NC;	! GJ/ha
NWOODProductivityAbon[C] = AbonMult[C] * NWOODProductivity[C], C = 1 to NC;	! GJ/ha
MAIZEProductivityAbon[C] = AbonMult[C] * MAIZEProductivity[C], C = 1 to NC;	! GJ/ha
SUGARProductivityAbon[C] = AbonMult[C] * SUGARProductivity[C], C = 1 to NC;	! GJ/ha
OilcropProductivityAbon[C] = AbonMult[C] * OilcropProductivity[C], C = 1 to NC;	! GJ/ha

WOODYProductivityRest[C] = RestMult[C] * WOODYProductivity[C], C = 1 to NC;	! GJ/ha
NWOODProductivityRest[C] = RestMult[C] * NWOODProductivity[C], C = 1 to NC;	! GJ/ha
MAIZEProductivityRest[C] = RestMult[C] * MAIZEProductivity[C], C = 1 to NC;	! GJ/ha
SUGARProductivityRest[C] = RestMult[C] * SUGARProductivity[C], C = 1 to NC;	! GJ/ha
OilcropProductivityRest[C] = RestMult[C] * OilcropProductivity[C], C = 1 to NC;! GJ/ha

! Potential energy production per cell
WOODYProdPerCellAbon[C] = WOODYProductivityAbon[C] * ImplementationMap[C] * AbonMult[C]*  Area[C] * HaPerKm2, C = 1 to NC;
NWOODProdPerCellAbon[C] = NWOODProductivityAbon[C] * ImplementationMap[C] * AbonMult[C]*  Area[C] * HaPerKm2, C = 1 to NC;
MAIZEProdPerCellAbon[C] = MAIZEProductivityAbon[C] * ImplementationMap[C] * AbonMult[C]*  Area[C] * HaPerKm2, C = 1 to NC;
SUGARProdPerCellAbon[C] = SUGARProductivityAbon[C] * ImplementationMap[C] * AbonMult[C]*  Area[C] * HaPerKm2, C = 1 to NC;
OilcropProdPerCellAbon[C] = OilcropProductivityAbon[C] * ImplementationMap[C] * AbonMult[C]*  Area[C] * HaPerKm2, C = 1 to NC;

WOODYProdPerCellREST[C] = WOODYProductivity[C] * ImplementationMap[C]* RestMult[C]*  Area[C] * HaPerKm2, C = 1 to NC;
NWOODProdPerCellREST[C] = NWOODProductivity[C] * ImplementationMap[C]* RestMult[C]*  Area[C] * HaPerKm2, C = 1 to NC;
MAIZEProdPerCellREST[C] = MAIZEProductivity[C] * ImplementationMap[C]* RestMult[C]*  Area[C] * HaPerKm2, C = 1 to NC;
SUGARProdPerCellREST[C] = SUGARProductivity[C] * ImplementationMap[C]* RestMult[C]*  Area[C] * HaPerKm2, C = 1 to NC;
OilcropProdPerCellREST[C] = OilcropProductivity[C] * ImplementationMap[C]* RestMult[C]*  Area[C] * HaPerKm2, C = 1 to NC;

WOODYProdPerCellTot[C]=MAX(WOODYProdPerCellAbon[C],WOODYProdPerCellREST[C]), C = 1 to NC;
NWOODProdPerCellTot[C]=MAX(NWOODProdPerCellAbon[C],NWOODProdPerCellREST[C]), C = 1 to NC;
MAIZEProdPerCellTot[C]=MAX(MAIZEProdPerCellAbon[C],MAIZEProdPerCellREST[C]), C = 1 to NC;
SUGARProdPerCellTot[C]=MAX(SUGARProdPerCellAbon[C],SUGARProdPerCellREST[C]), C = 1 to NC;
OilcropProdPerCellTot[C]=MAX(OilcropProdPerCellAbon[C],OilcropProdPerCellREST[C]), C = 1 to NC;