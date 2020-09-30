{
REAL	CarbonShadow[6,18](t) 		= FILE ("Data/TBFCARBON_DIF.out");

REAL	CarbonShadowSpec[2,18](t);
CarbonShadowSpec[1,R] = LSUM(i = 1 to 2, CarbonShadow[i,R]), R = 1 to 18;
CarbonShadowSpec[2,R] = LSUM(i = 3 to 6, CarbonShadow[i,R]), R = 1 to 18;

REAL WOODYCumPotREST[NR27T](t);

WOODYCumPotREST[R] = SWITCH(t = t.min or t = t.max ? LAST(WOODYCumPotREST[R],0) + (t.step / 2) * WOODYTotPotentialREST[R] ELSE 
			LAST(WOODYCumPotREST[R],0) + (t.step) * WOODYTotPotentialREST[R]), R = 1 to NR27;
WOODYCumPotREST[NR27T] = LSUM(R = 1 to NR27,WOODYCumPotREST[R]);

REAL CarbonContentAbon17[NR17T](t);
REAL CarbonContentAbon[NR27T](t);

CarbonContentAbon17[R] = SWITCH(LSUM(i = 1 to NR27,Conv[R,i]*WOODYCumPotAbon[i]) > EPS ? 
				1e12*CarbonShadowSpec[1,R]/LSUM(i = 1 to NR27,Conv[R,i]*WOODYCumPotAbon[i]) ELSE 0), R = 1 to 17;
CarbonContentAbon17[18] = SWITCH(WOODYCumPotAbon[NR27T] > EPS ? 
				1e12*CarbonShadowSpec[1,18]/WOODYCumPotAbon[NR27T] ELSE 0);
CarbonContentAbon[R] = CarbonContentAbon17[CONV2[R]], R = 1 to NR27;
CarbonContentAbon[NR27T] = CarbonContentAbon17[NR17T], R = 1 to NR27;

REAL CarbonContentREST17[NR17T](t);
REAL CarbonContentREST[NR27T](t);

CarbonContentREST17[R] = SWITCH(LSUM(i = 1 to NR27,Conv[R,i]*WOODYCumPotREST[i]) > EPS ? 
				1e12*CarbonShadowSpec[1,R]/LSUM(i = 1 to NR27,Conv[R,i]*WOODYCumPotREST[i]) ELSE 0), R = 1 to 17;
CarbonContentREST17[18] = SWITCH(WOODYCumPotREST[NR27T] > EPS ? 
				1e12*CarbonShadowSpec[1,18]/WOODYCumPotREST[NR27T] ELSE 0);
CarbonContentREST[R] = CarbonContentREST17[CONV2[R]], R = 1 to NR27;
CarbonContentREST[NR27T] = CarbonContentREST17[NR17T], R = 1 to NR27;
}