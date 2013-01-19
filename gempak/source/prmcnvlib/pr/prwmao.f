	FUNCTION PR_WMAO ( wwma )
C************************************************************************
C* PR_WMAO								*
C*									*
C* This function converts the numeric WMO weather code for present      *
C* weather reported from an automatic station (WMO code table 4680) to  *
C* the corresponding numeric WMO weather code for present weather       *
C* reported from a manned station (WMO code table 4677).                *
C*									*
C* REAL PR_WMAO  ( WWMA )						*
C*									*
C* Input parameters:							*
C*	WWMA		REAL		Automatic station weather code  *
C*									*
C* Output parameters:							*
C*	PR_WMAO		REAL		Manned station weather code     *
C**									*
C* LOG:									*
C* D. Kidwell/NCEP	10/99		       				*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	INTEGER 	man (0:99)
C*	
	DATA		man /
     +			 -1,   1,   2,   3,   5,   5,  -1,  -1,  -1, 
     +			 -1,  10,  76,  13,  -1,  -1,  -1,  -1,  -1, 
     +			 18,  -1,  28,  21,  20,  21,  22,  24,  29, 
     +			 -1,  -1,  -1,  42,  41,  42,  44,  46,  47, 
     +			 -1,  -1,  -1,  -1, 203, 203, 203,  63,  65,  
     +			 73,  75,  67,  67,  -1,  53,  51,  53,  55, 
     +			 56,  57,  57,  58,  59,  -1,  63,  61,  63, 
     +			 65,  66,  67,  67,  68,  69,  -1,  73,  71, 
     +			 73,  75,  79,  79,  79,  77,  78,  -1,  81, 
     +			 80,  81,  81,  82,  85,  86,  86,  -1,  90, 
     +			 95,  17,  95,  96,  17,  97,  99,  -1,  -1, 
     +			 19  /
C------------------------------------------------------------------------
C
C*  	Convert automatic station weather number to manual station
C*	weather number.  Reserved locations in table 4680 and those
C*	values not having an analogue in table 4677 are mapped to 
C*	RMISSD.
C
	PR_WMAO = RMISSD
	iwx     = NINT ( wwma )
	IF ( ( iwx .ge. 0 ) .and. ( iwx .le. 99 ) ) THEN
	    IF ( man ( iwx ) .ge. 0 ) PR_WMAO = man ( iwx )
	END IF
C*
	RETURN
	END
