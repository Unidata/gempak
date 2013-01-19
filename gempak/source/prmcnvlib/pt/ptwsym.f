	FUNCTION PT_WSYM  ( wthr )
C************************************************************************
C* PT_WSYM								*
C*									*
C* This function converts a character weather code, WTHR, into 		*
C* a synoptic numeric code for the weather symbol number, which is	*
C* used to draw weather symbols.					*
C*									*
C*                    WSYM = PT_WSYM ( WTHR )				*
C*									*
C* The conversion that is used is based upon that which the National	*
C* Meteorological Center (NMC) uses to convert hourly alphanumeric	*
C* characters to the synoptic weather code.  This conversion is shown 	*
C* in the following table.  Note that some GEMPAK codes have been added.*
C* These are denoted in lower case.  Also note, 10 has been added to	*
C* codes that have two symbols for the snow case as a convention.	*
C*									*
C*	 4 =  K				 71 =  S-			*
C* 	 5 =  KH HK H			 73 =  S			*
C*  	 6 =  KD HD D			 75 =  S+			*
C*  	 7 =  BD BN N   		 76 =  IN			*
C*	 8 =  PO                         77 =  SGW SG SGW-		*
C*  	10 =  KF HF F   		 78 =  IC			*
C*      12 =  GF HGF                     79 =  IP R-IP			*
C*  	17 =  T+ T  			 80 =  RW-			*
C*	18 =  Q   			 81 =  RW+ RW			*
C*	19 =  TORNA FUNNE WATER          83 =  RW-S			*
C*	34 =  BD+              		 84 =  RWSW RW+S		*
C*	38 =  BS 			 85 =  SW-			*
C* 	39 =  BS+			 86 =  SW+ SW			*
C* 	48 =  IF 			 87 =  AP- IPW- SPW- SP-	*
C*	51 =  L-			 88 =  AP+ SPW AP IPW IPW+ SP	*
C*	53 =  L LS			 89 =  RW-A A-			*
C*	55 =  L+			 90 =  RW+A A A+ RWA		*
C*	56 =  ZL- ZLW-			 95 =  TRW RT TR TRW- T-R	*
C*	57 =  ZL+ ZL			 96 =  T-A TA TRWA TRW-A	*
C*	58 =  R-L- L-R-			 97 =  TRW+ T+R TR+		*
C*	59 =  RL L+R+ LR		 98 =  TBN TBD T+BN T+BD TD	*
C*	61 =  R-			 99 =  T+A TRW+A		*
C*	63 =  R				105 =  T-S TSW TSW- TS		*
C*	65 =  R+			107 =  TSW+ T+RS T+S		*
C*	66 =  ZR- ZRW-			201 =  V			*
C*	67 =  ZR+ ZR			202 =  BY			*
C* 	68 =  L-S- R-S- R+S- R-S RS- 	203 =  UP			*
C*	69 =  L+S+ R+S+ RS R-S+						*
C*									*
C*  REAL PT_WSYM ( WTHR )						*
C*									*
C* Input parameters:							*
C*	WTHR		CHAR*		Character weather code		*
C*									*
C* Output parameters:							*
C*	PT_WSYM		REAL		Numeric weather code		*
C**									*
C* Log:									*
C* S. Schotz/GSC	 4/90						*
C* S. Jacobs/NCEP	11/96	Added V, PO, BY, UP; Changed IC, IN, IF	*
C* S. Jacobs/NCEP	11/96	Fixed total number of symbols		*
C************************************************************************
	CHARACTER*(*)	wthr
C*
	CHARACTER*12	wcod (119), buff
	INTEGER    	wpnt (119)
C*
	DATA	wcod /	'A', 'D', 'F', 'H', 'K', 'L', 'N', 'Q', 'R', 
     +			'S', 'T', 'V',
     +			'A-', 'A+', 'AP', 'BD', 'BN', 'BS',
     +			'BY', 'GF', 'HD', 'HF', 'HK', 'IC', 'IF', 'IN',
     +			'IP', 'KD', 'KF', 'KH', 'L-', 'L+', 'LR', 'LS',
     +			'PO', 'R-', 'R+', 'RL', 'RS', 'RT', 'RW', 'S-',
     +			'S+', 'SG', 'SP', 'SW', 'T+', 'TA', 'TD', 'TR',
     +			'TS', 'UP', 'ZL', 'ZR', 
     +			'AP-', 'AP+', 'BD+', 'BS+', 'HGF', 'IPW', 'R-S',
     +			'RS-', 'RW-', 'RW+', 'RWA', 'SGW', 'SP-', 'SPW', 
     +			'SW-', 'SW+', 'T-A', 'T+A', 'T-R', 'T+R', 'T-S', 
     +			'T+S', 'TBD', 'TBN', 'TR+', 'TRW', 'TSW', 'ZL-',
     +			'ZL+', 'ZR-', 'ZR+',
     +			'IPW-', 'IPW+', 'L-R-', 'L+R+', 'L-S-', 'L+S+',
     +			'R-IP', 'R-L-', 'R-S+', 'R+S-', 'R-S-', 'R+S+', 
     +			'RW-A', 'RW+A', 'RW-S', 'RW+S', 'RWSW', 'SGW-', 
     +			'SPW-', 'T+BD', 'T+BN', 'T+RS', 'TRW-', 'TRW+', 
     +			'TRWA', 'TSW-', 'TSW+', 'ZLW-', 'ZRW-',
     +			'FUNNE', 'TORNA', 'TRW-A', 'TRW+A', 'WATER' /
C
C*	The following array points the character weather code to
C*	the numeric WYSM code.
C
	DATA	wpnt /	 90,   6,  10,   5,   4,  53,   7,  18,  63,
     +			 73,  17, 201,
     +                   89,  90,  88,   7,   7,  38,
     +			202,  12,   6,  10,   5,  78,  48,  76,
     +			 79,   6,  10,   5,  51,  55,  59,  53,
     +			  8,  61,  65,  59,  69,  95,  81,  71,
     +			 75,  77,  88,  86,  17,  96,  98,  95,
     +			105, 203,  57,  67, 
     +			 87,  88,  34,  39,  12,  88,  68,
     +			 68,  80,  81,  90,  77,  87,  88,
     +			 85,  86,  96,  99,  95,  97, 105,
     +			107,  98,  98,  97,  95, 105,  56,
     +			 57,  66,  67,
     +			 87,  88,  58,  59,  68,  69,
     +			 79,  58,  69,  68,  68,  69,
     +			 89,  90,  83,  84,  84,  77,
     +			 87,  98,  98, 107,  95,  97,
     +			 96, 105, 107,  56,  66,
     +			 19,  19,  96,  99,  19 /
C*
	INCLUDE 'GEMPRM.PRM'
C------------------------------------------------------------------------
C
	PT_WSYM = RMISSD
C
C*	Remove leading spaces in the string.
C
	CALL ST_LDSP  ( wthr, buff, lenout, ier )
C
C*	If there is no weather, return.
C
	IF  ( buff .eq. ' ' )  RETURN
C
C*	Check each code. Save longest match.
C
	DO  i = 1, 119
C
C*	    Find length of string to match.
C
            IF  ( i .le. 12 ) THEN
                ilen = 1
            ELSE IF ( i .le. 54 ) THEN
                ilen = 2
            ELSE IF ( i .le. 85 ) THEN
                ilen = 3
            ELSE IF ( i .le. 114 ) THEN
                ilen = 4
            ELSE 
                ilen = 5
            END IF
	    IF  ( buff ( 1:ilen ) .eq. wcod ( i ) ) PT_WSYM = wpnt (i)
C
	END DO
C*
	RETURN
	END
