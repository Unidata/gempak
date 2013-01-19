	CHARACTER*(*) FUNCTION PT_WTMO  ( wwmo )
C************************************************************************
C* PT_WTMO								*
C*									*
C* This character function converts a numeric WMO weather code,		*
C* WWMO, into a character code:						*
C*									*
C*                     WTMO = PT_WTMO ( WWMO )				*
C*									*
C* The conversion is:							*
C*         0 =             34 = BD+          67 = ZR           		*
C*         1 =             35 = BD+          68 = R-S-            	*
C*         2 =             36 = BS           69 = RS         		*
C*         3 =             37 = BS+          70 = S-           		*
C*         4 = K           38 = BS           71 = S-			*
C*         5 = H           39 = BS+          72 = S          		*
C*         6 = D           40 =              73 = S            		*
C*         7 = BD          41=  F            74 = S+			*
C*         8 = PO          42 = F            75 = S+         		*
C*         9 =             43 = F            76 = IN            	*
C*        10 = F           44 = F            77 = SG			*
C*        11 = GF          45 = F            78 = IC         		*
C*        12 = GF          46 = F            79 = IP           		*
C*        13 =             47 = F            80 = RW-			*
C*        14 =             48 = IF           81 = RW         		*
C*        15 =             49 = IF           82 = RW+          		*
C*        16 =             50 = L-           83 = RW-SW-		*
C*        17 = T           51 = L-           84 = RWSW        		*
C*        18 = Q           52 = L            85 = SW-          		*
C*        19 = FUNNE       53 = L            86 = SW 			*
C*        20 =             54 = L+           87 = IPW-			*
C*        21 =             55 = L+           88 = IPW			*
C*        22 =             56 = ZL-          89 = A-			*
C*        23 =             57 = ZL           90 = A         		*
C*        24 =             58 = R-L-         91 = R-           		*
C*        25 =             59 = RL           92 = R 			*
C*        26 =             60 = R-           93 = RS         		*
C*        27 =             61 = R-           94 = R+S+         		*
C*        28 =             62 = R            95 = TRW-			*
C*        29 =             63 = R            96 = TRW-A        		*
C*        30 = BD          64 = R+           97 = TRW+			*
C*        31 = BD          65 = R+           98 = TD 			*
C*        32 = BD          66 = ZR-          99 = TRW+A			*
C*        33 = BD+							*
C*									*
C*       105 = TSW-       107 = TSW+					*
C*       201 = V          202 = BY          203 = UP			*
C*									*
C* CHARACTER*(*) PT_WTMO  ( WWMO )					*
C*									*
C* Input parameters:							*
C*	WWMO		REAL		Numeric weather code		*
C*									*
C* Output parameters:							*
C*	PT_WTMO		CHAR*		Character weather code		*
C**									*
C* Log:									*
C* I. Graffman/RDS	12/84						*
C* S. Schotz/GSC	10/89	Documentation				*
C* S. Jacobs/NCEP	11/96	Clean up and redefine some values	*
C* S. Jacobs/NCEP	11/96	Redefine SP -> IPW; F- -> F		*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
	CHARACTER*8	wthr (0:99)
	DATA		wthr  / 4 * ' ', 'K', 'H', 'D', 'BD', 'PO',
     +				' ', 'F', 'GF', 'GF', 4 * ' ', 'T',
     +				'Q', 'FUNNE', 10 * ' ', 3 * 'BD',
     +				3 * 'BD+', 'BS', 'BS+', 'BS', 'BS+',
     +				' ', 7 * 'F', 'IF', 'IF', 'L-', 'L-',
     +				'L', 'L', 'L+', 'L+', 'ZL-', 'ZL',
     +				'R-L-', 'RL', 'R-', 'R-', 'R', 'R',
     +				'R+', 'R+', 'ZR-', 'ZR', 'R-S-', 'RS',
     +				'S-', 'S-', 'S', 'S', 'S+', 'S+',
     +				'IN', 'SG', 'IC', 'IP', 'RW-', 'RW',
     +				'RW+', 'RW-SW-', 'RWSW', 'SW-', 'SW',
     +				'IPW-', 'IPW', 'A- ', 'A', 'R-', 'R',
     +				'RS', 'R+S+', 'TRW-', 'TRW-A', 'TRW+',
     +				'TD', 'TRW+A' /
C------------------------------------------------------------------------
	PT_WTMO = ' '
	icode = wwmo
C
C*	Find the weather code from the data table.
C
	IF  ( ( icode .ge. 0 ) .and. ( icode .le. 99 ) )  THEN
	    PT_WTMO = wthr ( icode )
C
C*	    Otherwise, set the special cases.
C
	  ELSE IF ( icode .eq. 105 )  THEN
	    PT_WTMO = 'TSW-'
	  ELSE IF ( icode .eq. 107 )  THEN
	    PT_WTMO = 'TSW+'
	  ELSE IF ( icode .eq. 201 )  THEN
	    PT_WTMO = 'V'
	  ELSE IF ( icode .eq. 202 )  THEN
	    PT_WTMO = 'BY'
	  ELSE IF ( icode .eq. 203 )  THEN
	    PT_WTMO = 'UP'
	END IF
C*
	RETURN
	END
