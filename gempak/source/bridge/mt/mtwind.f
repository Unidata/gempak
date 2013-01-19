	SUBROUTINE MT_WIND ( strwnd, idecd, iret )               
C************************************************************************
C* MT_WIND                                                              *
C*                                                                      *
C* This subroutine will decode the wind direction, speed, and gust     	*
C* speed from a METAR wind field.  The values are stored in common.	*
C* Wind speeds are stored in the units in which they are reported.      *
C* 								        *
C* MT_WIND ( STRWND, IDECD, IRET )                    		        *
C*								        *
C* Input parameters: 						        *
C*      STRWND		CHAR*		Wind field			*
C*								        *
C* Output parameters:						        *
C*      RIVALS(IRDRCT)  REAL		Wind direction                  *
C*      RIVALS(IRSKNT)  REAL		Wind speed (kts)                *
C*      RIVALS(IRSPED)  REAL		Wind speed (m/sec)              *
C*      RIVALS(IRGUST)  REAL		Gust wind speed (kts)           *
C*      RIVALS(IRGUMS)  REAL		Gust wind speed (m/sec)         *
C*	IDECD		INTEGER		Decode decision flag            *
C*					  1 = field decoded	        *
C*					  0 = field not decoded     	*
C*	IRET		INTEGER		Return code                     *
C*	      		    		  0 = normal return		*
C*	                                  2 = units in wrong position	*
C*					  3 = presumed in KTS		*
C*					  4 = wind direction miscoding	*
C*					  5 = wind speed miscoding	*
C*					  6 = excessive variable wind	*
C*					  7 = wind gust miscoding	*
C*                                       -1 = no wind field found	*
C**								        *
C* Log:									*
C* D. Kidwell/NCEP 	 4/95                                           *
C* D. Kidwell/NCEP 	11/96   Changed variable wdir flag value to 0   *
C* K. Tyle/GSC		 1/97	Changed var. wind dir. value to IMISSD;	*
C*				additional check for miscoded wdir;	*
C*				change call to DC_WLOG; reorganize	*
C*				header and comments			*
C* K. Tyle/GSC		 2/97	Changed error processing; set limit	*
C*				on minimum string length		*
C* K. Tyle/GSC		 2/97	Handle miscoded units of 'KZ'		*
C* K. Tyle/GSC		 4/97	Check last index of substring >= first	*
C* D. Kidwell/NCEP 	 6/97   Replaced ST_LSTR with INDEX             *
C* D. Kidwell/NCEP 	 4/98   New interface; cleaned up; added KMH;   *
C*				use -99 to flag VRB wdir                *
C* D. Kidwell/NCEP 	 7/02   Fixed bug for gusts reported in KMH     *
C* D. Kidwell/NCEP 	 9/02   Moved most of code to BR_WIND           *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	strwnd
C*
	INCLUDE 	'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret  = -1
	idecd = 0
C
C*	Exit immediately if string is too small.
C
	IF ( INDEX ( strwnd, ' ' ) .le. 5 ) THEN
	    RETURN
	END IF
C
	CALL BR_WIND ( strwnd, drct, sknt, sped, gust, gums, idecd, 
     +		       iret )
	IF ( .not. ERMISS ( drct ) ) rivals ( irdrct ) = drct
	IF ( .not. ERMISS ( sknt ) ) rivals ( irsknt ) = sknt
	IF ( .not. ERMISS ( sped ) ) rivals ( irsped ) = sped
	IF ( .not. ERMISS ( gust ) ) rivals ( irgust ) = gust
	IF ( .not. ERMISS ( gums ) ) rivals ( irgums ) = gums
C*
	RETURN
	END
