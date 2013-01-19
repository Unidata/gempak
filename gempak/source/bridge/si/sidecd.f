	SUBROUTINE SI_DECD ( report, lenr, stid, slat, slon, drct, dtnm,
     +			     iret )
C************************************************************************
C* SI_DECD								*
C*                                                                      *
C* This subroutine decodes one Sea Ice (SI) drift report.		*
C* 								        *
C* SI_DECD  ( REPORT, LENR, STID, DRCT, DTNM, SLAT, SLON, IRET )	*
C*								        *
C* Input parameters:						        *
C*	REPORT		CHAR*		SCD report  		        *
C*	LENR		INTEGER		Report length		        *
C*								        *
C* Output parameters:						        *
C*	STID		CHAR*		Point ID			*
C*	SLAT		REAL		Latitude of the point		*
C*	SLON		REAL		Longitue of the point		*
C*	DRCT		REAL		Sea ice drift direction		*
C*	DTNM		REAL		Distance in nautical miles	*
C*	IRET		INTEGER		Return code                     *
C*					  0 = Normal return             *
C*					 -1 = Zero length - no report   *
C*								        *
C**								        *
C* Log:								        *
C* T. Lee/SAIC		 8/02	Created					*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*) 	report, stid
C*
	REAL 		slat, slon, drct, dtnm
	CHARACTER	carr (5)*12
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize decoded variables to missing.
C
	slat = RMISSD
	slon = RMISSD
	drct = RMISSD
	dtnm = RMISSD
C
C*	Check for zero length report.
C
	IF ( lenr .le. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Break down the report into an array; SI groups are separated
C*	by blanks in the report.
C
	CALL ST_CLST ( report, ' ', ' ', 5, carr, num, iret )
C			   
C*	There are 'num' substrings to decode here.
C
	IF ( num .le. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	If num = 3, report contains drift direction and distance.
C*	If num = 5, report also contains lat/lon of the point.
C
	stid = carr (1)
	IF  ( num .eq. 3 )  THEN
	    CALL ST_C2R ( carr (2), 1, drct, num, ier )
	    CALL ST_C2R ( carr (3), 1, dtnm, num, ier )
	  ELSE
	    CALL ST_LSTR ( carr (2), lens, ier )
	    CALL ST_C2R  ( carr (2) ( :lens-1 ), 1, slat, num, ier )
	    IF  ( carr (2) ( lens:lens ) .eq. 'S' )  slat = - slat
C
	    CALL ST_LSTR ( carr (3), lens, ier )
	    CALL ST_C2R  ( carr (3) ( :lens-1 ), 1, slon, num, ier )
	    IF  ( carr (3) ( lens:lens ) .eq. 'W' )  slon = - slon
	    CALL ST_C2R  ( carr (4), 1, drct, num, ier )
	    CALL ST_C2R  ( carr (5), 1, dtnm, num, ier )
	END IF 
C*
	RETURN
	END
