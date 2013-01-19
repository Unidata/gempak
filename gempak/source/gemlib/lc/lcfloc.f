	SUBROUTINE LC_FLOC  ( point, rlat, rlon, iret )
C************************************************************************
C* LC_FLOC								*
C*									*
C* This subroutine translates a location into a latitude and		*
C* longitude.  The location may be entered in the following ways:	*
C*									*
C*	LAT;LON								*
C*	LAT/LON								*
C*	character station identifier					*
C*	station number							*
C*									*
C* The surface station table, the upper-air station table and the 	*
C* world station table will be searched for stations.			*
C*									*
C* LC_FLOC  ( POINT, RLAT, RLON, IRET )					*
C*									*
C* Input parameters:							*
C*	POINT		CHAR*		Location			*
C*									*
C* Output parameters:							*
C*	RLAT		REAL		Latitude			*
C*	RLON		REAL		Longitude			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return 		*
C*					 -4 = station not in table	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/89						*
C* M. desJardins/GSFC	 4/90	Add error msg if table cannot be opened	*
C* K. Brill/NMC		 8/93	stnid*4 -> stnid*8			*
C* S. Jacobs/NMC	 7/94	Added station table name to call to 	*
C*				LC_FSTN; Used new LC_FSTN to search	*
C*				upper air and world station tables	*
C* D. Keiser/GSC	12/95	Changed table parameters to filenames	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*) 	point
C*
	CHARACTER	ppp*12, sep*1
	REAL		rarr (2)
C------------------------------------------------------------------------
	iret = -4
	rlat = 0.0
	rlon = 0.0
C
C*	Check for a blank.
C
	IF  ( point .eq. ' ' )  RETURN
C
C*	Check for a semicolon or slash.
C
	islash = INDEX ( point, '/' )
	isemic = INDEX ( point, ';' )
	IF  ( islash .ne. 0 )  THEN
	    sep = '/'
	  ELSE IF  ( isemic .ne. 0 )  THEN
	    sep = ';'
	  ELSE
	    sep = ' '
	END IF
C
C*	If the lat and lon were entered, get the two numbers.
C
	IF  ( sep .ne. ' ' )  THEN
	    CALL ST_RLST  ( point , sep, RMISSD, 2, rarr, n, ier )
	    IF  ( ( rarr (1) .ne. RMISSD ) .and. 
     +		  ( rarr (2) .ne. RMISSD ) )  iret = 0
	    rlat = rarr (1)
	    rlon = rarr (2)
	  ELSE
C
C*	    Convert the station id to upper case.
C
	    CALL ST_LCUC  ( point, ppp, ier )
C
C*	    First, check the surface table.
C
	    CALL LC_FSTN  ( 'sfstns.tbl', ppp, rlat, rlon, ier )
	    IF  ( ier .eq. 0 )  iret = 0
C
C*	    If station is not found, check the upper air table.
C
	    IF  ( iret .ne. 0 )  THEN
		CALL LC_FSTN  ( 'snstns.tbl', ppp, rlat, rlon, ier )
		IF  ( ier .eq. 0 )  iret = 0
	    END IF
C
C*	    If station is still not found, check the world table.
C
	    IF  ( iret .ne. 0 )  THEN
		CALL LC_FSTN  ( 'snworld.tbl', ppp, rlat, rlon, ier )
		IF  ( ier .eq. 0 )  iret = 0
	    END IF
	END IF
C*
	RETURN
	END
