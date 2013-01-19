	SUBROUTINE SNTTTL  ( ctime,  area, level, ivcord, parm, stnprm,
     +			     ttlstr, iret )
C************************************************************************
C* SNTTTL								*
C*									*
C* This subroutine creates a title for SNTSER.  			*
C*									*
C* SNTTTL  ( CTIME, AREA, LEVEL, IVCORD, PARM, STNPRM, TTLSTR, IRET )	*
C*									*
C* Input parameters:							*
C*	CTIME		CHAR*		String showing date/time range	*
C*	AREA		CHAR*		User-specified location		*
C*	LEVEL		CHAR*		Level				*
C*	IVCORD		INTEGER		Number of vertical coordinate	*
C*	PARM		CHAR*		Level parameter			*
C*	STNPRM		CHAR*		Station parameter		*
C*									*
C* Output parameters:							*
C*	TTLSTR		CHAR*		Title string			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 5/89	Adapted from GDTTTL			*
C************************************************************************
	CHARACTER*(*)	ctime, area, level, parm, stnprm, ttlstr
C*
	CHARACTER	outprm*4, carea*48, clev*48
C------------------------------------------------------------------------
	iret = 0
C
C*	Load the parameter name.
C
	IF  ( parm .ne. ' ' )  THEN
	    outprm = parm
	  ELSE
	    outprm = stnprm
	END IF
	CALL ST_LSTR  ( outprm, lenp, ier )
C
C*	Convert AREA to upper case and drop a leading '@', if any, then
C*	find the length.
C
	CALL ST_LCUC  ( area, carea, ier )
	IF  ( carea ( 1:1 ) .eq. '@' )  carea = carea ( 2: )
	CALL ST_LSTR  ( carea, lena, ier )
C
C*	Convert LEVEL=0 to 'SFC'; otherwise, tack units onto level 
C*	according to the vertical coord.
C
	CALL ST_LSTR  ( level, lenl,  ier )
	IF  ( level ( 1:lenl ) .eq. '0' )  THEN
	    clev = 'SFC'
	    lenl = 3
	  ELSE IF  ( ivcord .eq. 1 )  THEN
	    clev = level ( 1:lenl ) // ' MB'
	    lenl = lenl + 3
	  ELSE IF  ( ivcord .eq. 2 )  THEN
	    clev = level ( 1:lenl ) // ' K'
	    lenl = lenl + 2
	  ELSE IF  ( ivcord .eq. 3 )  THEN
	    clev = level ( 1:lenl ) // ' M'
	    lenl = lenl + 2
	  ELSE
	    clev = level ( 1:lenl )
	END IF
C
C*	Get the length of the time range string.
C
	CALL ST_LSTR  ( ctime, lent, ier )
C
C*	Put the title string together.  The maximum length permitted
C*	is the length of TTLSTR scaled down by the X extent of the view
C*	region (GQVIEW returns fractions of the physical device).  If 
C*	the "full" title is too long, drop some intermediate verbiage,
C*	then drop the time.
C
	CALL GQVIEW  ( xllf, yllf, xurf, yurf, ier )
	lenmax = LEN ( ttlstr ) * NINT ( xurf - xllf )
C
	lenttl = lenp + lenl + lena + lent + 3
	IF  ( lenttl .le. lenmax )  THEN
	    ttlstr = outprm ( 1:lenp ) // ' ' //
     +		     clev   ( 1:lenl ) // ' ' //
     +		     carea  ( 1:lena ) // ' ' //
     +		     ctime  ( 1:lent )
	  ELSE
	    ttlstr = outprm ( 1:lenp )  // ' ' //
     +		     clev   ( 1:lenl )  // ' ' //
     +		     carea  ( 1:lena )
	END IF
C*
	RETURN
	END
