	SUBROUTINE MR_MANW  ( datmnw, nmnw, datamw, namw, nlev, stndat,
     +			      iret )
C************************************************************************
C* MR_MANW								*
C*									*
C* This subroutine adds the mandatory wind data to the STNDAT array.    *
C* Winds will only be added at levels which are already in the STNDAT   *
C* array.                						*
C*									*
C* MR_MANW  ( DATMNW, NMNW, DATAMW, NAMW, NLEV, STNDAT, IRET )		*
C*									*
C* Input parameters:							*
C*	DATMNW (3,NMNW)	REAL		Mandatory wind data below 100 mb*
C*	NMNW		INTEGER		Number of man lev below 100 mb	*
C*	DATAMW (3,NAMW)	REAL		Mandatory wind data above 100 mb*
C*	NAMW		INTEGER		Number of man lev above 100 mb	*
C*	NLEV		INTEGER		Number of STNDAT levels		*
C*									*
C* Input and output parameters:						*
C*	STNDAT (6,NLEV)	REAL		Station data			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 2/01	                                      	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	REAL		datmnw (3,*), datamw (3,*), stndat (6,*)
C
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Start processing mandatory data at first level above surface.
C
 	ilev = 2
C
C*	Start wind data at first level.
C
	iwind = 1
C
C*	Loop through merging data below 100 mb.
C
	DO WHILE  ( ( ilev .le. nlev ) .and. ( iwind .le. nmnw ) )
C
C*	    If this is the correct level, add wind data.
C
	    IF  ( datmnw ( 1, iwind ) .eq. stndat ( 1, ilev ) )  THEN
		IF  ( ERMISS ( stndat ( 4, ilev ) ) )  THEN
		    stndat ( 4, ilev ) = datmnw ( 2, iwind )
		    stndat ( 5, ilev ) = datmnw ( 3, iwind )
		END IF
		iwind = iwind + 1
		ilev  = ilev  + 1
C
C*		If mandatory level is above wind, increment wind level.
C
	      ELSE IF (datmnw ( 1, iwind ) .gt. stndat ( 1, ilev )) THEN
		iwind = iwind + 1
C
C*		Otherwise, wind level is above mandatory level, so mand
C*		must be incremented.
C
	      ELSE
		ilev = ilev + 1
	    END IF
	END DO
C
C*	Start wind data at first level.
C
	iwind = 1
C
C*	Loop through merging data above 100 mb.
C
	DO WHILE  ( ( ilev .le. nlev ) .and. ( iwind .le. namw ) )
C
C*	    If this is the correct level, add wind data.
C
	    IF  ( datamw ( 1, iwind ) .eq. stndat ( 1, ilev ) )  THEN
		IF  ( ERMISS ( stndat ( 4, ilev ) ) )  THEN
		    stndat ( 4, ilev ) = datamw ( 2, iwind )
		    stndat ( 5, ilev ) = datamw ( 3, iwind )
		END IF
		iwind = iwind + 1
		ilev  = ilev  + 1
C
C*		If mandatory level is above wind, increment wind level.
C
	      ELSE IF (datamw ( 1, iwind ) .gt. stndat ( 1, ilev )) THEN
		iwind = iwind + 1
C
C*		Otherwise, wind level is above mandatory level, so mand
C*		must be incremented.
C
	      ELSE
		ilev = ilev + 1
	    END IF
	END DO
C*
	RETURN
	END
