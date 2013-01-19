	SUBROUTINE SN_MMAN  ( above, jlev, datpp, npp, dattt, ntt, 
     +			      datman, nlev, iret )
C************************************************************************
C* SN_MMAN								*
C*									*
C* This subroutine merges mandatory data reports (TTAA or TTCC) with    *
C* mandatory wind reports (PPAA or PPCC).				*
C*									*
C* SN_MMAN  ( ABOVE, JLEV, DATPP, NPP, DATTT, NTT, DATMAN, NLEV, IRET ) *
C*									*
C* Input parameters:							*
C*	ABOVE		LOGICAL		Above 100 mb flag		*
C*	JLEV		INTEGER		Index of first output level     *
C*	DATPP (3,NPP)	REAL		Mandatory wind data (PP)	*
C*	NPP		INTEGER		Number of wind levels		*
C*	DATTT (6,NTT)	REAL		Mandatory data (TT)		*
C*	NTT		INTEGER		Number of levels		*
C*									*
C* Input and output parameters:						*
C*	DATMAN (6,NLEV)	REAL		Merged mandatory data		*
C*	NLEV		INTEGER		Number of merged mand. levels	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* M. desJardins/GSFC	 4/89	Eliminate surface data for above 100mb	*
C* D. Kidwell/NCEP	 2/01	Changed calling sequence                *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		datpp ( 3, * ), dattt ( 6, * ), datman ( 6, * )
	LOGICAL		above
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Move TT data to output.
C
	jj = jlev
	DO j = 1, ntt
	    DO i = 1, 6
	        datman ( i, jj ) = dattt ( i, j )
	    END DO
	    jj = jj + 1
	END DO
	nlev = jj - 1
	IF ( npp .le. 0 ) RETURN
C
C*	If there is no mandatory TT data, add surface data.
C
	IF  ( ( ntt .le. 0 ) .and. ( .not. above ) )  THEN
	    DO  i = 1, 6
		datman ( i, 1 ) = RMISSD
	    END DO
	    nlev = 1
	END IF
C
C*	Start processing mandatory data at first level above surface.
C
	IF  ( above )  THEN
	    iman = jlev
	  ELSE
	    iman = 2
	END IF
C
C*	Start wind data at first level.
C
	iwind = 1
C
C*	Loop through merging data till top is reached.
C
	DO WHILE  ( ( iman .le. nlev ) .and. ( iwind .le. npp ) )
C
C*	    If this is the correct level, add wind data.
C
	    IF  ( datpp ( 1, iwind ) .eq. datman ( 1, iman ) )  THEN
		IF  ( ERMISS ( datman ( 4, iman ) ) )  THEN
		    datman ( 4, iman ) = datpp ( 2, iwind )
		    datman ( 5, iman ) = datpp ( 3, iwind )
		END IF
		iwind = iwind + 1
		iman  = iman  + 1
C
C*		If mandatory level is above wind, increment wind level.
C
	      ELSE IF (datpp ( 1, iwind ) .gt. datman ( 1, iman )) THEN
		iwind = iwind + 1
C
C*		Otherwise, wind level is above mandatory level, so mand
C*		must be incremented.
C
	      ELSE
		iman = iman + 1
	    END IF
	END DO
C
C*	Now, add levels at top of sounding if necessary.
C
	DO  i = iwind, npp
	    nlev = nlev + 1
	    datman ( 1, nlev ) = datpp ( 1, i )
	    datman ( 2, nlev ) = RMISSD
	    datman ( 3, nlev ) = RMISSD
	    datman ( 4, nlev ) = datpp ( 2, i )
	    datman ( 5, nlev ) = datpp ( 3, i )
	    datman ( 6, nlev ) = RMISSD
	END DO
C*
	RETURN
	END
