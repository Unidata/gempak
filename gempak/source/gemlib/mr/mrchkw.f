	SUBROUTINE MR_CHKW ( datsgw, nsgw, datasw, nasw, zbwind,
     +			     zawind, iret )
C************************************************************************
C* MR_CHKW								*
C*									*
C* This subroutine checks the significant wind data to see if it is	*
C* on pressure or height surfaces.  If the wind is on pressure		*
C* surfaces, the first valid pressure has been made negative.		*
C*									*
C* MR_CHKW ( DATSGW, NSGW, DATASW, NASW, ZBWIND, ZAWIND, IRET )		*
C*									*
C* Input parameters:							*
C*	DATSGW (3,NSGW)	REAL		Sig wind below 100 mb		*
C*	NSGW		INTEGER		Number of levels below 100 mb	*
C*	DATASW (3,NASW)	REAL		Sig wind above 100 mb		*
C*	NASW		INTEGER		Number of levels above 100 mb	*
C*									*
C* Output parameters:							*
C*	ZBWIND		LOGICAL		Data below 100 mb on z		*
C*	ZAWIND		LOGICAL		Data above 100 mb on z		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		datsgw ( 3, * ), datasw ( 3, * )
	LOGICAL		zbwind, zawind
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Check data below 100 mb.
C
	zbwind = .true.
	ilev   =  1
	DO WHILE  ( ilev .le. nsgw )
	    IF  ( .not. ERMISS ( datsgw ( 1, ilev ) ) )  THEN
		IF  ( datsgw ( 1, ilev ) .lt. 0. ) zbwind = .false.
		ilev = nsgw + 1
	      ELSE
		ilev = ilev + 1
	    END IF
	END DO
C
C*	Check data above 100 mb.
C
	zawind = .true.
	ilev   =  1
	DO WHILE  ( ilev .le. nasw )
	    IF  ( .not. ERMISS ( datasw ( 1, ilev ) ) )  THEN
		IF  ( datasw ( 1, ilev ) .lt. 0. ) zawind = .false.
		ilev = nasw + 1
	      ELSE
		ilev = ilev + 1
	    END IF
	END DO
C*
	RETURN
	END
