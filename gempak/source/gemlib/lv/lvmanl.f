	SUBROUTINE LV_MANL  ( nexp, nlev, rlevel, iret )
C************************************************************************
C* LV_MANL								*
C*									*
C* This subroutine returns the mandatory levels below 100 mb.		*
C*									*
C* LV_MANL  ( NEXP, NLEV, RLEVEL, IRET )				*
C*									*
C* Input parameters:							*
C*	NEXP		INTEGER		Maximum number of levels	*
C*									*
C* Output parameters:							*
C*	NLEV		INTEGER		Number of levels 		*
C*	RLEVEL (NLEV)	REAL		Levels				*
C*	IRET		INTEGER		Return code			*
C*					  1 = more than NEXP values	*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/86						*
C* K. Brill/NMC		01/92	Added 925 mb level			*
C************************************************************************
	REAL		rlevel (*)
C*
	PARAMETER	( NMAN = 12 )
	REAL		rman ( NMAN )
C
	DATA		rman  /   0., 1000., 925., 850., 700., 500.,
     +				400., 300.,  250., 200., 150., 100. /
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for number of levels to return.
C
	IF  ( nexp .ge. NMAN )  THEN
	    nlev = NMAN
	  ELSE
	    nlev = nexp
	    iret = 1
	END IF
C
C*	Move the levels into the output array.
C
	DO  i = 1, nlev
	    rlevel (i) = rman (i)
	END DO
C*
	RETURN
	END
