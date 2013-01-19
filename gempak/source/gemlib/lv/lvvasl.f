	SUBROUTINE LV_VASL  ( nexp, nlev, rlevel, iret )
C************************************************************************
C* LV_VASL								*
C*									*
C* This subroutine returns the standard VAS levels of 1000, 920,	*
C* 850, 700, 600, 500, 400, 350, 300, 250, 200, 175, 150, 125 and	*
C* 100 mb.								*
C*									*
C* LV_VASL  ( NEXP, NLEV, RLEVEL, IRET )				*
C*									*
C* Input parameters:							*
C*	NEXP		INTEGER		Maximum number of levels	*
C*									*
C* Output parameters:							*
C*	NLEV		INTEGER		Number of levels		*
C*	RLEVEL (NLEV)	REAL		Levels				*
C*	IRET		INTEGER		Return code			*
C*					  1 = more than NEXP values	*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/86						*
C************************************************************************
	REAL		rlevel (*)
C*
	PARAMETER	( NVAS = 15 )
	REAL		rvas ( NVAS )
C
	DATA		rvas  / 1000., 920., 850., 700., 600., 500., 
     +				 400., 350., 300., 250., 200., 175., 
     +				 150., 125., 100. /
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for number of levels to return.
C
	IF  ( nexp .ge. NVAS )  THEN
	    nlev = NVAS
	  ELSE
	    nlev = nexp
	    iret = 1
	END IF
C
C*	Move the levels into the output array.
C
	DO  i = 1, nlev
	    rlevel (i) = rvas (i)
	END DO
C*
	RETURN
	END
