	SUBROUTINE PS_LCLL  ( datain, tlcl, plcl, iret )
C************************************************************************
C* PS_LCLL								*
C*									*
C* This subroutine computes the lifted condensation level from the 	*
C* surface.								*
C*									*
C* PS_LCLL  ( DATAIN, TLCL, PLCL, IRET )				*
C*									*
C* Input parameters:							*
C*	DATAIN(*)	REAL		Input sounding data		*
C*									*
C* Output parameters:							*
C*	TLCL		REAL		LCL temperature			*
C*	PLCL		REAL		LCL pressure			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		 7/90						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL            datain (*)
C*
	REAL    	rdata (10)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------------
	iret = 0
	tlcl = RMISSD
	plcl = RMISSD
C*
	CALL PC_CMDT ( 5, 6, 7, 0., 1, datain, rdata, ier )
C
C*	Find temperature and pressure at the LCL.
C
        prs = rdata (1)
	tmp = rdata (2)
	dwp = rdata (3)
	IF ( ( ier .ne. 0 ) .or. ERMISS ( prs ) .or. ERMISS ( tmp )
     +	     .or. ERMISS ( dwp ) )   RETURN
	tlcl = PR_TLCL ( tmp, dwp )
	plcl = PR_PLCL ( tmp, prs, tlcl )
C*
	RETURN
	END
