	FUNCTION PR_P03D ( ctend )
C************************************************************************
C* PR_P03D								*
C*									*
C* This subroutine translates a WMO pressure tendency string into the	*
C* pressure tendency code * 1000 + the magnitude of the change in	*
C* tenths of millibars.							*
C*									*
C* REAL PR_P03D ( CTEND )						*
C*									*
C* Input parameters:							*
C*	CTEND		CHAR*		WMO pressure tendency code	*
C*									*
C* Output parameters:							*
C*	PR_P03D		REAL		Pressure change information	*
C**									*
C* Log:									*
C* K. Brill/NMC		11/91						*
C* K. Brill/NMC		02/92	Translate into a 4-digit number		*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	ctend
C*
	CHARACTER*4	cc
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	PR_P03D = RMISSD
C
C*	Get pressure tendency information.
C
	cc = ctend (1:4)
	IF  ( cc (1:1) .ge. '0' .and. cc (1:1) .le. '8' .and.
     +        cc (2:4) .ne. '999' )  THEN
	    CALL ST_CRNM  ( cc, ptend, ier )
	    IF  ( .not. ERMISS ( ptend ) )  THEN
		PR_P03D = ptend
	    END IF
	END IF
C*
	RETURN
	END
