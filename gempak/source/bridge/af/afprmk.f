	SUBROUTINE AF_PRMK  ( report, isrmk, iermk, iret )
C************************************************************************
C* AF_PRMK								*
C*									*
C* This subroutine decodes and stores the remarks from within a PIREP   *
C* report.								*
C*									*
C* AF_PRMK  ( REPORT, ISRMK, IERMK, IRET )				*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		PIREP report 			*
C*	ISRMK		INTEGER		Pointer to start of remarks 	*
C*					within REPORT 			*
C*	IERMK		INTEGER		Pointer to end of remarks  	*
C*					within REPORT 			*
C*									*
C* Input and output parameters:						*
C*	RIVALS (IRNTRB)	REAL		Number of turbulence levels     *
C*									*
C* Output parameters:							*
C*	RIVALS (IRTPOT)	REAL		Type of turbulence              *
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 7/99						*
C* D. Kidwell/NCEP	 8/99	Added check for haze                    *
C* J. Ator/NCEP		09/99	Modify ntrb increment, add MXNLYR check	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	report
C-----------------------------------------------------------------------
	iret = 0
C
C*	Break up the input string into groups of "like-type" in order
C*	to facilitate decoding.
C
	CALL AF_BKGP  ( report ( isrmk : iermk ), ierbgp )
	IF  ( ierbgp .ne. 0 )  THEN
	    RETURN
	END IF
C
C*	Look for low level wind shear (LLWS) remark.
C
	CALL ST_FIND ( 'LLWS', fields, nflds, ipos, ier )
	IF ( ipos .gt. 0 ) THEN
	    ntrb = NINT ( rivals ( irntrb ) ) + 1
	    IF  ( ntrb .le. MXNLYR )  THEN
		rivals ( irtpot ( ntrb ) ) = 3
		rivals ( irntrb ) = FLOAT ( ntrb )
	    END IF
	END IF
C
C*	Look for haze reported as a remark.
C
	CALL AF_PHAZ ( ier )
C*
	RETURN
	END
