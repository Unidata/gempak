	SUBROUTINE G2T_STAT ( nt, nz, iret )
C************************************************************************
C* G2T_STAT								*
C*									*
C* This subroutine processes the RANGE of the gridded data and applies	*
C* histogram analysis if necessary.					*
C*									*
C* G2T_STAT ( NT, NZ, IRET )						*
C*									*
C* Input parameters:							*
C*	NT		INTEGER		Nth time step			*
C*	NZ		INTEGER		Nth zone area			*
C*									*
C* Output Parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		03/07						*
C* T. Lee/SAIC		11/07	Initialized zone flag for minimum value	*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	INTEGER		jwave(2), jwind(2)
	LOGICAL		wok, sok
C-----------------------------------------------------------------------
	iret = 0
C
C*	Initialize minimum zone flag.  If true, only one zone contains
C*	minimum value which will be included in the EXCEPT part.
C
	DO ii = 1, 2
	    mnzflg ( ii ) = .false.
	END DO
C
C*	Check if the range lies within the guideline.
C
	jwave ( 1 ) = mnval ( 1, 1, nt )
	jwave ( 2 ) = mxval ( 1, 1, nt )
	jwind ( 1 ) = mnval ( 2, 1, nt )
	jwind ( 2 ) = mxval ( 2, 1, nt )
C
	CALL G2T_CHECK ( 0, jwave, jwind, wok, sok, ier )
C
C*	If not, apply statistical analysis to acquire range information. 
C
	IF  ( .not. wok .or. .not. sok )
     +		CALL G2T_HIST ( nt, nz, wok, sok, iret )
C*
	RETURN
	END
