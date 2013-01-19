	SUBROUTINE OA_NAVG  ( rnvblk, gsnvbk, navsz, gsflag, iret )
C************************************************************************
C* OA_NAVG								*
C*									*
C* This subroutine checks the navigation block of the analysis grid	*
C* against that of the guess grid.					*
C*									*
C* OA_NAVG  ( RNVBLK, GSNVBK, NAVSZ, GSFLAG, IRET )			*
C*									*
C* Input parameters:							*
C*	RNVBLK (NAVSZ)	REAL		Analysis grid navigation block	*
C*	GSNVBK (NAVSZ)	REAL		Guess grid navigation block	*
C*	NAVSZ		INTEGER		Navigation length		*
C*									*
C* Output parameters:							*
C*	GSFLAG		LOGICAL		Check flag			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = no match			*
C**									*
C* Log:									*
C* K. Brill/GSC		4/90						*
C* K. Brill/NMC		1/92		Call GR_CNAV			*
C************************************************************************
	REAL		rnvblk (*), gsnvbk (*)
	LOGICAL		gsflag
C------------------------------------------------------------------------
	iret = 0
C*
	CALL GR_CNAV ( rnvblk, gsnvbk, navsz, gsflag, ier )
	IF ( .not. gsflag ) THEN
		iret   = -4
		CALL ER_WMSG ( 'OA', iret, ' ', ier )
	END IF
C*
	RETURN
	END
