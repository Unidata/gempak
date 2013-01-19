	SUBROUTINE PC_MAND  ( mandat, iret )
C************************************************************************
C* PC_MAND								*
C*									*
C* This subroutine sets the interpolation flag for the pc library.      *
C* If "/MAN" is specified as part of the LEVELS input variable, no      *
C* interpolation is done.                                               *
C*									*
C* PC_MAND ( MANDAT, IRET )                                             *
C*									*
C* Input parameters:							*
C*	MANDAT		LOGICAL		Only mandatory data flag        *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = PC_INIT must be called	*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 5/99						*
C* T. Lee/GSC		10/99	Reset PC table index flag		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	LOGICAL		mandat
C------------------------------------------------------------------------
C*	Check that PC_INIT has been called.
C
	IF  ( .not. dsflg )  THEN
	    iret = -4
	    RETURN
	ENDIF
	iret = 0
C
C*	Set the interpolation flag.
C
	IF  ( inton .neqv. .not. mandat )  THEN
	    inton = .not. mandat
	    IF  ( inton ) sindxf = .false.
	END IF
C*
	RETURN
	END
