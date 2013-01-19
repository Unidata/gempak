	SUBROUTINE HSDASH  ( szdsh, iret )
C************************************************************************
C* HSDASH - VG								*
C*									*
C* This subroutine sets the line dashing scale.				*
C*									*
C* HSDASH  ( SZDSH, IRET )						*
C*									*
C* Input parameters:							*
C*	SZDSH		REAL		Line dashing scale		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/98						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL VSDASH ( szdsh, iret )
C*
	RETURN
	END
