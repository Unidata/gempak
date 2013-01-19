	SUBROUTINE HEGRP  ( iret )
C************************************************************************
C* HEGRP - UTF								*
C*									*
C* This subroutine ends a drawing element group.			*
C*									*
C* HEGRP  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/99						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL UEGRP  ( iret )
C*
	RETURN
	END
