	SUBROUTINE HEGRP  ( iret )
C************************************************************************
C* HEGRP - VG								*
C*									*
C* This subroutine ends a drawing element group.			*
C*									*
C* HEGRP  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 7/97						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL VEGRP  ( iret )
C*
	RETURN
	END
