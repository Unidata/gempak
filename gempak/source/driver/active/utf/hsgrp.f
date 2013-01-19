	SUBROUTINE HSGRP  ( igroup, iret )
C************************************************************************
C* HSGRP - UTF								*
C*									*
C* This subroutine starts a new drawing element group.			*
C*									*
C* HSGRP  ( IGROUP, IRET )						*
C*									*
C* Input parameters:							*
C*	IGROUP		INTEGER		Group type			*
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
	CALL USGRP  ( igroup, iret )
C*
	RETURN
	END
