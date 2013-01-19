	SUBROUTINE GEGRP  ( iret )
C************************************************************************
C* GEGRP								*
C* 									*
C* This subroutine ends a new drawing element group.		 	*
C* 									*
C* GEGRP  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* S. Maxwell/GSC	 7/97	                                	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	CALL DEGRP  ( iret )
C*
	RETURN
	END
