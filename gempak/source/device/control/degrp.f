	SUBROUTINE DEGRP  ( iret )
C************************************************************************
C* DEGRP								*
C* 									*
C* This subroutine ends a new drawing element group.			*
C* 									*
C* DEGRP ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* S. Maxwell/GSC	 7/97						*
C* S. Jacobs/NCEP	 7/97	Added call to HEGRP			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL HEGRP ( iret )
C*
	RETURN
	END
