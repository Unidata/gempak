	SUBROUTINE HSFILL  ( szfil, iftyp, iret )
C************************************************************************
C* HSFILL - PS								*
C* 									*
C* This subroutine sets the fill pattern type and size.			*
C* 									*
C* HSFILL  ( SZFIL, IFTYP, IRET )					*
C*									*
C* Input parameters:							*
C* 	SZFIL		REAL		Fill pattern size 		*
C* 	IFTYP		INTEGER		Fill pattern type		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/98						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
	CALL PSFILL ( szfil, iftyp, iret )
C*
	RETURN
	END
