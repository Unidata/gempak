	SUBROUTINE HSFILL  ( szfil, iftyp, iret )
C************************************************************************
C* HSFILL - GIF								*
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
C* S. Danz/AWC	 	11/03						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
	CALL WSFILL ( szfil, iftyp, iret )
C*
	RETURN
	END
