	SUBROUTINE HSFILL  ( szfil, iftyp, iret )
C************************************************************************
C* HSFILL - XWP								*
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
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
	    CALL XSFILL ( szfil, iftyp, iret )
	  ELSE IF  ( ddev .eq. 'PS' )  THEN
	    CALL PSFILL ( szfil, iftyp, iret )
	END IF
C*
	RETURN
	END
