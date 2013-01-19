	SUBROUTINE DSDASH  ( szdsh, size, iret )
C************************************************************************
C* DSDASH								*
C* 									*
C* This subroutine sets the line dashing scale.  If the parameter is	*
C* not positive, no change is made.					*
C* 									*
C* DSDASH  ( SZDSH, SIZE, IRET )					*
C*                                                                    	*
C* Input parameters:							*
C* 	SZDSH		REAL		Line dashing scale		*
C*                                                                    	*
C* Output parameters:							*
C*	SIZE		REAL		Line dashing scale		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/98						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C-----------------------------------------------------------------------
	iret = NORMAL
C*
	IF  ( szdsh .gt. 0. )  tszdsh = szdsh
	size = tszdsh
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSDASH ( size, iret )
	END IF
C*
	RETURN
	END
