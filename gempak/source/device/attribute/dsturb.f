	SUBROUTINE DSTURB ( szturb, ituwid, size, jtuwid, iret )
C************************************************************************
C* DSTURB								*
C*									*
C* This subroutine sets the turbulence symbol parameters.  If these	*
C* parameters are not positive, no changes are made.			*
C*									*
C* DSTURB ( SZTURB, ITUWID, SIZE, JTUWID, IRET )			*
C*									*
C* Input parameters:							*
C*	SZTURB		REAL		Turbulence symbol size		*
C*	ITUWID		INTEGER		Turbulence symbol line width	*
C*									*
C* Output parameters:							*
C*	SIZE		REAL		Turbulence symbol size		*
C*	JTUWID		INTEGER		Turbulence symbol line width	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 8/96	Based on DSWTHR				*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C-----------------------------------------------------------------------
	iret = NORMAL
C
	IF ( szturb .gt. 0. ) ttursz = szturb
	IF ( ituwid .gt. 0  ) mtuwid = ituwid
	size   = ttursz
	jtuwid = mtuwid
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSSYMB ( 8, 0, size, jtuwid, iret )
	END IF
C*
	RETURN
	END
