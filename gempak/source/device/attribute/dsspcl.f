	SUBROUTINE DSSPCL ( szspcl, ispwid, size, jspwid, iret )
C************************************************************************
C* DSSPCL								*
C*									*
C* This subroutine sets the special symbol parameters.  If these	*
C* parameters are not positive, no changes are made.			*
C*									*
C* DSSPCL ( SZSPCL, ISPWID, SIZE, JSPWID, IRET )			*
C*									*
C* Input parameters:							*
C*	SZSPCL		REAL		Special symbol size		*
C*	ISPWID		INTEGER		Special symbol line width	*
C*									*
C* Output parameters:							*
C*	SIZE		REAL		Special symbol size		*
C*	JSPWID		INTEGER		Special symbol line width	*
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
	IF ( szspcl .gt. 0. ) tsprsz = szspcl
	IF ( ispwid .gt. 0  ) mspwid = ispwid
	size   = tsprsz
	jspwid = mspwid
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSSYMB ( 7, 0, size, jspwid, iret )
	END IF
C*
	RETURN
	END
