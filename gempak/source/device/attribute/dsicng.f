	SUBROUTINE DSICNG ( szicng, icewid, size, jcewid, iret )
C************************************************************************
C* DSICNG								*
C*									*
C* This subroutine sets the icing symbol parameters.  If these		*
C* parameters are not positive, no changes are made.			*
C*									*
C* DSICNG ( SZICNG, ICEWID, SIZE, JCEWID, IRET )			*
C*									*
C* Input parameters:							*
C*	SZICNG		REAL		Icing symbol size		*
C*	ICEWID		INTEGER		Icing symbol line width		*
C*									*
C* Output parameters:							*
C*	SIZE		REAL		Icing symbol size		*
C*	JCEWID		INTEGER		Icing symbol line width		*
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
	IF ( szicng .gt. 0. ) tcersz = szicng
	IF ( icewid .gt. 0  ) mcewid = icewid
	size   = tcersz
	jcewid = mcewid
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSSYMB ( 3, 0, size, jcewid, iret )
	END IF
C*
	RETURN
	END
