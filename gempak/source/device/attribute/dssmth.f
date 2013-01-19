	SUBROUTINE DSSMTH ( ismtyp, dens, ietype, tensn,
     +			    jsmtyp, denspt, jetype, tension, iret )
C************************************************************************
C* DSSMTH								*
C*									*
C* This subroutine sets the line smoothing attributes.			*
C*									*
C* DSSMTH ( ISMTYP, DENS, IETYPE, TENSN, JSMTYP, DENSPT, JETYPE,	*
C*	    TENSION, IRET )						*
C*									*
C* Input parameters:							*
C*	ISMTYP		INTEGER		Smoothing type			*
C*					  0 = none			*
C*					  1 = splines			*
C*					  2 = parametric 		*
C*	DENS		REAL		Density of intermediate points	*
C*	IETYPE		INTEGER		End point type			*
C*	TENSN		REAL		Line tension			*
C*									*
C* Output parameters:							*
C*	JSMTYP		INTEGER		Smoothing type			*
C*	DENSPT		REAL		Density of intermediate points	*
C*	JETYPE		INTEGER		End point type			*
C*	TENSION		REAL		Line tension			*
C*	IRET		INTEGER		Return code			*
C**									*
C* S. Jacobs/NCEP	 2/98						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C*
	IF  ( ismtyp .ge. 0 )  msmtyp = ismtyp
	IF  ( dens   .ge. 0 )  tdens  = dens
	IF  ( ietype .gt. 0 )  metype = ietype
	IF  ( tensn  .ge. 0 )  ttensn = tensn
C
	jsmtyp  = msmtyp
	denspt  = tdens
	jetype  = metype
	tension = ttensn
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSSMTH ( jsmtyp, denspt, jetype, tension, iret )
	END IF
C*
	RETURN
	END
