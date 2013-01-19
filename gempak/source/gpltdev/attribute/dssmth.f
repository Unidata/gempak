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
C*					  2 = parametric		*
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
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER		isend (3), ircv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 6
	isend (2) = CSSMTH
	isend (3) = ismtyp
C
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GPUTR  ( dens, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GPUT  ( ietype, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GPUTR  ( tensn, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( ircv, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Fill output parameters.
C
	iret   = ircv (1)
	jsmtyp = ircv (2)
	CALL GGETR  ( denspt, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GGET  ( jetype, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GGETR  ( tension, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Fill the ACTIVE common block variables
C
	msmtyp = jsmtyp
	tdens  = denspt
	metype = jetype
	ttensn = tension
C*
	RETURN
	END
