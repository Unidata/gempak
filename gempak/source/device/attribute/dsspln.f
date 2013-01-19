	SUBROUTINE DSSPLN  ( isltyp, islstr, isldir, slsiz, islwid,
     +			     jsltyp, jslstr, jsldir, size, jslwid, iret )
C************************************************************************
C* DSSPLN								*
C*									*
C* This subroutine sets special line attributes including the special	*
C* line type number, stroke multiplier, direction indicator, size, and	*
C* width multiplier.							*
C*									*
C* DSSPLN  ( ISLTYP, ISLSTR, ISLDIR, SLSIZ, ISLWID,			*
C*	     JSLTYP, JSLSTR, JSLDIR, SIZE,  JSLWID, IRET )		*
C*									*
C* Input parameters:							*
C*	ISLTYP		INTEGER		Special line type		*
C*	ISLSTR		INTEGER		Special line stroke multiplier	*
C*					   0 = no change		*
C*	ISLDIR		INTEGER		Special line direction indicator*
C*					   1 = up or out		*
C*					   0 = no change		*
C*					  -1 = down or in		*
C*	SLSIZ		REAL		Special line size		*
C*					   0 = no change		*
C*	ISLWID		INTEGER		Special line width multiplier	*
C*					   0 = no change		*
C*									*
C* Output parameters:							*
C*	JSLTYP		INTEGER		Special line type		*
C*	JSLSTR		INTEGER		Special line stroke multiplier	*
C*	JSLDIR		INTEGER		Special line direction indicator*
C*	SIZE		REAL		Special line size		*
C*	JSLWID		INTEGER		Special line width multiplier	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Keiser/GSC	 3/97	Copied from DSLINE			*
C* S. Jacobs/NCEP	 4/98	Cleaned up line type check		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C------------------------------------------------------------------------
	iret   = NORMAL
	jsltyp = msltyp
	jslstr = mslstr
	jsldir = msldir
	size   = tslsiz 
	jslwid = mslwid
C
C*	Set line characteristics.  Check for valid line type first.
C
	IF  ( isltyp .gt. 0 )  jsltyp = isltyp
C
C*	Check stroke.
C
	IF  ( islstr .gt. 0 )  jslstr = islstr
C
C*	Check direction indicator.
C
	IF  ( isldir .ne. 0 )  jsldir = isldir
C
C*	Check size.
C
	IF  ( slsiz .gt. 0. )  size = slsiz
C
C*	Check width.
C
	IF  ( islwid .gt. 0 )  jslwid = islwid
C
C*	Save characteristics in /DEVACT/.
C
	msltyp = jsltyp
	mslstr = jslstr
	msldir = jsldir
	tslsiz = size 
	mslwid = jslwid
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSSPLN ( msltyp, mslstr, msldir, tslsiz, mslwid, iret )
	END IF
C*
	RETURN
	END
