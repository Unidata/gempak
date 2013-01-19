	SUBROUTINE DSBARB  ( szbarb, ibrwid, ibrtyp, size, jbrwid, 
     +                       jbrtyp, iret )
C************************************************************************
C* DSBARB								*
C* 									*
C* This subroutine sets the wind barb size and width multipliers and	*
C* the wind barb type.  If these parameters are not positive, no	* 
C* changes are made.							*
C* 									*
C* DSBARB  ( SZBARB, IBRWID, IBRTYP, SIZE, JBRWID, JBRTYP, IRET )	*
C*                                                                    	*
C* Input parameters:							*
C* 	SZBARB		REAL		Barb size multiplier		*
C* 					  <=0 = no change		*
C*	IBRWID		INTEGER		Barb width multiplier		*
C*					  <=0 = no change		*
C*	IBRTYP		INTEGER		Barb type			*
C*					  <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Barb size			*
C*	JBRWID		INTEGER		Barb width			*
C*	JBRTYP		INTEGER		Barb type			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/89	Make size = 0 no change			*
C* S. Schotz/GSC	 1/90	Added barb width and type		*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C-----------------------------------------------------------------------
	iret = NORMAL
C*
	IF  ( szbarb .gt. 0. )  twbsz = szbarb
	IF  ( ibrwid .gt. 0  )  mbrwid = ibrwid
	IF  ( ibrtyp .gt. 0  )  mbrtyp = ibrtyp
	size = twbsz
        jbrwid = mbrwid
        jbrtyp = mbrtyp
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSWIND ( 1, size, jbrwid, jbrtyp, 0, iret )
	END IF
C*
	RETURN
	END
