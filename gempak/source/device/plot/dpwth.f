	SUBROUTINE DPWTH ( iwndw, np, pwcod, x, y, ixoff, iyoff, iret )
C************************************************************************
C* DPWTH								*
C*									*
C* This subroutine draws past weather symbols on the current graphics	*
C* device.								*
C*									*
C* DPWTH ( IWNDW, NP, PWCOD, X, Y, IXOFF, IYOFF, IRET )			*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	NP		INTEGER		Number of symbols		*
C*	PWCOD (NP)	REAL		Past weather symbol codes	*
C*	X     (NP)	REAL		X coordinates in device units	*
C*	Y     (NP)	REAL		Y coordinates in device units	*
C*	IXOFF (NP)	INTEGER		X offsets in half characters	*
C*	IYOFF (NP)	INTEGER		Y offsets in half characters	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC		GEMPLT Version 5.0			*
C* K. Brill/NMC			Cleaned up and implemented		*
C* M. Linda/GSC		 8/96	Changed so line type does not influence	*
C* M. Linda/GSC		12/96	Changed X, Y, and symbol code to reals	*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C* S. Jacobs/NCEP	 8/97	Added check for UTF driver		*
C* S. Jacobs/NCEP	10/97	Fixed check for UTF driver		*
C* A. Hardy/GSC          8/98   Changed bscalc to bscalw                *
C* A. Hardy/GSC          9/98   Added check for RBK driver              *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
	REAL		pwcod (*), x (*), y (*)
	INTEGER		ixoff (*), iyoff (*)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	If the driver is VG, send the points directly to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSYMB ( 5, np, pwcod, x, y, ixoff, iyoff, iret )
	    RETURN
	END IF
C
C*	Save line type and line width.
C
	IF  ( ( mltyp .ne. 1 ) .or. ( mlwid .ne. mpwwid ) ) THEN
	    jltyp = mltyp
	    jlwid = mlwid
	    CALL DSLINE ( 1, 0, mpwwid, 0, i1, i2, i3, i4, ier )
	  ELSE
	    jltyp = 0
	    jlwid = 0
	END IF
C
C*	Set clipping window.
C
	CALL DSCLIP ( iwndw, ier )
C
C*	Compute symbol size.
C
	wsize = tpwtsz * bscalw
C
C*	If the device is UTF or RBK, pass the symbol codes and locations.
C
	IF  ( ( ddev .eq. 'UTF' ) .or. ( ddev .eq. 'RBK' ) ) THEN
	    CALL HSYMB ( 5, np, pwcod, x, y, ixoff, iyoff, iret )
	  ELSE
C
C*	    Loop through symbols if size is greater than 0.
C
	    IF  ( wsize .gt. 0. ) THEN
		DO  i = 1, np
		    icode = NINT ( pwcod (i) )
		    ix    = NINT (     x (i) )
		    iy    = NINT (     y (i) )
		    CALL IPWTH ( icode, ix, iy, ixoff(i), iyoff(i),
     +				 wsize, ier )
		END DO
	    END IF
	END IF
C
C*	Restore line type and line width.
C
	IF  ( ( jltyp .ne. 0 ) .or. ( jlwid .ne. 0 ) )
     +	    CALL DSLINE ( jltyp, 0, jlwid, 0, i1, i2, i3, i4, ier )
C*
	RETURN
	END
