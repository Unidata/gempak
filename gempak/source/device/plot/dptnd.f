	SUBROUTINE DPTND ( iwndw, np, ptcod, x, y, ixoff, iyoff, iret )
C************************************************************************
C* DPTND								*
C*									*
C* This subroutine draws pressure tendency symbols on the current	*
C* graphics device.							*
C*									*
C* DPTND ( IWNDW, NP, PTCOD, X, Y, IXOFF, IYOFF, IRET )			*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	NP		INTEGER		Number of symbols		*
C*	PTCOD (NP)	REAL		Pressure tendency symbol codes	*
C*	X     (NP)	REAL		X coordinates in device units	*
C*	Y     (NP)	REAL		Y coordinates in device units	*
C*	IXOFF (NP)	INTEGER		X offsets in half characters	*
C*	IYOFF (NP)	INTEGER		Y offsets in half characters	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	GEMPLT Version 5.0				*
C* K. Brill/NMC		11/91	Implemented and cleaned up; added size	*
C*				parameter defined in GEMPRM.PRM		*
C* K. Brill/NMC		11/91	Plot numeric change before symbol	*
C* K. Brill/NMC		02/92	Use n999 for plotting symbol n		*
C* S. Jacobs/EAI	 1/93	Changed check for sign of change	*
C*				from ".gt. 5" to ".ge. 5"		*
C* M. Linda/GSC		 6/96	Changed so text size works for HW text	*
C* M. Linda/GSC		 8/96	Changed so line type does not influence	*
C* M. Linda/GSC		 9/96	Changed offsets logic for consistency	*
C* M. Linda/GSC		12/96	Changed X, Y, and symbol code to reals	*
C* M. Linda/GSC		 3/97	Changed DTEXT call passing in real X, Y	*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C* S. Jacobs/NCEP	 8/97	Added check for UTF driver		*
C* S. Jacobs/NCEP	10/97	Fixed check for UTF driver		*
C* S. Jacobs/NCEP	11/97	Changed calling sequence for HSTEXT	*
C* S. Jacobs/NCEP	 7/98	Changed ttxsz to txsize			*
C* S. Jacobs/NCEP	 7/98	Changed call to HSTEXT			*
C* A. Hardy/GSC          9/98   Added RBK device driver                 *
C* D.W.Plummer/NCEP	 2/99	Added blank background capability	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
	REAL		ptcod (*), x (*), y (*)
	INTEGER		ixoff (*), iyoff (*)
	CHARACTER	csgn*1, cint*4, cfinl*4
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	If the driver is VG, send the points directly to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSYMB ( 4, np, ptcod, x, y, ixoff, iyoff, iret )
	    RETURN
	END IF
C
C*	Save line type and line width.
C
	IF  ( ( mltyp .ne. 1 ) .or. ( mlwid .ne. mptwid ) ) THEN
	    jltyp = mltyp
	    jlwid = mlwid
	  ELSE
	    jltyp = 0
	    jlwid = 0
	END IF
C
	IF ( mptwid .gt. 99 )  THEN
            kfgwid = MOD ( mptwid, 100 )
            IF  ( kfgwid .le. 0 )  kfgwid = 1
            kbgwid = ( mptwid / 100 ) + kfgwid
	ELSE
            kfgwid = mptwid
            kbgwid = 0
	END IF
C
C*	Set clipping window.
C
	CALL DSCLIP ( iwndw, ier )
C
C*	Compute symbol size.
C
	wsize = tptnsz * bscalc * RSZPTN
C
C*	If the device is UTF or RBK, pass the symbol codes and locations.
C
	IF  ( ( ddev .eq. 'UTF' ) .or. ( ddev .eq. 'RBK' ) ) THEN
	    CALL HSYMB ( 4, np, ptcod, x, y, ixoff, iyoff, iret )
	  ELSE
C
C*	    Loop through symbols if size is greater than 0.
C
	    IF  ( wsize .gt. 0. ) THEN
	       DO i = 1, np
C
C*	          Decode the input into numeric change and symbol code.
C
		  iptnm = MOD ( NINT ( ptcod (i) ), 1000 )
		  iptsy =       NINT ( ptcod (i) ) / 1000
C
		  IF ( iptnm .ne. 999 ) THEN
		    IF ( iptsy .ge. 5 ) THEN
			csgn = '-'
		      ELSE
			csgn = '+'
		    END IF
		    IF ( iptnm .eq. 0 ) THEN
			cfinl = '0'
			lentx = 1
		      ELSE
			CALL ST_INLN ( iptnm, cint, lentx, ier )
			cfinl = csgn // cint (1:lentx)
			lentx = lentx + 1
		    END IF
C
C*		    Derive the ratio of original text size to
C*		    symbol size.
C
		    tsratio = txsize / tptnsz
C
C*		    Save the text size and reset it to match symbols.
C
		    txszsv = txsize
		    txsize = tptnsz
		    mbrdsv = mbrdr
		    IF ( kbgwid .ne. 0 )  THEN
			mbrdrx = 121
		    ELSE
			mbrdrx = 111
		    END IF
      		    CALL DSTEXT ( mtxfn, mtxhw, txsize, kfgwid,
     +				  mbrdrx, mrrotn, mjust, 
     +				  i1, i2, r1, i3, i4, i5, i6,
     +				  ier )
C
C*		    Calculate text offset in terms of original text size.
C
		    ixof = NINT ( tsratio * ixoff (i) )
		    iyof = NINT ( tsratio * iyoff (i) )
C
C*		    Plot the text.
C
		    CALL DTEXT (iwndw, x (i), y (i), cfinl, lentx, 0.,
     +			        ixof, iyof, ier )
C
C*		    Restore text size.
C
		    txsize = txszsv
      		    CALL DSTEXT ( mtxfn, mtxhw, txsize, mtxwid,
     +				  mbrdsv, mrrotn, mjust,
     +				  i1, i2, r1, i3, i4, i5, i6,
     +				  ier )
C
C*		    Plot the symbol after the text.
C
		    ixof = NINT ( ( ixof + ispanx * (2 * lentx + 1) ) /
     +				  tsratio )
		   ELSE
		    ixof = ixoff (i)
		  END IF
C
		  ix = NINT ( x (i) )
		  iy = NINT ( y (i) )
C
		  mcolsv = mcolr
		  IF ( kbgwid .ne. 0 )  THEN
	            CALL DSLINE ( 1, 0, kbgwid, 0, i1, i2, i3, i4, ier )
	            CALL DSCOLR ( 101, i1, ier )
		    CALL IPTND ( iptsy, ix, iy, ixof, iyoff (i),
     +			       wsize, ier )
		  END IF
	          CALL DSLINE ( 1, 0, kfgwid, 0, i1, i2, i3, i4, ier )
	          CALL DSCOLR ( mcolsv, i1, ier )
		  CALL IPTND ( iptsy, ix, iy, ixof, iyoff (i),
     +			       wsize, ier )
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
