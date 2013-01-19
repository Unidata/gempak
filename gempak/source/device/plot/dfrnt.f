	SUBROUTINE DFRNT ( iwndw, np, x, y, iret )
C************************************************************************
C* DFRNT								*
C*									*
C* This subroutine draws fronts on the current graphics device.		*
C*									*
C* DFRNT ( IWNDW, NP, X, Y, IRET )					*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	NP		INTEGER		Number of points		*
C*	X (NP)		REAL		X coordinates in device units	*
C*	Y (NP)		REAL		Y coordinates in device units	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Wehner/EAi	 9/96	Based on DLINE				*
C* M. Linda/GSC		 1/97	Changed X and Y to reals, cleaned up	*
C* M. Linda/GSC		 1/97	Fixed interference with line width	*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C* S. Jacobs/NCEP	 9/97	Added call to DSFILL to set solid fill	*
C* S. Jacobs/NCEP	 2/98	Added smoothing using splines		*
C* S. Jacobs/NCEP	 3/98	Added check for UTF for filling pips	*
C* S. Jacobs/NCEP	 3/98	Changed value of solid fill in DSFILL	*
C* S. Jacobs/NCEP	 4/98	Changed calc of front type; Call DSPLN	*
C* S. Jacobs/NCEP	 6/98	Changed pip size to a REAL variable	*
C* S. Jacobs/NCEP	 2/99	Added type 999 for sqall line		*
C* M. Li/GSC		 7/00	Added HSCLR2				*
C* S. Jacobs/NCEP	 3/01	Changed 997->1700; 998->1800; 999->1900	*
C* S. Jacobs/NCEP	 3/01	Added type 1809	(front with no pips)	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GBUFFT.CMN'
C*
	REAL		x (*), y (*)
C*
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	If the driver is VG, send the points directly to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSCLR2 (mcolr, mcolr2, iret )
	    CALL HFRNT ( np, x, y, iret )
	    RETURN
	END IF
C
C*	Compute pip size, line type and line width.
C
	icod1 = mfcod / 100
	icod2 = MOD ( mfcod/10, 10 )
	icod3 = MOD ( mfcod, 10 )
C
C*	The middle digit of the code controls the width of the line.
C
	IF  ( icod2 .le. 3 )  THEN
	    islwid = 1
	  ELSE IF  ( icod2 .le. 6 )  THEN
	    islwid = 3
	  ELSE
	    islwid = 5
	END IF
C
C*	Compute the front type.
C
	IF  ( icod1 .eq. 1 )  icod1 = 0
	IF  ( icod1 .eq. 3 )  icod1 = 2
	IF  ( icod1 .eq. 5 )  icod1 = 4
C
	IF  ( icod1 .eq. 7 )  THEN
	    isltyp = 1700
	  ELSE IF  ( icod1 .eq. 8 )  THEN
	    IF  ( icod3 .ne. 9 )  THEN
	    	isltyp = 1800
	      ELSE
		isltyp = 1809
	    END IF
	  ELSE IF  ( icod1 .eq. 9 )  THEN
	    isltyp = 1900
	  ELSE
	    IF ( ( icod3 .ne. 5 ) .and. ( icod3 .ne. 8 ) ) icod3 = 0
	    isltyp = 1000 + icod1 * 100 + icod3
	END IF
C
C*	Save special line attributes.
C
	jsltyp = msltyp
	jslstr = mslstr
	jsldir = msldir
	size   = tslsiz
	jslwid = mslwid
C
C*	Set the attributes to draw the front.
C
	CALL DSSPLN ( isltyp, 0, mpipdr, tpipsz, islwid,
     +		      idum1, idum2, idum3, dummy, idum4, ier )
C
C*	Draw the front.
C
	CALL DSPLN ( iwndw, np, x, y, ier )
C
C*	Restore special line attributes.
C
	CALL DSSPLN ( jsltyp, jslstr, jsldir, size, jslwid,
     +		      idum1, idum2, idum3, dummy, idum4, ier )
C*
	RETURN
	END
