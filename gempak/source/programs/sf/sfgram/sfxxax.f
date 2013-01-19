	SUBROUTINE SFXXAX  ( taxis, npts, timfnd, x, xstrt, xstop, 
     +			     xtlbl, ctlbl, nxlbl, xmndst, iret )
C************************************************************************
C* SFXXAX								*
C*									*
C* This subroutine gets the parameters to use for the x axis in		*
C* SFGRAM.								*
C*									*
C* SFXXAX  ( TAXIS, NPTS, TIMFND, X, XSTRT, XSTOP, XTLBL, CTLBL, NXLBL,	*
C*           XMNDST, IRET )						*
C*									*
C* Input parameters:							*
C*	TAXIS		CHAR*		User input for T axis		*
C*	NPTS		INTEGER		Number of x points		*
C*	TIMFND (NPTS)	CHAR*		GEMPAK times			*
C*									*
C* Output parameters:							*
C*	X  (NPTS)	REAL		Values of x points		*
C*	XSTRT		REAL		Left value of x			*
C*	XSTOP		REAL		Right value of x		*
C*	XTLBL (NXLBL)	REAL		X axis label positions		*
C*	CTLBL (NXLBL)	CHAR*		X axis labels			*
C*	NXLBL		INTEGER		Number of x axis labels		*
C*	XMNDST		REAL		Minimum distance between times	*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -6 = time range is size zero	*
C*					 -16 = error in TAXIS		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 6/89						*
C* M. desJardins/GSFC	 1/90	Rewritten to include long time ranges	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	taxis, timfnd (*), ctlbl (*)
	REAL		x (*), xtlbl (*)
C*
	LOGICAL		taxrev, lmin
	CHARACTER*20	tarr (5), tstrt, tstop, temp, start,
     +			taxstr, taxend
	INTEGER		kintvl (16)
	DATA		kintvl / 1, 2, 3, 5, 10, 15, 20, 30, 60, 180,
     +			         360, 720, 1440, 2880, 5760, 10080 /
C
C*	These data values are in min; starting with 60, these represent
C*	1 hr, 3 hr, 6 hr, 12 hr, 1 d, 2 d, 4 d, 7 d .
C
C------------------------------------------------------------------------
	iret  = 0
	ntlbl = 0
C
C*	If TAXIS = 'R', the default axes are to be used but reversed.
C
	IF  ( ( taxis (1:1) .eq. 'R' ) .or. ( taxis (1:1) .eq. 'r' ) )
     +							THEN
	    taxrev = .true.
	    tarr (1) = ' '
	    tarr (2) = ' '
	    tarr (3) = ' '
	  ELSE
	    CALL ST_CLST  ( taxis, '-', ' ', 4, tarr, ntarr, ier )
	    taxrev = .false.
	END IF
C
C*	Use the start and stop times from the file as a default.  If
C*	values are specified, substitute in the given start and stop
C*	times.
C
	IF  ( tarr (1) .eq. ' ' )  THEN
	    tstrt = timfnd (1)
	  ELSE
	    CALL TI_STAN  ( tarr (1), timfnd (1), tstrt, ier )
	    IF  ( ier .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SFGRAM', -16, tarr (1), ier1 )
		tstrt = timfnd (1)
	    END IF
	END IF
C
	IF  ( tarr (2) .eq. ' ' )  THEN
	    tstop = timfnd (npts)
	  ELSE
	    CALL TI_STAN  ( tarr (2), timfnd (npts), tstop, ier )
	    IF  ( ier .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SFGRAM', -16, tarr (2), ier1 )
		tstop = timfnd (npts)
	    END IF
	END IF
C
C*	If TAXREV , interchange times.
C
	IF  ( taxrev )  THEN
	    temp   = tstrt 
	    tstrt  = tstop
	    tstop  = temp
	END IF
C
C*	Compute the difference along the x-axis in days.
C
	xstrt = 0.
	CALL TI_DIFD  ( tstop, tstrt, xstop, ier )
	IF  ( xstop .eq. 0. )  THEN
	    iret = -6
	    CALL ER_WMSG  ( 'SFGRAM', iret, ' ', ier )
	    RETURN
	END IF
C
C*	When the label increment is blank, compute a reasonable value.
C
	IF  ( tarr (3) .eq. ' ' )  THEN
C
C*	    If the time range is greater then two years, label every
C*	    year.
C
	    IF  ( xstop .gt. 720. )  THEN
C--
C
C*		If the time range is greater then 6 weeks, use labels
C*		by the month.
C
	      ELSE IF  ( xstop .gt. 40. )  THEN
C--
C
C*		Otherwise, pick a range from the list in kintvl.
C
	      ELSE
		kmin = xstop * 1440
		kmin = kmin / 4
		ipnt = 15
		DO  i = 1, 15
		    IF  ( kintvl (i) .lt. kmin )  ipnt = i
		END DO
		IF  ( ( kmin - kintvl (ipnt) ) .lt. 
     +		      ( 2 * (kintvl (ipnt + 1) - kmin) ) )  THEN
		    ndeltt = kintvl (ipnt)
		  ELSE
		    ndeltt = kintvl (ipnt + 1)
		END IF
C
C*		Convert minutes of increment to HHHMM.
C
		nhr    = ndeltt / 60
		nhhhmm = ndeltt - nhr * 60 + nhr * 100
		CALL ST_INCH  ( nhhhmm, tarr (3), ier )
	    END IF
	END IF
C
C*	Generate the labels; The first label is at 00 UTC of the first
C*	date.
C
	IF  ( tarr (3) .ne. ' ' )  THEN
	    start = tstrt ( 1:7 ) // '00' // tstrt ( 10: )
	    IF  ( tstrt .lt. tstop )  THEN
		taxstr = start
		taxend = tstop
	      ELSE
		taxstr = tstop
		taxend = start
	    END IF
	    CALL TG_RINC  ( taxstr, taxend, tarr (3), 2, nxlbl, ctlbl,
     +			    ier )
	END IF
C
C*	Generate the label positions.  Note that some will be clipped.
C
	DO  i = 1, nxlbl
	    CALL TI_DIFD  ( ctlbl (i), tstrt, xtlbl (i), ier )
	END DO
C
C*	Abbreviate the time labels.  First, detect whether date/time 
C*	minutes are ever non-zero.
C
	lmin = .false.
	DO  i = 1, nxlbl
	    IF  ( ctlbl (i) ( 10:11 ) .ne. '00' )  lmin = .true.
	END DO
C
C*	Remove date/time minutes if they are always zero.
C
	IF  ( .not. lmin )  THEN
	    DO  i = 1, nxlbl
		ctlbl (i) ( 10: ) = ' '
	    END DO
	END IF
C
C*	Remove date elements which are duplicated in all the labels.
C
	IF  ( ctlbl (1) ( 1:2 ) .eq. ctlbl (nxlbl) ( 1:2 ) )  THEN
	    IF  ( ctlbl (1) ( 3:4 ) .eq. ctlbl (nxlbl) ( 3:4 ) )  THEN
		IF  ( ctlbl (1) ( 5:6 ) .eq. ctlbl (nxlbl) ( 5:6 ) )  
     +								    THEN
		    nskp = 7
		  ELSE
		    nskp = 5
		END IF
	      ELSE
		nskp = 3
	    END IF
	  ELSE
	    nskp = 1
	END IF
C
	DO  i = 1, nxlbl
	    ctlbl (i) = ctlbl (i) ( nskp: )
	END DO
C
C*	Find the x values of points along the axis.
C
	DO  i = 1, npts
	    CALL TI_DIFD  ( timfnd (i), tstrt, x (i), ier )
	END DO
C
C*	Get minimum time difference in file.
C
	xmndst = ABS ( xstop )
	DO  i = 2, npts
	    IF  ( ABS ( x (i) - x (i-1) ) .lt. xmndst )  THEN
		xmndst = x (i) - x (i-1)
	    END IF
	END DO
	xmndst = xmndst * 3.
C*
	RETURN
	END
