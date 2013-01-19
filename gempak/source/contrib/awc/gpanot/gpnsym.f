	SUBROUTINE GPNSYM ( ictyp, line, loci, info, isymb, iret )
C************************************************************************
C* GPNSYM								*
C*									*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	12/91						*
C* C. Bailey/HPC	10/06		Changed call to IN_LINE		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	line, loci, info
C*
	CHARACTER	sys*1
	REAL		tmpt(2), sympt(3), xxl (4), yyl (4)
	REAL		smooth, linfltr
	LOGICAL		scflag
C------------------------------------------------------------------------
C*	Get plot bounds in Normal coordinates.
C
        CALL GQBND ( 'N', xleft, ybot, xrght, ytop, ier )
C
C*	Parse the line input.
C
	CALL IN_LINE ( line, 1, 1, icolor, itype, iwidth, ilabel, 
     +                 smooth, linfltr, scflag, ier )
C
	IF  ( icolor .ne. 0 )  THEN
C
C*	    Set coordinate system to be used.
C
	    sys = 'N'
	    IF  ( loci(1:1) .eq. '@' )  THEN
		sys = 'G'
		loci = loci(2:)
	    ELSE IF  ( loci(1:1) .eq. '#' )  THEN
		sys = 'M'
		loci = loci(2:)
	    END IF
C
C*	    Get the point(s) for the plot.
C
	    CALL ST_RLST ( loci, ';', 0., 2, tmpt, num2, ier )
C
C*	    Separate the shape information.
C
	    CALL ST_RLST ( info, '/', 1., 2, sympt, num, ier )
C
C*	    Get the symbol code number.
C
	    isym = NINT (sympt(1))
C
C*	    Get the symbol size.
C
	    siz = sympt(2)
C
C*	    If using Normal coords, correct for PS device.
C
	    IF  ( sys .eq. 'N' )  THEN
		tmpt(1) = (xrght-xleft) * tmpt(1)
		tmpt(2) = (ytop -ybot ) * tmpt(2)
	    ENDIF
C
	    xx = tmpt(1)
	    yy = tmpt(2)
C
	    CALL GSCOLR ( 2, ier )
	    CALL GQSYSZ ( wmk, zmk, d1, d2, blx, bly, ier )
	    IF ( sys .ne. 'N' ) THEN
		CALL GTRANS ( sys, 'N', 1, xx, yy, xx1, yy1, ier )
	    ELSE
		xx1 = xx
		yy1 = yy
	    END IF
C
            xxl (1) = xx1 - ( .5 * ( 1. * d1 ) + ( .2 * d1 ) )
            yyl (1) = yy1 - ( .5 * ( d2 ) + ( .2 * d2 ) )
            xxl (2) = xx1 - ( .5 * ( 1.* d1 ) + ( .2 * d1 ) )
            yyl (2) = yy1 + ( .5 * ( d2 ) + ( .2 * d2 ) )
            xxl (3) = xx1 + ( .5 * ( 1. * d1 ) + ( .2 * d1 ) )
            yyl (3) = yy1 + ( .5 * ( d2 ) + ( .2 * d2 ) )
            xxl (4) = xx1 + ( .5 * ( 1. * d1 ) + ( .2 * d1 ) )
            yyl (4) = yy1 - ( .5 * ( d2 ) + ( .2 * d2 ) )
C            CALL GFILL ( 'N', 4, xxl, yyl, ier )
            CALL GSCOLR ( isvclr, ier )
C
C*	    Set attributes.
C
	    CALL GSCOLR ( icolor, ier )
	    CALL GSLINE ( 1, 0, iwidth, 0, ier )
C
	    IF  ( isymb .eq. 1 )  THEN
		CALL GSWTHR ( siz, iwidth, ier )
		CALL GWTHR ( sys, 1, sympt(1), xx, yy, 0, 0, ier )
C
	    ELSE IF  ( isymb .eq. 2 )  THEN
		CALL GSCTYP ( siz, iwidth, ier )
		CALL GCTYP ( sys, 1, sympt(1), xx, yy, 0, 0, ier )
C
	    ELSE IF  ( isymb .eq. 3 )  THEN
		CALL GSSKY ( siz, 1, iwidth, ier )
		CALL GSKY ( sys, 1, sympt(1), xx, yy, 0, 0, ier )
C
	    ELSE IF  ( isymb .eq. 4 )  THEN
		CALL GSMRKR ( isym, 1, siz, iwidth, ier )
		CALL GMARK ( sys, 1, xx, yy, ier )
C
	    ELSE IF  ( isymb .eq. 5 )  THEN
		CALL GSPTND ( siz, iwidth, ier )
		CALL GPTND ( sys, 1, sympt(1), xx, yy, 0, 0, ier )
C
	    ELSE IF  ( isymb .eq. 6 )  THEN
		CALL GSPWTH ( siz, iwidth, ier )
		CALL GPWTH ( sys, 1, sympt(1), xx, yy, 0, 0, ier )
C
	    ELSE IF  ( isymb .eq. 7 )  THEN
		CALL GSTURB ( siz, iwidth, ier )
		CALL GTURB ( sys, 1, sympt(1), xx, yy, 0, 0, ier )
C
	    ELSE IF  ( isymb .eq. 8 )  THEN
		CALL GSICNG ( siz, iwidth, ier )
		CALL GICNG ( sys, 1, sympt(1), xx, yy, 0, 0, ier )
C
	    ELSE IF  ( isymb .eq. 9 )  THEN
		CALL GSSPCL ( siz, iwidth, ier )
		CALL GSPCL ( sys, 1, sympt(1), xx, yy, 0, 0, ier )
C
	    ELSE
		CALL ER_WMSG ( 'GPANOT', -4, ' ', ier )
C
	    END IF
C
	END IF
C*
	iret = 0
C*
	RETURN
	END
