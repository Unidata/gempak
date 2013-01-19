	SUBROUTINE GDBHLO (  hilo, hlsym, grid, kx, ky, iret )
C************************************************************************
C* GDBHLO								*
C*									*
C* This subroutine finds and marks relative extrema for the grid.	*
C*									*
C* GDBHLO  ( HILO, HLSYM, GRID, KX, KY, IRET )				*
C*									*
C* Input parameters:							*
C*	HILO		CHAR*		HILO input string 		*
C*	HLSYM		CHAR*		Text specifier			*
C*	GRID (KX,KY)	REAL		Scaled grid data		*
C*	KX		INTEGER		Grid x dimension		*
C*	KY		INTEGER		Grid y dimension		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		 6/93						*
C* D. Keiser/GSC	 7/95	Changed DC to DG in call to find extrema*
C* D. Keiser/GSC	10/95	Changed ierr to acct for pos numbers	*
C* S. Jacobs/NCEP	 5/96	Fixed plotting location of the values	*
C* S. Jacobs/NCEP	 9/97	Changed call to GQTEXT and GSTEXT	*
C* D.W.Plummer/NCEP	 6/01	Added special symbols processing	*
C* T. Lee/SAIC		10/01	Grouped symbols and labels		*
C* T. Lee/SAIC		10/02	Reset symbol size			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	PARAMETER	( JHGRP = 5, JLGRP = 6 )
C*
	CHARACTER*(*)	hilo, hlsym
	REAL		grid (*)
C*
	LOGICAL		intflg, valflg (2)
	CHARACTER*32	csymbl (2), text, symtxt, valtxt
	INTEGER		isymbl (2), isymtp (2), iprecn (2), knt (2),
     +			icolr (2)
	REAL		range (4)
	REAL		xmax (128), ymax (128), vmax (128)
	REAL		xmin (128), ymin (128), vmin (128)
	INTEGER		isymwd
C------------------------------------------------------------------------
	iret  = 0
	CALL IN_HILO ( hilo, icolr, csymbl, isymbl, isymtp,
     +		       valflg, iprecn, range, krad, knt, intflg,
     +		       iret )
	IF ( ( icolr (1) + icolr (2) ) .eq. 0 ) RETURN
	IF ( knt (1) .gt. 128) knt (1) = 128
	IF ( knt (2) .gt. 128) knt (2) = 128
C
C*	Find the extrema.
C
	CALL DG_HILO ( grid, kx, ky, krad, intflg, knt (1),
     +		       knt (2), range (1), range (2), range (3),
     +		       range (4), nummax, xmax, ymax, vmax,
     +		       nummin, xmin, ymin, vmin, ierr )
C
	IF ( ierr .ge. 0 ) THEN
C
C*          Write out error message regarding internal buffers
C*	    in DG_HILO.
C
            IF ( ierr .ne. 0 ) CALL ER_WMSG ( 'DG', ierr, ' ', ier2 )
C
C*	    Save the TEXT characteristics.
C
	    CALL GQTEXT ( ifntsv, ihwsv, sizsv, iwidsv,
     +			  ibdrsv, irrtsv, ijstsv, ier )
C
C*	    Set the TEXT for the HILO strings.
C
            IF ( hlsym .eq. ' ' ) THEN
                text = '1.5;1'
            ELSE
                text = hlsym
            END IF
 	    CALL IN_HLSY ( text, symtxt, valtxt, labloc, ier )
C
C*	    Set the marker size to same size as text..
C
	    sizmrk = 0
	    IF ( isymtp (1) .ge. 2 .or. isymtp (2) .ge. 2 ) THEN
		CALL ST_RLST ( symtxt, '/', 0., 1, sizmrk, n, ier )
	    END IF
	    CALL IN_TEXT ( symtxt, ier )
	    CALL GQTEXT ( itxfn, itxhw, siztxt, itxwid, ibrdr, irrotn,
     +			  ijust, ier )
	    isymwd = itxwid
C
C*	    Label the highs.
C
	    siztxt = 0
	    IF ( icolr (1) .gt. 0 .and. nummax .gt. 0 ) THEN
		CALL GSCOLR ( icolr (1), ier )
		IF ( isymtp (1) .ge. 2 ) THEN
		    IF ( isymtp (1) .eq. 3 )  THEN
			CALL GSSPCL ( sizmrk, isymwd, ier )
			code    = isymbl (1)
			iixoff  = 0
			iiyoff  = 0
		    ELSE
		      CALL GSMRKR ( isymbl (1), 0, sizmrk, isymwd, ier )
		    END IF
		END IF
		DO i = 1, nummax
		    CALL GSGRP ( JHGRP, ier )
C
		    IF  ( isymtp (1) .ge. 2 )  THEN
			IF  ( isymtp (1) .eq. 3 )  THEN
			    CALL GSPCL ( 'G', 1, code, 
     +					 xmax (i), ymax (i), 
     +					 iixoff, iiyoff, ier )
			  ELSE
			    CALL GMARK ( 'G', 1, xmax(i), ymax(i), ier )
			END IF
		    END IF
C
		    IF ( isymtp (1) .eq. 1 ) THEN
			CALL ST_LSTR ( csymbl (1), lens, ier )
			ixoff = -  ( lens - 1 )
			CALL IN_TEXT ( symtxt, ier )
			CALL GQTEXT ( itxfn, itxhw, siztxt,
     +				      itxwid, ibrdr, irrotn,
     +				      ijust, ier )
			CALL GTEXT ( 'G', xmax (i), ymax (i),
     +				         csymbl (1), 0., ixoff, 0,
     +					 ier )
		    END IF
		    size1 = MAX ( siztxt, sizmrk )
		    IF ( valflg (1) ) THEN
			IF ( iprecn (1) .gt. 0 ) THEN
			    CALL ST_RLCH ( vmax (i), iprecn (1),
     +					   text, ier )
			ELSE
			    ivm = NINT ( vmax (i) )
			    CALL ST_INCH ( ivm, text, ier )
			END IF
			CALL ST_LSTR ( text, lens, ier )
			ixoff = - ( lens - 1 )
			IF ( labloc .eq. 1 ) ixoff = (-2) * lens
			IF ( labloc .eq. 3 ) ixoff = 1
			CALL IN_TEXT ( valtxt, ier )
			CALL GQTEXT ( itxfn, itxhw, size2, itxwid,
     +				      ibrdr, irrotn, ijust, ier )
			iyoff = NINT ( size1 / size2 + 1 )
			CALL GTEXT ( 'G', xmax (i), ymax (i), text,
     +				 	0., ixoff, -iyoff, ier )
		    END IF 
		    CALL GEGRP ( ier )
		END DO
	    END IF
C
C*	    Label the lows.
C
	    siztxt = 0
	    IF ( icolr (2) .gt. 0 .and. nummin .gt. 0 ) THEN 
		CALL GSCOLR ( icolr (2), ier  )
		IF ( isymtp (2) .ge. 2 ) THEN
		    IF ( isymtp (2) .eq. 3 )  THEN
			CALL GSSPCL ( sizmrk, isymwd, ier )
			code   = isymbl (2)
			iixoff = 0
			iiyoff = 0
		    ELSE
		      CALL GSMRKR ( isymbl (2), 0, sizmrk, isymwd, ier )
		    END IF
		END IF
		DO i = 1, nummin
		    CALL GSGRP ( JLGRP, ier )
		    IF  ( isymtp (2) .ge. 2 )  THEN
			IF  ( isymtp (2) .eq. 3 )  THEN
			    CALL GSPCL ( 'G', 1, code,
     +					 xmin (i), ymin (i),
     +					 iixoff, iiyoff, ier )
			  ELSE
			    CALL GMARK ( 'G', 1, xmin(i), ymin(i), ier )
			END IF
		    END IF
C
		    IF ( isymtp (2) .eq. 1 ) THEN
			CALL ST_LSTR ( csymbl (2), lens, ier )
			ixoff = -  ( lens - 1 )
			CALL IN_TEXT ( symtxt, ier )
			CALL GQTEXT ( itxfn, itxhw, siztxt,
     +				      itxwid, ibrdr, irrotn,
     +				      ijust, ier )
			CALL GTEXT ( 'G', xmin (i), ymin (i),
     +				         csymbl (2), 0., ixoff, 0,
     +					 ier )
		    END IF
		    size1 = MAX ( siztxt, sizmrk )
		    IF ( valflg (2) ) THEN
			IF ( iprecn (2) .gt. 0 ) THEN
			    CALL ST_RLCH ( vmin (i), iprecn (2),
     +					   text, ier )
			ELSE
			    ivm = NINT ( vmin (i) )
			    CALL ST_INCH ( ivm, text, ier )
			END IF
			CALL ST_LSTR ( text, lens, ier )
			ixoff = - ( lens - 1 )
			IF ( labloc .eq. 1 ) ixoff = (-2) * lens
			IF ( labloc .eq. 3 ) ixoff = 1
			CALL IN_TEXT ( valtxt, ier )
			CALL GQTEXT ( itxfn, itxhw, size2, itxwid,
     +				      ibrdr, irrotn, ijust, ier )
			iyoff = NINT ( size1 / size2 + 1 )
			CALL GTEXT ( 'G', xmin (i), ymin (i), text,
     +				 	0., ixoff, -iyoff, ier )
		    END IF 
		    CALL GEGRP ( ier )
		END DO
	    END IF
C
C*	    Reset text characteristics.
C
	    CALL GSTEXT ( ifntsv, ihwsv, sizsv, iwidsv,
     +			  ibdrsv, irrtsv, ijstsv, ier )
	END IF
C*
	RETURN
	END
