	SUBROUTINE GDNBRB ( ictyp, line, loci, info, iret )
C************************************************************************
C* GDNBRB								*
C*									*
C*									*
C**									*
C* Log:									*
C* J. Whistler/NSSFC	 2/95						*
C* T. Lee/SAIC		01/06	Copied from GPNBRB			*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	line, loci, info
C*
	CHARACTER	sys*1, brbpts(6)*24
	REAL		tmpt(2)
	REAL		smooth, linfltr
	LOGICAL		scflag
C------------------------------------------------------------------------
C*	Get plot bounds in Normal coordinates.
C
	CALL GQBND ( 'N', xleft, ybot, xrght, ytop, ier )
C
C*	Parse the fill input.
C
	CALL IN_LINE ( line, 1, 1, icolor, itype, iwidth, ilabel, 
     +		       smooth, linfltr, scflag, ier )
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
	    CALL ST_CLST ( info, '/', ' ', 4, brbpts, num, ier )
C
C*	    Get the wind barb direction
C
	    CALL ST_CRNM ( brbpts(1), drct, ier )
	    IF  ( drct .eq. RMISSD )  drct = 270
C
C*	    Get the wind barb speed
C
	    CALL ST_CRNM ( brbpts(2), sped, ier )
	    IF  ( sped .eq. RMISSD )  sped = 75.
C
C*	    Get the wind barb size
C
	    CALL ST_CRNM ( brbpts(3), bsiz, ier )
	    IF  ( bsiz .eq. RMISSD )  bsiz = 1.
C
C*	    Get the wind barb type
C
	    CALL ST_CRNM ( brbpts(4), rbtyp, ier )
	    ibtyp = NINT ( rbtyp )	
	    IF  ( ibtyp .eq. RMISSD )  ibtyp = 1
C
C*	    If using Normal coords, correct for PS device.
C
	    IF  ( sys .eq. 'N' )  THEN
		tmpt(1) = (xrght-xleft) * tmpt(1)
		tmpt(2) = (ytop -ybot ) * tmpt(2)
	    ENDIF
C
C*	    Set attributes.
C
	    CALL GSLINE ( itype, 0, iwidth, 0, ier )
	    CALL GSBARB ( bsiz, iwidth, ibtyp, ier )
	    CALL GSCOLR ( icolor, ier )
C
C*	    Draw the wind barb.
C
	    CALL GBARB  ( sys, 1, tmpt(1), tmpt(2), sped, drct, iret )
C*
	END IF
C*
	RETURN
	END
