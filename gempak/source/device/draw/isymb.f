	SUBROUTINE ISYMB ( ix, iy, ixoff, iyoff, size, iwidth, ishape,
     +			   iret )
C************************************************************************
C* ISYMB								*
C*									*
C* This subroutine plots symbols, such as weather symbols and others.	*
C*									*
C* ISYMB ( IX, IY, IXOFF, IYOFF, SIZE, IWIDTH, ISHAPE, IRET )		*
C*									*
C* Input parameters:							*
C*	IX		INTEGER		X coordinate in device units	*
C*	IY		INTEGER		Y coordinate in device units	*
C*	IXOFF		INTEGER		X offset in half characters	*
C*	IYOFF		INTEGER		Y offset in half characters	*
C*	SIZE		REAL		Symbol size			*
C*	IWIDTH		INTEGER		Symbol line width		*
C*	ISHAPE ( 0:45 )	INTEGER		Symbol shape definition		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 8/96	Based on IWTHR				*
C* M. Linda/GSC		12/96	Added inbnds test, removed color fill	*
C* S. Jacobs/NCEP	 9/97	Added call to DSFILL to set solid fill	*
C* S. Jacobs/NCEP	 3/98	Changed value of solid fill in DSFILL	*
C* S. Jacobs/NCEP	 7/98	Changed ttxsz to txsize			*
C* G. Krueger/EAI	 1/99	Added space for FLAME symbol		*
C* D.W.Plummer/NCEP      2/99   Added blank background capability       *
C************************************************************************
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'ERROR.PRM'
C
	INTEGER		ishape ( 0:45 )
C*
	INTEGER		ixp (45), iyp (45)
	LOGICAL		inbnds
C
C*	Statement function to check a point to be within clipping bounds.
C
	inbnds ( jx, jy ) = ( ( ispanx * ( jx  - icleft ) .ge. 0 ) .and.
     +			      ( ispanx * ( icrght -  jx ) .ge. 0 ) .and.
     +			      ( ispany * ( jy  -  icbot ) .ge. 0 ) .and.
     +			      ( ispany * ( ictop  -  jy ) .ge. 0 ))
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Compute offsets based on text character spacing.
C
	IF  ( MOD ( ixoff, 2 ) .ne. 0 ) THEN
	    ixo = ( ixoff - 1 ) / 2 * 7 + 4
	  ELSE
	    ixo = ixoff / 2 * 7
	END IF
C
	IF  ( MOD ( iyoff, 2 ) .ne. 0 ) THEN
	    iyo = ( iyoff - 1 ) / 2 * 9 + 5
	  ELSE
	    iyo = iyoff / 2 * 9
	END IF
C
C*	Calculate the symbol center location by adding the offsets.
C
	ixc = ix + ispanx * NINT ( ixo * bscalc * txsize )
        iyc = iy + ispany * NINT ( iyo * bscalc * txsize )
C
C*	Check center against clipping area; also checks if point missing.
C
	IF ( .not. inbnds ( ixc, iyc ) ) RETURN
C
C*	Pre-compute the dot multiplication factor.
C
	dotmul = bscald / bscalw * size
C
C*	Get the number of points that define the symbol.
C
	npnts = ishape ( 0 )
C
C*	Save iwidth and color.
C
	iwidsv = iwidth
	mqcolr = mcolr
C
C*	Split up iwidth into foreground and background components.
C
	IF  ( iwidth .gt. 99 )  THEN
            kfgwid = MOD ( iwidth, 100 )
            IF  ( kfgwid .le. 0 )  kfgwid = 1
            kbgwid = ( iwidth / 100 ) + kfgwid
	    nx = 2
          ELSE
            kfgwid = iwidth
            kbgwid = 0
	    nx = 1
        END IF
C	
C*	Loop over background and foreground.
C
	DO  ibf = 1, nx
C
C*	  Depending on background or foreground, set line width,
C*	  color and dot plotting expansion.
C
	  iwidth = kfgwid
	  IF ( nx .eq. 2 .and. ibf .eq. 1 )  THEN
	    iwidth = kbgwid
	    CALL DSCOLR ( 101, imclr, ier )
	    dotexp = kbgwid - kfgwid
	  ELSE
	    CALL DSCOLR ( mqcolr, imclr, ier )
	    dotexp = 0.0
	  END IF
C
	  CALL DSLINE ( 0, 0, iwidth, 0, i1, i2, i3, i4, ier)
C
C*	  Loop over the points.
C
	  np = 0
	  jwidth = 0
	  DO  ip = 1, npnts
C
C*	    Get the next item from the symbol shape table.
C
	    isitem = ishape ( ip )
	    idrwpt = 1
C
C*          If pen up or fill, draw all points up to last point.
C
	    IF  ( ( isitem .le. 0 ) .and. ( np .gt. 0 ) ) THEN
C
		IF  ( isitem .lt. 0 ) THEN
		    CALL ILINE  ( np, ixp, iyp, ier )
		    IF  ( jwidth .ne. 0 )
     +			CALL DSLINE ( 0, 0, iwidth, 0, i1,i2,i3,i4, ier )
		  ELSE
		    msvft = mfltyp
		    svfsz = tfilsz
		    IF ( nx .eq. 2 .and. ibf .eq. 1 )  THEN
			CALL ILINE ( np, ixp, iyp, ier )
		    ELSE
		        CALL DSFILL ( 1.0, 1, fsize, itype, ier )
		        CALL IFILL ( np, ixp, iyp, ier )
		        CALL DSFILL ( svfsz, msvft, fsize, itype, ier )
		        idrwpt = 0
		    END IF
		END IF
C
	        np = 0
	    ENDIF
C
	    IF  ( idrwpt .ne. 0 ) THEN
		isitem = ABS ( isitem )
C
C*		If point is a filled dot, set line width to dot width.
C
		idotwd = isitem / 10000
		IF  ( idotwd .ne. 0 ) THEN
		    jwidth = iwidth
		    idotwd = NINT ( ( idotwd * dotmul ) + dotexp )
		    CALL DSLINE ( 0, 0, idotwd, 0, i1, i2, i3, i4, ier )
		    isitem = ABS ( MOD ( ishape ( ip ), 10000 ) )
		  ELSE
		    jwidth = 0
		END IF
C
C*		Translate point so it is relative to symbol center.
C
		ixm = ABS (       isitem / 100 )   - 6
		iym = ABS ( MOD ( isitem,  100 ) ) - 6
C
C*		Multiply point location by symbol size.
C
		ixd = ispanx * NINT ( ixm * size )
		iyd = ispany * NINT ( iym * size )
C
C*		Add point location to the symbol center location.
C
		np = np + 1
		ixp ( np ) = ixc + ixd
		iyp ( np ) = iyc + iyd
	    END IF
	  END DO
C
C*	  Draw the last set of points.
C
	  IF  ( np .ne. 0 ) CALL ILINE ( np, ixp, iyp, ier )
C
C*	  Reset line width if needed.
C
	  IF  ( jwidth .ne. 0 )
     +	    CALL DSLINE ( 0, 0, iwidth, 0, i1, i2, i3, i4, ier)
C
	END DO
C
	iwidth = iwidsv
C*
	RETURN
	END
