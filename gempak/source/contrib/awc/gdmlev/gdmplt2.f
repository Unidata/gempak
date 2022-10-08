	SUBROUTINE GDMPLT2  ( grid, kx, ky, ix1, iy1, ix2, iy2, ixinc,
     +			     istag, iyinc, color, positn, rmin, rmax,
     +			     cint, iret )
C************************************************************************
C* GDMPLT2								*
C*									*
C* This subroutine plots grid data for GDMAP.				*
C*									*
C* GDMPLT2  ( GRID, KX, KY, IX1, IY1, IX2, IY2, IXINC, IYINC, COLOR,	*
C*	     RMIND, RMAXD, IRET )					*
C*									*
C* Input parameters:							*
C*	GRID (KX, KY)	REAL		Grid data			*
C*	KX		INTEGER		Number of points in x dir	*
C*	KY		INTEGER		Number of points in y dir	*
C*	IX1		INTEGER		First point in x dir		*
C*	IY1		INTEGER		First point in y dir		*
C*	IX2		INTEGER		Last point in x dir		*
C*	IY2		INTEGER		Last point in y dir		*
C*	IXINC		INTEGER		Increment in x dir		*
C*	ISTAG		INTEGER		Increment for stagger		*
C*	IYINC		INTEGER		Increment in y dir		*
C*	COLOR		CHAR*		Color				*
C*	POSITN	        CHAR*		Position number	   		*
C*	RMIND		REAL		Minimum valid value		*
C*	RMAXD		REAL		Maximum valid value		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/85						*
C* G. Huffman/GSC	 1/89	Dont plot colors less than 1		*
C* M. desJardins/GSFC	 2/91	Added valid range			*
C* S. Jacobs/NMC	 9/94	Added staggered plotting		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid ( kx, ky )
	CHARACTER*(*)	color, positn, cint
C*
	CHARACTER	pstr*10, clbl(LLCLEV)*24
	REAL		clvl (LLCLEV)
C*
	INTEGER		iposx (9), iposy (9) , icolor(LLCLEV)
	LOGICAL		found
	DATA		iposx  / 0, -1, -1, 3, 3, -1,  3, 0,  0 /
	DATA		iposy  / 0,  2,  0, 2, 0, -2, -2, 4, -4 /
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret  = 0
C
C*	Get range of data to plot
C
	CALL IN_INTC ( cint, rmin, rmax, clvl, nclvl, clbl, 
     +			rint, cmin, cmax, iret )
C
C*	Set the color for the plot.
C
	CALL IN_COLR  ( color, nclvl, icolor, ier )
C
C*	Get the offsets for the position number.
C
	CALL ST_ILST  ( positn, '/', 0, 1, ipos, n, ier )
	IF  ( ( ipos .lt. 0 ) .or. ( ipos .gt. 8 ) )  ipos = 0
	iyoff = iposy ( ipos + 1 )
	ixoff = iposx ( ipos + 1 )
	ix    = ixoff
C
C*	Loop through the grid.
C
	ixstrt = ix1
	DO  j = iy1, iy2, iyinc
	    fy = FLOAT (j)
C*
	    DO  i = ixstrt, ix2, ixinc
	    	fx = FLOAT (i)
		d  = grid ( i, j )
		IF  ( .not. ERMISS (d) ) THEN
		    jj = 1
		    found = .false.
		    DO WHILE ( ( .not. found ) .and.
     +			       ( jj .le. nclvl ) )
			IF ( d .lt. clvl(jj) ) THEN
			    found = .true.
			ELSE
			    jj = jj + 1
			END IF
		    END DO
C*	
		    IF ( found .and. ( icolor(jj) .ne. 0 ) ) THEN
	    		CALL GSCOLR  ( icolor(jj), ier )
			id = NINT (d)
			CALL ST_INCH  ( id, pstr, ier )
			CALL ST_LSTR  ( pstr, len, ier )
			IF  ( ( ipos .eq. 0 ) .or. ( ipos .eq. 7 ) .or.
     +			      ( ipos .eq. 8 ) )  THEN
			    ix = ixoff - len + 1
			  ELSE IF  ( (ipos.eq.1) .or. (ipos.eq.2) .or.
     +				     (ipos.eq.5) )  THEN
			    ix = ixoff - 2 * len
			END IF
                        CALL GQTEXT  ( itxfn, itxhw, sztext, itxwid,
     +                          ibrdr, irrotn, ijust, iret )
		        CALL GTEXT  ( 'G', fx, fy, pstr, 0.0, ix,
     +				      iyoff, ier )
		    END IF
		END IF
	    END DO
	    IF  ( ixstrt .eq. ix1 )  THEN
	        ixstrt = ixstrt + istag
	    ELSE
	        ixstrt = ix1
	    END IF
	END DO
C*
	RETURN
	END
