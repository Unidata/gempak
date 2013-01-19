	SUBROUTINE IDRAW ( np, ixd, iyd )
C************************************************************************
C* IDRAW								*
C*									*
C* This subroutine draws lines using the desired software line width.	*
C*									*
C* IDRAW ( NP, IXD, IYD )						*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	IXD (NP)	INTEGER		X coordinates in device units	*
C*	IYD (NP)	INTEGER		Y coordinates in device units	*
C*									*
C** Log:								*
C* M. desJardins/GSFC	 8/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	10/87	Eliminate plotting duplicate points	*
C* S. Schotz/GSFC	 3/90	Added simple algorithm for dot fill in	*
C* M. Linda/GSC		 6/96	Replaced local buffers with GBUFFT	*
C* S. Jacobs/NCEP	 9/97	Added check for UTF to not draw widths	*
C* S. Jacobs/NCEP	 1/98	Modified SW dots to be circular		*
C* A. Hardy/GSC          9/98   Added RBK device driver                 *
C* S. Jacobs/NCEP	 2/99	Added rounded endpoints for all lines	*
C* S. Jacobs/NCEP	 3/99	Made rounded endpoints smaller dots	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GBUFFT.CMN'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
	INTEGER		ixd (*), iyd (*)
C
	INTEGER		ix (2), iy (2)
	LOGICAL		linvis, betwen, middle, dotflg
C
	MIDDLE (i1, i2, i3) = ( ( ( i1 .lt. i2 ) .and. ( i2 .lt. i3 ) )
     +						 .or.
     +				( ( i1 .gt. i2 ) .and. ( i2 .gt. i3 ) ))
C
	BETWEN ( i ) = ( ( ( ixd (i) .eq. kgx (nn) ) .and.
     +			   ( ixd (i) .eq. ixd (i+1) ) .and.
     +			   ( MIDDLE ( kgy (nn), iyd (i), iyd (i+1) ) ) )
     +					.or.
     +			 ( ( iyd (i) .eq. kgy (nn)  ) .and.
     +			   ( iyd (i) .eq. iyd (i+1) ) .and.
     +			   ( MIDDLE ( kgx (nn), ixd (i), ixd (i+1)))))
C------------------------------------------------------------------------
C*	Move points into temporary buffer, eliminating duplicate points.
C
	kgx ( 1 ) = ixd ( 1 )
	kgy ( 1 ) = iyd ( 1 )
	nn = 1
C
C*	Check to see if dot is being drawn
C
	IF ( np .eq. 2 ) THEN
	    IF ( ( ixd(1) .eq. ixd(2) ) .and. ( iyd(1) .eq. iyd(2) ) )
     +      THEN
		dotflg = .true.
	    ELSE
		dotflg = .false.
	    END IF
	ELSE
	    dotflg = .false.
	END IF
C*
	DO  i = 2, np
C
C*	    Check to see if this is the same as the last point.
C
	    IF  ( ( ixd (i) .eq. kgx (nn) ) .and.
     +		  ( iyd (i) .eq. kgy (nn) ) ) THEN
		IF  ( ( i .eq. np ) .and. ( nn .eq. 1 ) ) THEN
		    nn = nn + 1
		    kgx (nn) = ixd (i)
		    kgy (nn) = iyd (i)
		END IF
C
C*		Otherwise, add last point to buffer.
C
	    ELSE IF ( i .eq. np ) THEN
		nn = nn + 1
		kgx (nn) = ixd (i)
		kgy (nn) = iyd (i)
C
C*		Check if this point is between last point and next
C*		point.
C
	    ELSE IF ( .not. BETWEN ( i ) ) THEN
		nn = nn + 1
		kgx (nn) = ixd (i)
		kgy (nn) = iyd (i)
	    END IF
	END DO
C
C*	The given line is always drawn.
C
	CALL HLINE ( nn, kgx, kgy, iret )
C
C*	Check that the line width is greater than 1 and is to be drawn
C*	using software.
C
	IF  ( ( mlwid .ne. 1 ) .and.
     +	      ( mlwhw .eq. 1 ) .and.
     +	      ( ddev .ne. 'UTF' ) .and. 
     +        ( ddev .ne. 'RBK' ) ) THEN
C
C*	    Find out how many points to draw "above" and "below" the line.
C
	    ibelow = mlwid / 2
	    IF  ( MOD ( mlwid, 2 ) .eq. 0 ) THEN
		iabove = ibelow - 1
	    ELSE
		iabove = ibelow
	    END IF
C
C*	    Loop though the points and draw the extra lines.
C
	    DO  i = 2, nn
		ix1 = kgx ( i-1 )
		iy1 = kgy ( i-1 )
		ix2 = kgx (  i  )
		iy2 = kgy (  i  )
		idelx = IABS ( ix2 - ix1 )
		idely = IABS ( iy2 - iy1 )
C
C*		Check for dot being drawn.
C
		IF  ( dotflg) THEN
C
C*		    Fill in dot as a circular shape.
C
		    DO  jy = -iabove, ibelow, 1
			iy (1) = iy1 - jy
			iy (2) = iy (1)
			hyp   = mlwid / 2.0
			side  = ( hyp*hyp - jy*jy ) ** .5
			iside = NINT ( side )
			DO jx = -iside, iside, 1
			    ix(1) = ix1 + jx
			    ix(2) = ix(1)
			    CALL ICLIP ( ix(1),iy(1),ix(2),iy(2),ix(1),
     +					 iy(1),ix(2),iy(2),linvis )
			   IF ( linvis ) CALL HLINE ( 2, ix, iy, iret )
			END DO
		    END DO
		ELSE
C
C*		    Increase width of line by repetition
C
		    IF  ( idelx .le. idely ) THEN
			DO  j = 1, ibelow
			    ix (1) = ix1 + j
			    iy (1) = iy1
			    ix (2) = ix2 + j
			    iy (2) = iy2
			    CALL ICLIP ( ix(1),iy(1),ix(2),iy(2),ix(1),
     +					 iy(1),ix(2),iy(2),linvis )
			    IF ( linvis ) CALL HLINE ( 2, ix, iy, iret )
			END DO
C*
			DO  j = 1, iabove
			    ix (1) = ix1 - j
			    iy (1) = iy1
			    ix (2) = ix2 - j
			    iy (2) = iy2
			    CALL ICLIP ( ix(1),iy(1),ix(2),iy(2),ix(1),
     +					 iy(1),ix(2),iy(2),linvis )
			    IF ( linvis ) CALL HLINE ( 2, ix, iy, iret )
			END DO
C
C*			Fill in both endpoints as a dot.
C
			DO  jy = -iabove+1, ibelow-1, 1
			    iy (1) = iy1 - jy
			    iy (2) = iy (1)
			    hyp   = mlwid / 2.0
			    side  = ( hyp*hyp - jy*jy ) ** .5
			    iside = NINT ( side )
			    DO jx = -iside+2, iside-1, 1
				ix(1) = ix1 + jx
				ix(2) = ix(1)
				CALL ICLIP ( ix(1), iy(1),
     +					     ix(2), iy(2),
     +					     ix(1), iy(1),
     +					     ix(2), iy(2), linvis )
			       IF  ( linvis )
     +				   CALL HLINE ( 2, ix, iy, iret )
			    END DO
			END DO
C
			DO  jy = -iabove+1, ibelow-1, 1
			    iy (1) = iy2 - jy
			    iy (2) = iy (1)
			    hyp   = mlwid / 2.0
			    side  = ( hyp*hyp - jy*jy ) ** .5
			    iside = NINT ( side )
			    DO jx = -iside+2, iside-1, 1
				ix(1) = ix2 + jx
				ix(2) = ix(1)
				CALL ICLIP ( ix(1), iy(1),
     +					     ix(2), iy(2),
     +					     ix(1), iy(1),
     +					     ix(2), iy(2), linvis )
			       IF  ( linvis )
     +				   CALL HLINE ( 2, ix, iy, iret )
			    END DO
			END DO
		    ELSE
			DO  j = 1, ibelow
			    ix (1) = ix1
			    iy (1) = iy1 - j
			    ix (2) = ix2
			    iy (2) = iy2 - j
			    CALL ICLIP ( ix(1),iy(1),ix(2),iy(2),ix(1),
     +					 iy(1),ix(2),iy(2),linvis )
			    IF ( linvis ) CALL HLINE ( 2, ix, iy, iret )
			END DO
C*
			DO  j = 1, iabove
			    ix (1) = ix1
			    iy (1) = iy1 + j
			    ix (2) = ix2
			    iy (2) = iy2 + j
			    CALL ICLIP ( ix(1),iy(1),ix(2),iy(2),ix(1),
     +					 iy(1),ix(2),iy(2),linvis )
			    IF ( linvis ) CALL HLINE ( 2, ix, iy, iret )
			END DO
C
C*			Fill in both endpoints as a dot.
C
			DO  jy = -iabove+1, ibelow-1, 1
			    iy (1) = iy1 - jy
			    iy (2) = iy (1)
			    hyp   = mlwid / 2.0
			    side  = ( hyp*hyp - jy*jy ) ** .5
			    iside = NINT ( side )
			    DO jx = -iside+2, iside-1, 1
				ix(1) = ix1 + jx
				ix(2) = ix(1)
				CALL ICLIP ( ix(1), iy(1),
     +					     ix(2), iy(2),
     +					     ix(1), iy(1),
     +					     ix(2), iy(2), linvis )
			       IF  ( linvis )
     +				   CALL HLINE ( 2, ix, iy, iret )
			    END DO
			END DO
C
			DO  jy = -iabove+1, ibelow-1, 1
			    iy (1) = iy2 - jy
			    iy (2) = iy (1)
			    hyp   = mlwid / 2.0
			    side  = ( hyp*hyp - jy*jy ) ** .5
			    iside = NINT ( side )
			    DO jx = -iside+2, iside-1, 1
				ix(1) = ix2 + jx
				ix(2) = ix(1)
				CALL ICLIP ( ix(1), iy(1),
     +					     ix(2), iy(2),
     +					     ix(1), iy(1),
     +					     ix(2), iy(2), linvis )
			       IF  ( linvis )
     +				   CALL HLINE ( 2, ix, iy, iret )
			    END DO
			END DO
		    END IF
		END IF
	    END DO
	END IF
C*
	RETURN
	END
