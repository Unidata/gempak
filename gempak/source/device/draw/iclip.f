	SUBROUTINE ICLIP  ( ix1, iy1, ix2, iy2, ixc1, iyc1, ixc2, iyc2,
     +			    linvis )
C************************************************************************
C* ICLIP                                                              	*
C*                                                                    	*
C* This subroutine clips line segments using the clipping window	*
C* which is stored in the common area.  If the line segment does	*
C* not intersect the clipping area, LINVIS is returned as false and	*
C* the output points are set to the input points.  Otherwise, the	*
C* clipped line segment points are returned.				*
C*                                                                   	*
C* ICLIP  ( IX1, IY1, IX2, IY2, IXC1, IYC1, IXC2, IYC2, LINVIS )	*
C*									*
C* Input parameters:							*
C*	IX1		INTEGER		X coordinate 1			*
C*	IY1 		INTEGER		Y coordinate 1			*
C*	IX2	  	INTEGER		X coordinate 2			*
C*	IY2	  	INTEGER		Y coordinate 2			*
C*                                                                    	*
C* Output parameters:							*
C*	IXC1		INTEGER		Clipped x coordinate 1		*
C*	IYC1    	INTEGER		Clipped y coordinate 1		*
C*	IXC2	  	INTEGER		Clipped x coordinate 2		*
C*	IYC2	  	INTEGER		Clipped y coordinate 2		*
C*	LINVIS  	LOGICAL		Visible flag 			*
C**                                                                   	*
C* Log:                                                               	*
C* M. Goodman/RDS	 1/83	                                      	*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* M. desJardins/GSFC	 4/90	Change coding scheme to logical 	*
C*				functions to make portable		*
C************************************************************************
	INCLUDE		'DVWNDW.CMN'
C*
        LOGICAL		linvis
C*
	LOGICAL		left1, left2, rght1, rght2, belw1, belw2,
     +			abov1, abov2, first, loop
C------------------------------------------------------------------------
C*	Initialize output variables
C
        ixc1   = ix1
        iyc1   = iy1
        ixc2   = ix2
        iyc2   = iy2
	linvis = .true.
	loop = .true.
C*
	DO WHILE ( loop )
C
C*	  The sign of ISPANX and ISPANY are used to indicate the
C*	  direction of increasing X and Y coordinates.
C*	  Logical flags are set to indicate whether the points are to
C*	  the left, right, above or below the clipping area.
C
	  left1 = .false.
	  left2 = .false.
	  rght1 = .false.
	  rght2 = .false.
	  belw1 = .false.
	  belw2 = .false.
	  abov1 = .false.
	  abov2 = .false.
C
C*	  Find the position of the first point.
C
	  IF ( ispanx * (ixc1-icleft) .lt. 0 ) THEN
	    left1 = .true.
	  ELSE IF ( ispanx * (ixc1-icrght) .gt. 0 ) THEN
	    rght1 = .true.
	  END IF
	  IF ( ispany * (iyc1 - icbot) .lt. 0 ) THEN
	    belw1 = .true.
	  ELSE IF ( ispany * (iyc1 - ictop) .gt. 0 ) THEN
	    abov1 = .true.
	  END IF
C
C*	  Find the position of the second point.
C
	  IF ( ispanx * (ixc2 - icleft) .lt. 0 ) THEN
	    left2 = .true.
	  ELSE IF ( ispanx * (ixc2 - icrght) .gt. 0 ) THEN
	    rght2 = .true.
	  END IF
	  IF ( ispany * (iyc2 - icbot) .lt. 0 ) THEN
	    belw2 = .true.
	  ELSE IF ( ispany * (iyc2 - ictop) .gt. 0 ) THEN
 	    abov2 = .true.
	  END IF
C
C*	  If the line cannot be clipped, exit.
C
	  IF  ( ( left1 .and. left2 ) .or. ( rght1 .and. rght2 ) .or.
     +	      ( belw1 .and. belw2 ) .or. ( abov1 .and. abov2 ) )  THEN
	    linvis = .false.
	    RETURN
	  END IF
C
C*	  This loop creates a new line segment by clipping the line at 
C*	  the top, bottom, left and right borders of the clipping
C*	  window. 
C
	  loop = ( left1 .or. left2 .or. rght1 .or. rght2 .or.
     +		    belw1 .or. belw2 .or. abov1 .or. abov2 )
	  IF ( loop ) THEN
	    IF  ( left1 .or. rght1 .or. belw1 .or. abov1 )  THEN
		ix = ixc1
		iy = iyc1
		first = .true.
	      ELSE
		ix = ixc2
		iy = iyc2
		first = .false.
	    END IF
C
C*	    Truncate line at left edge of window.
C
	    IF ( ( left1 .and. first ) .or. 
     +		 ( left2 .and. .not. first ) )  THEN
		x = icleft
		y = iyc1 + FLOAT ( (iyc2 - iyc1) * (icleft - ixc1)) /
     +			   FLOAT ( ixc2 -ixc1 )
		left1 = .false.
		left2 = .false.
C
C*		Truncate line at right edge of window.
C
	      ELSE IF ( ( rght1 .and.  first ) .or. 
     +		        ( rght2 .and.  .not. first ) ) THEN
		x = icrght
	        y = iyc1 + FLOAT ( (iyc2 - iyc1) * (icrght - ixc1)) /
     +			   FLOAT ( ixc2 - ixc1 )
		rght1 = .false.
		rght2 = .false.
C
C*		Truncate line at bottom edge of window.
C
	      ELSE IF ( ( belw1 .and. first ) .or.
     +			( belw2 .and. .not. first ) ) THEN
		x = ixc1 + FLOAT ( (ixc2 - ixc1) * (icbot - iyc1)) /
     +			   FLOAT ( iyc2 - iyc1 )
		y = icbot
		belw1 = .false.
		belw2 = .false.
C
C*		Truncate line at top edge of window.
C
	      ELSE IF ( ( abov1 .and. first ) .or.
     +			( abov2 .and. .not. first ) ) THEN
		x = ixc1 + FLOAT ( (ixc2 - ixc1) * (ictop - iyc1)) / 
     +			   FLOAT ( iyc2 - iyc1 )
		y = ictop
		abov1 = .false.
		abov2 = .false.
	    END IF
C
C*	    Set and roundup end point coordinates.
C
	    IF  ( first )  THEN
		ixc1 = NINT (x)
		iyc1 = NINT (y)
	      ELSE
		ixc2 = NINT (x)
		iyc2 = NINT (y)
	    END IF
	  END IF
	END DO
C*
 	RETURN
	END
