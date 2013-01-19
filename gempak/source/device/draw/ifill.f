	SUBROUTINE IFILL  ( np, ix, iy, iret )
C************************************************************************
C* IFILL								*
C*									*
C* This subroutine fills a polygon on a graphics device.  The points	*
C* are first clipped.							*
C*									*
C* IFILL  ( NP, IX, IY, IRET )						*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	IX (NP)		INTEGER		X coordinates in device units	*
C*	IY (NP)		INTEGER		Y coordinates in device units	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Whistler/SSAI	10/91	Adapted from ILINE			*
C* M. Linda/GSC		 2/97	Replaced local buffers with GBUFFT	*
C************************************************************************
	INCLUDE		'DVWNDW.CMN'
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'GBUFFT.CMN'
C*
	INTEGER 	ix (*),  iy (*)
	LOGICAL		bounds, clip, linvis, inside
	LOGICAL		lft1, top1, bot1, rgt1, lft2, top2, bot2, rgt2
C
C*	This statement function checks whether a point is within the
C*	current clipping area.
C
	bounds (j) = (( ispanx * (ix(j)   - icleft) .ge. 0 ) .and.
     +		      ( ispanx * (icrght  - ix(j) ) .ge. 0 ) .and.
     +		      ( ispany * (iy(j)   - icbot ) .ge. 0 ) .and.
     +		      ( ispany * (ictop   - iy(j) ) .ge. 0 ))
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check to see that there are at least three points in the polygon.
C
	IF  ( np .le. 2 )  RETURN
C*
	clip   = .false.
	inside = .false.
	n = 0
	i = 1
	IF ( ( .not. bounds (i) ) .or. ( .not. bounds (np) ) ) THEN
	    np1 = np + 1
	  ELSE
	    np1 = np
	END IF
C
C*	Loop through all the points.
C
	DO  i = 1, np1
C
C*	    This is point 2.
C
	    IF  ( i .le. np )  THEN
		ipx2 = ix (i)
		ipy2 = iy (i)
		ii   = i
	      ELSE
		ipx2 = ix (1)
		ipy2 = iy (1)
		ii   = 1
	    END IF
C
C*	    If the last point was not clipped and this point is inbounds,
C*	    add this point to the buffer.
C
	    IF ( (.not. clip) .and. bounds (ii) ) THEN
		n      = n + 1
		igx (n) = ipx2
		igy (n) = ipy2
		inside = .true.
C
C*	      If the last point was clipped and this point is inbounds,
C*	      clip this line segment and start a new buffer.
C
	      ELSE IF ( bounds (ii) ) THEN
		CALL ICLIP ( ipx1, ipy1, ipx2, ipy2, i1, i2, j1, j2, 
     +			     linvis )
		n = n + 1
		IF ( ( ipx2 .eq. j1 ) .and. ( ipy2 .eq. j2 ) )  THEN 
		    igx (n) = i1
		    igy (n) = i2
		  ELSE
		    igx (n) = j1
		    igy (n) = j2
		END IF
		n = n + 1
		igx (n) = ipx2
		igy (n) = ipy2
		clip   = .false.
		inside = .true.
C
C*	        If the first point is out of bounds, set the clip flag to true.
C
	      ELSE IF ( (.not. bounds (ii) ) .and. ( i .eq. 1 ) ) THEN
		clip = .true.
C
C*	        If the last point was in bounds and this point is out of bounds,
C*	        clip the line segment, add to buffer and dump buffer.
C
	      ELSE IF ( .not. clip ) THEN
C
		n = n + 1
		CALL ICLIP ( ipx1, ipy1, ipx2, ipy2, j1, j2, i1, i2, 
     +			     linvis )
		IF ( ( ipx1 .eq. j1 ) .and. ( ipy1 .eq. j2 ) )  THEN 
		    igx (n) = i1
		    igy (n) = i2
		  ELSE
		    igx (n) = j1
		    igy (n) = j2
		END IF
		clip = .true.
C
C*		Handle the case where the last point and this point are
C*		both out of bounds.
C
	      ELSE IF ( clip .and. (.not. bounds (ii) ) )  THEN
		CALL ICLIP ( ipx1, ipy1, ipx2, ipy2, j1, j2, i1, i2, 
     +			     linvis )
C
C*		Add the points if some are visible.
C
		IF ( linvis )  THEN
		    n = n + 1		 
		    igx (n) = j1
		    igy (n) = j2
		    n = n + 1		 
		    igx (n) = i1
		    igy (n) = i2
		    inside = .true.
C
C*		    If last two points are outside the clip bounds, check
C*		    to see if the points straddle a corner.
C
		  ELSE
		    lft1 = .false.
		    rgt1 = .false.
		    top1 = .false.
		    bot1 = .false.
		    lft2 = .false.
		    rgt2 = .false.
		    top2 = .false.
		    bot2 = .false.
C
C* 		    Find the position of the first point.
C

		    IF ( ispanx * (ipx2  -  icleft) .lt. 0 ) THEN
			lft1 = .true.
		      ELSE IF ( ispanx * (icrght -  ipx2) .lt. 0 ) THEN
			rgt1 = .true.
		    END IF
		    IF ( ispany * (ipy2  - icbot) .lt. 0 ) THEN
			bot1 = .true.
		      ELSE IF ( ispany * (ictop - ipy2) .lt. 0 ) THEN
			top1 = .true.
		    END IF
C
C* 		    Find the position of the second point.
C
		    IF ( ispanx * (ipx1 - icleft) .lt. 0 ) THEN
			lft2 = .true.
		      ELSE IF ( ispanx * (icrght - ipx1) .lt. 0 ) THEN
			rgt2 = .true.
		    END IF
		    IF ( ispany * (ipy1 - icbot) .lt. 0 ) THEN
			bot2 = .true.
		      ELSE IF ( ispany * (ictop - ipy1) .lt. 0 ) THEN
			top2 = .true.
		    END IF
C
C*		    Check to see if the two points surround a corner.
C
		    IF ( ( lft1 .and. bot1 ) .and. 
     +			 ( ( bot2 .and. .not. lft2 ) .or. 
     +			   ( lft2 .and. .not. bot2 ) ) ) THEN
			n = n + 1
			igx (n) = icleft
			igy (n) = icbot
		      ELSE IF ( ( lft1 .and. top1 ) .and. 
     +			      ( ( top2 .and. .not. lft2 ) .or. 
     +			        ( lft2 .and. .not. top2 ) ) )  THEN
			n = n + 1
			igx (n) = icleft
			igy (n) = ictop
		      ELSE IF ( ( rgt1 .and. bot1 ) .and. 
     +			      ( ( bot2 .and. .not. rgt2 ) .or. 
     +			        ( rgt2 .and. .not. bot2 ) ) )  THEN
			n = n + 1
			igx (n) = icrght
			igy (n) = icbot
		      ELSE IF ( ( rgt1 .and. top1 ) .and. 
     +			      ( ( top2 .and. .not. rgt2 ) .or. 
     +			        ( rgt2 .and. .not. top2 ) ) )  THEN
			n = n + 1
			igx (n) = icrght
			igy (n) = ictop
		      ELSE IF ( lft1 .and. bot2 )  THEN
			n = n + 1
			igx (n) = icleft
			igy (n) = icbot
		      ELSE IF ( lft1 .and. top2 )  THEN
			n = n + 1
			igx (n) = icleft
			igy (n) = ictop
		      ELSE IF ( rgt1 .and. bot2 )  THEN
			n = n + 1
			igx (n) = icrght
			igy (n) = icbot
		      ELSE IF ( rgt1 .and. top2 )  THEN
			n = n + 1
			igx (n) = icrght
			igy (n) = ictop
		      ELSE IF ( bot1 .and. lft2 )  THEN
			n = n + 1
			igx (n) = icleft
			igy (n) = icbot
		      ELSE IF ( bot1 .and. rgt2 )  THEN
			n = n + 1
			igx (n) = icrght
			igy (n) = icbot
		      ELSE IF ( top1 .and. lft2 )  THEN
			n = n + 1
			igx (n) = icleft
			igy (n) = ictop
		      ELSE IF ( top1 .and. rgt2 )  THEN
			n = n + 1
			igx (n) = icrght
			igy (n) = ictop
		    END IF
	    END IF
		clip = .true.
	    END IF
C
C*	    The second point becomes the first point.
C
	    ipx1 = ipx2
	    ipy1 = ipy2
	END DO
C
C*	Flush the buffer if necessary.
C
	IF  ( ( n .gt. 0 ) .and. inside )  THEN
	    CALL HFILL  ( n, igx, igy, iret)
	END IF
C*
	RETURN
	END
