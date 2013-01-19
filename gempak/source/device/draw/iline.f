	SUBROUTINE ILINE ( np, ix, iy, iret )
C************************************************************************
C* ILINE								*
C*									*
C* This subroutine draws lines on a graphics device.  The points are 	*
C* first clipped.							*
C*									*
C* ILINE ( NP, IX, IY, IRET )						*
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
C* M. desJardins/612	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/612	10/87	Cleaned up				*
C* M. Linda/GSC		 6/96	Replaced local buffers with GBUFFT	*
C************************************************************************
	INCLUDE		'DVWNDW.CMN'
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GBUFFT.CMN'
C*
	INTEGER 	ix (*), iy (*)
	LOGICAL		bounds, clip, linvis
C
C*	This statement function checks whether a point is within the
C*	current clipping area.
C
	bounds (j) = (( ispanx * (ix(j)  - icleft) .ge. 0 ) .and.
     +		      ( ispanx * (icrght - ix(j) ) .ge. 0 ) .and.
     +		      ( ispany * (iy(j)  - icbot ) .ge. 0 ) .and.
     +		      ( ispany * (ictop  - iy(j) ) .ge. 0 ))
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check to see that there are at least two points on the line.
C
	IF ( np .le. 1 ) RETURN
C*
	clip = .false.
	n = 0
	i = 1
C
C*	Loop through all the points.
C
	DO  i = 1, np
C
C*	    If the last point was not clipped and this point is inbounds,
C*	    add this point to the buffer.
C
	    IF  ( (.not. clip) .and. bounds(i) ) THEN
		n = n + 1
		igx (n) = ix(i)
		igy (n) = iy(i)
C
C*	      If the last point was clipped and this point is inbounds,
C*	      clip this line segment and start a new buffer.
C
	      ELSE IF ( bounds (i) ) THEN
C
		CALL ICLIP ( ix(i-1), iy(i-1), ix(i),  iy(i),
     +			     igx(1),  igy(1),  igx(2), igy(2), linvis )
		clip = .false.
		n = 2
C
C*	      If the first point is out of bounds, set the clip flag to true.
C
	      ELSE IF ( (.not. bounds(i)) .and. ( i .eq. 1) ) THEN
C
		clip = .true.
C
C*	      If the last point was in bounds and this point is out of bounds,
C*	      clip the line segment, add to buffer and dump buffer.
C
	      ELSE IF ( .not. clip ) THEN
C
		n = n + 1
		CALL ICLIP ( ix(i-1), iy(i-1), ix(i),   iy(i),
     +			     j1,      j2,      igx(n), igy(n), linvis )
C
		CALL IDASH ( n, igx, igy, ier )
		n = 0
		clip = .true.
C
C*	      If both the last point and this point are out of bounds,
C*	      clip the line segment.  If the line was visible, send the two
C*	      points to be drawn.
C
	      ELSE
C
		CALL ICLIP ( ix(i-1), iy(i-1), ix(i),  iy(i),
     +			     igx(1),  igy(1),  igx(2), igy(2), linvis )
C
		IF ( linvis ) CALL IDASH ( 2, igx, igy, ier )
		n    = 0
		clip = .true.
C
	    END IF
C
	END DO
C
C*	Flush the buffer if necessary.
C
	IF  ( n .gt. 0 ) CALL IDASH ( n, igx, igy, ier )
C*
	RETURN
	END
