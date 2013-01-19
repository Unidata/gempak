	SUBROUTINE ISPLN ( np, ix, iy, iret )
C************************************************************************
C* ISPLN								*
C*									*
C* This subroutine draws special lines on a graphics device.  The	*
C* points are first clipped.						*
C*									*
C* ISPLN ( NP, IX, IY, IRET )						*
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
C* S. Jacobs/NCEP	 3/98	Copied from ILINE			*
C************************************************************************
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'DEVACT.CMN'
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GBUFFT.CMN'
C*
	INTEGER 	ix (*), iy (*)
C*
	INTEGER 	jx (LLMXPT), jy (LLMXPT)
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
		jx (n) = ix(i)
		jy (n) = iy(i)
C
C*	      If the last point was clipped and this point is inbounds,
C*	      clip this line segment and start a new buffer.
C
	      ELSE IF ( bounds (i) ) THEN
C
		CALL ICLIP ( ix(i-1), iy(i-1), ix(i),  iy(i),
     +			     jx(1),  jy(1),  jx(2), jy(2), linvis )
		clip = .false.
		n = 2
C
C*	      If the first point is out of bounds, set the clip flag
C*	      to true.
C
	      ELSE IF ( (.not. bounds(i)) .and. ( i .eq. 1) ) THEN
C
		clip = .true.
C
C*	      If the last point was in bounds and this point is out of
C*	      bounds, clip the line segment, add to buffer and dump
C*	      buffer.
C
	      ELSE IF ( .not. clip ) THEN
C
		n = n + 1
		CALL ICLIP ( ix(i-1), iy(i-1), ix(i),   iy(i),
     +			     j1,      j2,      jx(n), jy(n), linvis )
C
		CALL IPATLN ( n, jx, jy, ier )
		n = 0
		clip = .true.
C
C*	      If both the last point and this point are out of bounds,
C*	      clip the line segment.  If the line was visible, send the
C*	      two points to be drawn.
C
	      ELSE
C
		CALL ICLIP ( ix(i-1), iy(i-1), ix(i),  iy(i),
     +			     jx(1),  jy(1),  jx(2), jy(2), linvis )
C
		IF  ( linvis )  THEN
		    npt = 2
		    CALL IPATLN ( npt, jx, jy, ier )
		END IF
		n    = 0
		clip = .true.
C
	    END IF
C
	END DO
C
C*	Flush the buffer if necessary.
C
	IF  ( n .gt. 0 ) CALL IPATLN ( n, jx, jy, ier )
C*
	RETURN
	END
