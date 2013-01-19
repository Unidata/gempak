	SUBROUTINE IARC  ( ix, iy, rx, ry, ns, angle1, angle2,
     +			   endpts, flgfil, flgcls, iret )
C************************************************************************
C* IARC									*
C*									*
C* This subroutine draws an elliptical arc with radii RX and RY at	*
C* point IX, IY. The arc is drawn counter-clockwise between ANGLE1 and	*
C* ANGLE2. The arc is subdivided into NS segments.			*
C*									*
C* IARC  ( IX, IY, RX, RY, NS, ANGLE1, ANGLE2, ENDPTS, FLGFIL, FLGCLS,  *
C*         IRET )							*
C*									*
C* Input parameters:							*
C*	IX		INTEGER		X coordinate of center		*
C*	IY		INTEGER		Y coordinate of center		*
C*	RX		REAL		Radius of X axis		*
C*	RY		REAL		Radius of Y axis		*
C*	NS		INTEGER		Number of segments 		*
C*	ANGLE1		REAL		First angle in degrees		*
C*	ANGLE2		REAL		Second angle in degrees		*
C*	ENDPTS (4)	REAL		Arc beginning and ending pts    *
C*	FLGFIL		LOGICAL		Fill flag			*
C*	FLGCLS		LOGICAL		Close flag			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/98						*
C* A. Hardy/GSC		 6/00		Calling change; added endpts    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
        REAL		endpts(4)
	INTEGER		ixp (LLMXPT), iyp (LLMXPT)
	LOGICAL		flgfil, flgcls
C------------------------------------------------------------------------
C*	Use at least two points to draw an arc.
C
	np = ns + 1
	IF  ( np .lt. 2 )  np = 2
	IF  ( np .ge. LLMXPT )  np = LLMXPT - 1
C
C*	Make sure that the radii are positive.
C
	radx = ABS ( rx )
	rady = ABS ( ry )
C
C*	Make sure that the total angle is <= 360 degrees.
C
	theta1 = angle1 * DTR
	theta2 = angle2 * DTR
	dtheta = theta2 - theta1
	IF  ( ABS (dtheta) .gt. TWOPI )  dtheta = TWOPI
C
	thta = dtheta / ( FLOAT (np-1) )
C
C*	Compute points around circumference. The temporary device
C*	arrays can only hold LLMXPT points at a time.
C
	DO  i = 1, np
	    xc =  radx * COS ( theta1 + thta * (i-1) ) * ispanx
	    yc =  rady * SIN ( theta1 + thta * (i-1) ) * ispany
	    ixp ( i ) = ix + NINT ( xc )
	    iyp ( i ) = iy + NINT ( yc )
	END DO
C
C*      Store the endpoint for the arc.
C
        endpts(1) = ixp(1)
        endpts(2) = iyp(1)
        endpts(3) = ixp(np)
        endpts(4) = iyp(np)
C
C*	If the arc should be closed, add the first point to the end.
C
	IF  ( flgcls )  THEN
	    np = np + 1
	    ixp (np) = ixp (1)
	    iyp (np) = iyp (1)
	END IF
C
C*	Draw line or fill area.
C
	IF  ( flgfil )  THEN
	    CALL IFILL  ( np, ixp, iyp, ier )
	  ELSE
	    CALL ILINE  ( np, ixp, iyp, ier )
	END IF
C*
	iret = 0
C*
	RETURN
	END
