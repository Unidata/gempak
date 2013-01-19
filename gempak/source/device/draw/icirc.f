	SUBROUTINE ICIRC  ( ix, iy, r, n, iret )
C************************************************************************
C* ICIRC								*
C*									*
C* This subroutine draws a circle of radius R at point IX, IY.  The	*
C* circle is subdivided into N segments.				*
C*									*
C* ICIRC  ( IX, IY, R, N, IRET )					*
C*									*
C* Input parameters:							*
C*	IX		INTEGER		X coordinate of center		*
C*	IY		INTEGER		Y coordinate of center		*
C*	R		REAL		Radius of circle		*
C*	N		INTEGER		Number of segments 		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 2/82						*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 9/88	Added buffer to send only 60 points	*
C* K. Brill/NMC		11/91	Draw circle in quadrants		*
C* S. Jacobs/NCEP	 3/98	Changed to call IARC			*
C* A. Hardy/GSC		 6/00   Added endpts parameter to IARC		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVCHR.CMN'
C
	REAL		endpts(4)
C------------------------------------------------------------------------
	nseg = n
	IF  ( nseg .lt. 8 )  nseg = 8
C
C*	Draw an arc from 0 to 360 degrees.
C
	CALL IARC  ( ix, iy, r, r, nseg, 0., 360.,
     +		     endpts, .false., .false., ier )
C*
	iret = 0
C*
	RETURN
	END
