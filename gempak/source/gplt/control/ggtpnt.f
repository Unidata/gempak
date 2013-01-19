	SUBROUTINE GGTPNT  ( sys, ityp, x, y, iret )
C************************************************************************
C* GGTPNT								*
C*									*
C* This subroutine returns the requested number of points from the 	*
C* cursor position when the mouse button is pressed.			*
C*									*
C* GGTPNT  ( SYS, ITYP, X, Y, IRET )					*
C*									*
C* Input parameters:							*
C*									*
C*	SYS		CHAR*		Coordinate system		*
C*                                        'S' = screen coordinates      *
C*					  'D' = device coordinates	*
C*					  'N' = normalized coordinates	*
C*					  'V' = view coordinates	*
C*					  'P' = plot coordinates	*
C*					  'M' = map coordinates		*
C*					  'G' = grid coordinates	*
C*	ITYP		INTEGER		Type of cursor			*
C*					   1  = point, 	      NP = 1	*
C*					   2  = line,  	      NP = 2	*
C*					   3  = box,   	      NP = 2	*
C*					   4  = center box,   NP = 2	*
C*					   13 = extended box, NP = 2	*
C*					   14 = ext. center box, NP = 2	*
C*									*
C* Output parameters:							*
C*									*
C*	X (NP)		REAL		X coordinates / latitudes	*
C*	Y (NP)		REAL		Y coordinates / longitudes	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Chou/EAI		 6/93						*
C* S. Jacobs/EAI	 6/93	Clean up				*
C* S. Jacobs/EAI	 9/93	Added ITYP				*
C* S. Jacobs/NCEP	 2/97	Added warning for no event processing	*
C* C. Lin/EAI	 	 6/97	Use S coordinates instead of D		*
C* S. Schotz/NCEP	 7/97	Update documentation for S coordinate	*
C* S. Jacobs/NCEP	 6/98	Removed NP from calling sequence	*
C* T. Lee/GSC		 1/01	Added extended zoom area		*
C* T. Lee/GSC		 1/01	Moved jspanx, jspany computation	*
C* M. Li/SAIC		12/07	Added center zoom mode			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'XYDEF.CMN'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE 	'ERROR.PRM'
C*
	CHARACTER*(*)	sys
	REAL		x ( * ), y ( * )    
C*
	INTEGER		ix ( 2 ), iy ( 2 )
	REAL		rx ( 2 ), ry ( 2 )
	CHARACTER	proj*72
C------------------------------------------------------------------------
	iret = NORMAL
	jtyp = MOD ( ityp, 10 )
	jext = ityp / 10
C
C*	Get the points.
C
	CALL DGTPNT ( jtyp, ix, iy, iret )    

	IF  ( iret .ne. 0 ) THEN
	    IF  ( iret .lt. 0 )  THEN
		iret = NGTPNT
	      ELSE
		iret = NEVENT
	    END IF
	    CALL ER_WMSG ( 'GEMPLT', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Convert the integer arrays to reals.
C
	IF  ( jtyp .eq. 1 )  THEN
	    np = 1
	  ELSE
	    np = 2
	END IF
C
	DO  j = 1, np
	    x ( j ) = FLOAT ( ix ( j ) )
	    y ( j ) = FLOAT ( iy ( j ) )
	END DO
C
C*	Get coordinate incremental direction.
C
	jspanx = ISIGN ( 1, ( iright - ileft ) )
	jspany = ISIGN ( 1, ( itop - ibot ) )
C
C*	Check for extended zoom area.
C
	IF  ( ( jtyp .eq. 3 .or. jtyp .eq. 4 ) .and. 
     +	      ( jext .eq. 1 ) )  THEN
C
C*	    Compute positions of margin boundaries.  Computation depends
C*	    on whether margin width is specified in fraction or by 
C*	    character width.  First pick up margin size and display 
C*	    boundaries cooresponding to current mode.
C
	   IF ( igmode .eq. 2 ) THEN
		xl = xlgmgn
	        yb = ybgmgn
		xr = xrgmgn
		yt = ytgmgn
		sz = cszg
	      ELSE IF ( igmode .eq. 1 ) THEN
		xl = xlmmgn
	        yb = ybmmgn
		xr = xrmmgn
		yt = ytmmgn
		sz = cszm
	      ELSE
		xl = 0.
		yb = 0.
		xr = 1.
		yt = 1.
		sz = 0.
	    END IF
C
C*	    Do margin position computation according to whether margin is
C*	    specified as fractional width or multiple of character size.
C
	    IF ( ( 0.0 .le. xl ) .and. ( xl .lt. 1.0 ) .and.
     *	         ( 0.0 .le. yb ) .and. ( yb .lt. 1.0 ) .and.
     *	         ( 0.0 .le. xr ) .and. ( xr .lt. 1.0 ) .and.
     *	         ( 0.0 .le. yt ) .and. ( yt .lt. 1.0 ) ) THEN
C*
	         xbndlw = xbndlv + xl * ( xbndrv - xbndlv )
	         ybndbw = ybndbv + yb * ( ybndtv - ybndbv )
	      	 xbndrw = xbndrv - xr * ( xbndrv - xbndlv )
	         ybndtw = ybndtv - yt * ( ybndtv - ybndbv )
C*
	      ELSE
	         xcsiz  =  7.0 * bscalc * sz / ABS ( andx1 )
	      	 ycsiz  =  9.0 * bscalc * sz / ABS ( andy1 )
	         xbndlw = xbndlv + xl * xcsiz
	         ybndbw = ybndbv + yb * ycsiz
	         xbndrw = xbndrv - xr * xcsiz
	         ybndtw = ybndtv - yt * ycsiz
	    END IF
C
C*	    Compute aspect ratio of view and zoom areas.
C
	    rv = ABS ( ( ybndtw - ybndbw ) / ( xbndrw - xbndlw ) )
	    dy = y ( 2 ) - y ( 1 )
	    dx = x ( 2 ) - x ( 1 )
	    rz = ABS ( dy / dx )
C	
C*	    Recompute extended zoom area when the apect ratio
C*	    is different.
C
	    IF  ( rz .gt. rv )  THEN
		xmid    = ( x ( 1 ) + x ( 2 ) ) / 2.
		x ( 1 ) = xmid - 0.5 * ABS ( dy ) / rv  * jspanx
		x ( 2 ) = xmid + 0.5 * ABS ( dy ) / rv  * jspanx
	      ELSE IF ( rz .lt. rv )  THEN
		ymid  = ( y ( 1 ) + y ( 2 ) ) / 2.
		y ( 1 ) = ymid - 0.5 * ABS ( dx ) * rv * jspany
		y ( 2 ) = ymid + 0.5 * ABS ( dx ) * rv * jspany
	    END IF
	END IF
C
C*	Inquiry map projection.
C
	CALL GQMPRJ ( proj,  angle1, angle2, angle3, 
     +		      dlatll, dlonll, dlatur, dlonur, iret )
C
C*	Check illegal bound for CED and MER projections.
C
	IF  ( ( proj .eq. 'CED' ) .or. ( proj .eq. 'MER' ) .or.
     +	      ( proj .eq. 'MCD' ) )  THEN
	    DO  i = 1, 2
		ry ( i ) = 0.
	    END DO
C
	    IF  ( proj .eq. 'CED' )  THEN
		rx ( 1 ) = -90.
		rx ( 2 ) =  90.
	      ELSE
		rx ( 1 ) = -89.
		rx ( 2 ) =  89.
	    END IF
	    CALL GTRANS ( 'M', 'S', 2, rx, ry, rx, ry, iret )
C
	    IF  ( jspany .ge. 0 )  THEN
		y ( 1 ) = AINT ( AMAX1 ( ry ( 1 ), y ( 1 ) ) ) + 1.
		y ( 2 ) = AINT ( AMIN1 ( ry ( 2 ), y ( 2 ) ) ) - 1.
	      ELSE
		y ( 1 ) = AINT ( AMIN1 ( ry ( 1 ), y ( 1 ) ) ) - 1.
		y ( 2 ) = AINT ( AMAX1 ( ry ( 2 ), y ( 2 ) ) ) + 1.
	    END IF
C
C*	    Check the x-direction bounds.
C
	END IF
C
C*      Transform from 'S' coordinates to the requested system.
C
	CALL GTRANS ( 'S', sys, np, x, y, x, y, iret )
C*
	RETURN
	END
