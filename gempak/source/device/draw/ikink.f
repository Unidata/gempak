	SUBROUTINE IKINK  ( np, ix, iy, iret )
C************************************************************************
C* IKINK								*
C*									*
C* This subroutine draws a two-point pointing arrow line with a kink	*
C* on the line.	The arrow could be open or filled. 			*
C*									*
C* Note: if the line is shorter than specified (25 in device unit), a   *
C* straight arrow line will be drawn instead of a kink line.			*
C*									*
C* IKINK ( NP, IX, IY, IRET )						*
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
C* J. Wu/SAIC		10/01	initial coding based on IARRW.F		*	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER		ix (*), iy (*)
	INTEGER		ipx (4), ipy (4), ihdx (3), ihdy (3)
C
C*	HANGLE1 is the kink slant angle in radians.
C*	HANGLE2 is the arrow head angle in radians.
C*	SDIST is the minimum length for drawing a kink line.
C*	MINXD is used for computing ATAN of an angle.
C
	PARAMETER	( HANGLE1 = DTR * 45. )
	PARAMETER	( HANGLE2 = DTR * 135. )
	PARAMETER	( SDIST = 25. )
	PARAMETER	( MINXD = 0.001 )

C------------------------------------------------------------------------

	iret = NORMAL
C
C*	Compute the line's length. If the start point is the same
C*	as the end point, quit and do nothing. 
C
	ixs = ix (1)
	iys = iy (1)
	ixe = ix (np)
	iye = iy (np)	
	IF ( ( ixs .eq. ixe ) .and. ( iys .eq. iye ) ) RETURN	
	xdis = ixe - ixs
	ydis = iye - iys
	xlen = SQRT ( ( xdis * xdis ) + ( ydis * ydis ) )
C
C*	Compute the line's direction 
C*	Note: a clock-wise angle is considered positive.
C
	xcos = xdis / xlen
	xsin = ydis / xlen
	IF ( ABS (xdis) .lt. MINXD ) THEN 	
	    alpha = 90. * DTR
	    IF ( ydis .lt. 0 )  alpha = -alpha
	  ELSE	
	    alpha = ATAN2 ( ydis, xdis )
	ENDIF	
C
C*	First point is always the base of pointing arrow line.
C
	ipx (1) = ixs
	ipy (1) = iys
C
C*	Compute arrow head length.
C
	hdsiz = tslsiz * bscalh
	hlen = hdsiz * ABS ( COS ( HANGLE1 ) )
C
C*	Computer other necessary coordinates for arrow shaft & head.
C*      A. If the line is shorter than specified, draw a straight 
C*	   2-point arrow line. Otherwise, add two kink points and
C*	   draw a 4-point kink arrow line. 
C*	B. If a filled arrow head is required, the arrow shaft will
C*	   be shorten to leave some space for adding the head.  
C
	IF ( xlen .lt. SDIST )  THEN
	    
	    npts_shaft = 2
	    rang1 = alpha - HANGLE2
	    rang2 = alpha + HANGLE2	
	    
	    IF ( msltyp .eq. 24 ) THEN
	        ipx (2) = ixe
	        ipy (2) = iye	    
	      ELSE
		ipx (2) = ixe - NINT ( hlen * xcos )
	        ipy (2) = iye - NINT ( hlen * xsin )	    			
	        IF ( ipx (2) .lt. 0 ) ipx (2) = 0
	        IF ( ipy (2) .lt. 0 ) ipy (2) = 0
	    ENDIF    	

          ELSE 
C
C*	  Compute kink points' coordinates.
C	  
	    npts_shaft = 4
C
C*	    First, compute the mid point's coordinates.
C	
	    pos = mslstr / 100.0
	    IF ( ( pos .lt. .25 ) .or. ( pos .gt. .75 ) )  pos = .50	
	    xmidlen = xlen * pos
	    ixmid = ixs + NINT ( xmidlen * xcos )
	    iymid = iys + NINT ( xmidlen * xsin )
C
C*	    Now, compute the kink points' coordinates. 
C	
	    rdir = alpha - HANGLE1
	    ixn = NINT ( hdsiz * COS ( rdir ) )
	    iyn = NINT ( hdsiz * SIN ( rdir ) )
	    ipx (2) = ixmid + ixn
	    ipy (2) = iymid + iyn
	    ipx (3) = ixmid - ixn
	    ipy (3) = iymid - iyn
C
C*	    Compute the arrow's direction after shifting.
C		
	    xnd = ixe - ipx (3)
	    ynd = iye - ipy (3)
	    xnlen = SQRT ( ( xnd * xnd ) + ( ynd * ynd ) )
	    xncos = xnd / xnlen
	    xnsin = ynd / xnlen
	    
	    IF ( ABS (xnd) .lt. MINXD ) THEN 	
	        beta = 90. * DTR
	        IF ( ynd .lt. 0 )  beta = -beta
	      ELSE	
	        beta = ATAN2 ( ynd, xnd )
	    ENDIF
	    
	    rang1 = beta - HANGLE2
	    rang2 = beta + HANGLE2

	    IF ( msltyp .eq. 24 ) THEN
	        ipx (4) = ixe
	        ipy (4) = iye	    
	      ELSE
	        ipx (4) = ixe - NINT ( hlen * xncos )
	        ipy (4) = iye - NINT ( hlen * xnsin )	    	
	        IF ( ipx (4) .lt. 0 ) ipx (4) = 0
	        IF ( ipy (4) .lt. 0 ) ipy (4) = 0
	    ENDIF    	
	ENDIF
C
C*	Compute arrow head. The center point of the arrow 
C*	head should always be the last input point. 
C
	ihdx (2) = ixe
	ihdy (2) = iye
C
C*	Compute arrow's side points' coordinates.
C				
	ihdx (1) = ixe + NINT ( hdsiz * COS (rang1) )
	ihdy (1) = iye + NINT ( hdsiz * SIN (rang1) )
	ihdx (3) = ixe + NINT ( hdsiz * COS (rang2) )
	ihdy (3) = iye + NINT ( hdsiz * SIN (rang2) )
C
C*	Draw the arrow shaft.
C	
        CALL ILINE  ( npts_shaft, ipx, ipy, ier )
C
C*	Fill the head if required, always use a solid fill.
C*	Do not fill the arrow head if the device is UTF or RBK.
C
	IF  ( ( msltyp .eq. 25 ) .and. 
     +	      ( ( ddev .ne. 'UTF' ) .and. ( ddev .ne. 'RBK' ) ) ) THEN
	    msvft = mfltyp
	    svfsz = tfilsz
	    CALL DSFILL ( 1.0, 1, fsize, itype, ier )
	    CALL IFILL  ( 3, ihdx, ihdy, ier )
	    CALL DSFILL ( svfsz, msvft, fsize, itype, ier )
	  ELSE
C
C*	    Draw the outside of the arrow head using lines.
C
	    CALL ILINE  ( 3, ihdx, ihdy, ier )
	END IF
C*
	RETURN
	END
