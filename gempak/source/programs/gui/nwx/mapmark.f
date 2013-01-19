	SUBROUTINE MAP_MARK ( nltln, rlat, rlon, ivalue, ncolor,
     +			      breaks, icolrs, mrktyp, sizmrk,
     +			      mrkwid, pltval, iposn, iret )
C************************************************************************
C* MAP_MARK								*
C*									*
C* This routine will plot markers at the stations. Options include	*
C* plotting in different colors, and displaying the value with a	*
C* marker.								*
C*									*
C* MAP_MARK ( NLTLN, RLAT, RLON, IVALUE, NCOLOR, BREAKS, ICOLRS,	*
C*	      MRKTYP, SIZMRK, MRKWID, PLTVAL, IPOSN, IRET )		*
C*									*
C* Input parameters:							*
C*	NLTLN		INTEGER		Number of points		*
C*	RLAT (*)	REAL		Latitudes of points		*
C*	RLON (*)	REAL		Longitudes of points		*
C*	IVALUE (*)	INTEGER		Data values of points		*
C*	NCOLOR		INTEGER		Number of color levels		*
C*	BREAKS (*)	INTEGER		Values for color levels		*
C*	ICOLRS (*)	INTEGER		Array of colors			*
C*	MRKTYP		INTEGER		Marker type			*
C*	SIZMRK		REAL		Marker size			*
C*	MRKWID		INTEGER		Marker line width		*
C*	PLTVAL		LOGICAL		Plot values flag		*
C*	IPOSN		INTEGER		Plot position			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NMC	 7/94						*
C* D. Keiser/GSC	10/96		Added changes from S. Chiswell	*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C* E. Safford/SAIC	12/07	use gemplot wrapper functions		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		rlat (*), rlon (*)
	INTEGER		icolrs (*), ivalue (*), breaks (*)
	LOGICAL		pltval
C*
	CHARACTER	chbuf*20
	INTEGER		ixof (10), iyof (10)
	REAL		rx (LLSTFL), ry (LLSTFL)
	INCLUDE		'ERMISS.FNC'
C*
	DATA		ixof  / 0, 0, 2, 2,  0,  2, 0,  0, 0,  0 /
	DATA		iyof  / 2, 0, 2, 0, -2, -2, 4, -4, 2, -2 /
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Set the marker attributes.
C
	CALL WGEM_GSMRKR ( mrktyp, 0, sizmrk, mrkwid, ier )
C
C*	If there is only 1 color and no values are requested,
C*	plot all of the markers at one time.
C
	IF  ( ( ncolor .eq. 1 ) .and. ( .not. pltval ) )  THEN
	    CALL WGEM_GSCOLR ( icolrs(1), ier )
	    CALL WGEM_GMARK ( 'M', nltln, rlat, rlon, ier )
	    CALL WGEM_GEPLOT ( ier )
	    RETURN
	END IF
C
C*	Transform from map coordinates to plot coordinates.
C
	CALL GTRANS ( 'M', 'P', nltln, rlat, rlon, rx, ry, ier )
C
C*	Loop over all of the points.
C
	DO  ip = 1, nltln
	    IF  ( .not. ERMISS ( FLOAT(ivalue(ip)) ) )  THEN
C
C*		Set the color based on the data values and the
C*		break-point values.
C
		ictmp = icolrs (1)
		DO  j = 1, ncolor - 1
		    IF  ( ivalue (ip) .gt. breaks (j) )  THEN
			ictmp = icolrs (j+1)
		    END IF
		END DO
		CALL WGEM_GSCOLR ( ictmp, ier )
C
C*		Find the location to plot the values, if requested.
C
		IF  ( pltval )  THEN
C		    CALL ST_RLCH ( ivalue(ip), 0, chbuf, ier )
		    CALL ST_INCH ( ivalue(ip), chbuf, ier )
		    CALL ST_LSTR ( chbuf, isiz, ier )
		    sx = rx(ip)
		    sy = ry(ip)
		    ix = 0
		    iy = 0 
		    IF  ( ( iposn .eq.  1 ) .or.
     +			  ( iposn .eq.  2 ) .or.
     +			  ( iposn .eq.  5 ) )  THEN
			ix = ixof ( iposn ) - ( isiz * 2 )
			iy = iyof ( iposn )
		    ELSE IF  ( ( iposn .eq.  3 ) .or.
     +			       ( iposn .eq.  4 ) .or.
     +			       ( iposn .eq.  6 ) )  THEN
			ix = ixof ( iposn )
			iy = iyof ( iposn )
		    ELSE IF  ( ( iposn .eq.  7 ) .or.
     +			       ( iposn .eq.  8 ) .or.
     +			       ( iposn .eq.  9 ) .or.
     +			       ( iposn .eq. 10 ) )  THEN
			ix = - isiz + 1
			iy = iyof ( iposn )
		    END IF
C
C*		    Plot the text.
C
		    CALL WGEM_GTEXT ( 'P', sx, sy, chbuf (1:isiz),
     +				 0., ix, iy, ier )
		ELSE
C
C*	           Plot the marker.
C
	           CALL WGEM_GMARK ( 'P', 1, rx(ip), ry(ip), ier )
	       END IF
	    END IF
	END DO
C
C*	Flush the buffers.
C
	CALL WGEM_GEPLOT ( ier )
C*
	RETURN
	END
