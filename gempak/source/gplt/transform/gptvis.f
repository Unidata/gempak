	SUBROUTINE GPTVIS  ( sys, np, x, y, vis, iret )
C************************************************************************
C* GPTVIS								*
C*									*
C* This subroutine returns a logical array indicating whether the 	*
C* input points are within the bounds of the map/graph to be plotted.	*
C*									*
C* GPTVIS  ( SYS, NP, X, Y, VIS, IRET )					*
C*									*
C* Input parameters:							*
C*	SYS		CHAR*		Coordinate system		*
C*                                        'S' = screen coordinates      *
C*					  'D' = device coordinates	*
C*					  'N' = normalized coordinates	*
C*					  'V' = view coordinates	*
C*					  'P' = plot coordinates	*
C*					  'M' = map coordinates		*
C*					  'G' = grid coordinates	*
C*	NP		INTEGER		Number of points		*
C*	X (NP)		REAL		X coordinates / latitudes	*
C*	Y (NP)		REAL		Y coordinates / longitudes	*
C*									*
C* Output parameters:							*
C*	VIS (NP)	LOGICAL		Visible flags			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* I. Graffman/RDS	 2/88	Fixed buffering				*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 9/88	Added epsilon calculation		*
C* K. Brill/GSC		 4/89   Fixed conditional check for vis		*
C* M. desJardins/GSFC	 6/89	Fixed again				*
C* S. Schotz/NCEP        7/97   Update documentation for S coordinate   *
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'XYDEF.CMN'
	INCLUDE		'GTBUFF.CMN'
C*
	REAL		x (*), y (*)
	LOGICAL 	vis (*)
	CHARACTER*(*)	sys
C*
	CHARACTER	csys*1
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Convert to upper case.
C
	CALL ST_LCUC  ( sys, csys, ier )
C
C*	Check to see if points will fit in buffer.
C
	IF  ( np .le. 0 )  RETURN
	npass  = ( np - 1 ) / IGTBSZ + 1
	istart = 1
C
C*	Get visible bounds in linear coordinates.
C
	IF  ( igmode .eq. 1 )  THEN
	    xl1 = xmbndl
	    yl1 = ymbndb
	    xl2 = xmbndr
	    yl2 = ymbndt
	  ELSE
	    xl1 = xgbndl
	    yl1 = ygbndb
	    xl2 = xgbndr
	    yl2 = ygbndt
	END IF
C
C*	Make sure that point 1 is less than point 2.
C
	IF  ( xl1 .gt. xl2 )  THEN
	    tmp = xl2
	    xl2 = xl1
	    xl1 = tmp
	END IF
	IF  ( yl1 .gt. yl2 )  THEN
	    tmp = yl2
	    yl2 = yl1
	    yl1 = tmp
	END IF
C
C*	Add on small amount to account for rounding errors.
C
	xepsil = ( xl1 - xl2 ) * .0001
	yepsil = ( yl1 - yl2 ) * .0001
	xl1    = xl1 + xepsil
	yl1    = yl1 + yepsil
	xl2    = xl2 - xepsil
	yl2    = yl2 - yepsil
C
C*	Loop through buffers checking visibility.
C
	DO  m = 1, npass
	    iend = istart + IGTBSZ - 1
	    IF  ( iend .gt. np )  iend = np
	    knt  = 1
	    DO  j = istart, iend
	        gx (knt) = x (j)
	        gy (knt) = y (j)
	        knt = knt + 1
	    END DO
	    num    = iend - istart + 1
C
C*	    Transform to L coordinates.
C
	    CALL GTRANS  ( csys, 'L', num, gx, gy, gx, gy, iret )
C
C*	    Check for visible points.
C
	    knt = istart
	    DO  i = 1, num
		vis (knt) = ( ( gx (i) .ge. xl1 ) .and.
     +			      ( gx (i) .le. xl2 ) .and. 
     +			      ( gy (i) .ge. yl1 ) .and. 
     +			      ( gy (i) .le. yl2 ) )
		knt = knt + 1
	    END DO
C
C*	    Increment istart.
C
	    istart = iend + 1
	END DO
C*
	RETURN
	END
