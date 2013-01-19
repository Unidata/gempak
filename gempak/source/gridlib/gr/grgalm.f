	SUBROUTINE GR_GALM  ( kx, ky, imin, jmin, imax, jmax, iret )
C************************************************************************
C* GR_GALM								*
C*									*
C* This subroutine finds the boundaries of a subgrid which covers the	*
C* graphics area.							*
C*									*
C* GR_GALM  ( KX, KY, IMIN, JMIN, IMAX, JMAX, IRET )			*
C*									*
C* Input parameters:							*
C*	KX		INTEGER		Number of grid points in x dir	*
C*	KY		INTEGER		Number of grid points in y dir	*
C*									*
C* Output parameters:							*
C*	IMIN		INTEGER		Minimum x value in area		*
C*	JMIN		INTEGER		Minimum y value in area		*
C*	IMAX		INTEGER		Maximum x value in area		*
C*	JMAX		INTEGER		Maximum y value in area		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -9 = invalid subgrid		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/84						*
C* I. Graffman/RDS	11/86	Polar coordinate check			*
C* M. desJardins/GSFC	 6/88	Fixed for new GQBND			*
C************************************************************************
C-----------------------------------------------------------------------
	iret = 0
	imin = 0
	jmin = 0
	imax = 0
	jmax = 0
	fx   = FLOAT (kx)
	fy   = FLOAT (ky)
C
C*	Special check for polar coordinates.
C
	CALL GQMODE  ( mode, ier )
	IF  ( mode .eq. 2 )  THEN
	    CALL GQGGRF  ( ixtype, iytype, jx, jy, r1, r2, r3, r4, ier )
	    IF  ( ixtype .eq. 5 )  THEN
	        imin = 1
	        jmin = 1
	        imax = kx
	        jmax = ky
	        RETURN
	    END IF
	END IF
C
C*	Get the coordinates which cover the grid.
C
	CALL GQBND  ( 'G', x1, y1, x2, y2, ier )
C
C*	The results need to be turned into integers.
C
	IF  ( ier .ne. 0 )  THEN
	    iret = -9
	  ELSE 
	    imin = NINT ( x1 )
	    imax = NINT ( x2 )
	    jmin = NINT ( y1 )
	    jmax = NINT ( y2 )
	END IF
C*
	RETURN
	END
