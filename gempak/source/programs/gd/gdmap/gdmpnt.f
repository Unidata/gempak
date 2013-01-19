	SUBROUTINE GDMPNT  ( skip, kx, ky, ix1, iy1, ix2, iy2, ixinc, 
     +			     iyinc, iret )
C************************************************************************
C* GDMPNT								*
C*									*
C* This subroutine finds the grid points at which data should be 	*
C* plotted.  The variable SKIP contains the increment for x and y to	*
C* be used in skipping grid points.					*
C*									*
C* GDMPNT  ( SKIP, KX, KY, IX1, IY1, IX2, IY2, IXINC, IYINC, IRET )	*
C*									*
C* Input parameters:							*
C*	SKIP		CHAR*		Value for SKIP input by user	*
C*	KX		INTEGER		Number of points in x dir	*
C*	KY		INTEGER		Number of points in y dir	*
C*									*
C* Output parameters:							*
C*	IX1		INTEGER		First point in x dir		*
C*	IY1		INTEGER		First point in y dir		*
C*	IX2		INTEGER		Last point in x dir		*
C*	IY2		INTEGER		Last point in y dir		*
C*	IXINC		INTEGER		Increment in x dir		*
C*	IYINC		INTEGER		Increment in y dir		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
c*					 -9 = invalid grid subset area	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* M. desJardins/GSFC	 8/88	Removed color 				*
C* K. Brill/NMC		01/92	Changes to replace POINTS with SKIP	*
C* K. Brill/NMC		02/92	Add 1 to ISKPWN				*
C************************************************************************
	CHARACTER*(*)	skip
C*
	INTEGER		iskpwn (2)
C------------------------------------------------------------------------
	iret = 0
C
C*	Break the value for point into four values.
C
	CALL IN_SKIP ( skip, iskpxy, iskpwn, ier )
	ix1   = 1
	iy1   = 1
	ixinc = iskpwn(1) + 1
	iyinc = iskpwn(2) + 1
C
C*	Check which points are in graphics area.
C
	CALL GR_GALM  ( kx, ky, imin, jmin, ix2, iy2, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = ier
	  ELSE
	    IF  ( ix1 .lt. imin )  THEN
		i1 = MOD ( (imin-ix1), ixinc )
		IF  ( i1 .ne. 0 )  i1 = ixinc - i1
		ix1 = imin + i1
	    END IF
	    IF  ( ix2 .lt. ix1 )  iret = -9
C*
	    IF  ( iy1 .lt. jmin )  THEN
		j1 = iyinc - MOD ( (jmin-iy1), iyinc )
		IF  ( j1 .ne. 0 )  j1 = iyinc - j1
		iy1 = jmin + j1
	    END IF
	    IF  ( iy2 .lt. iy1 )  iret = -9
	END IF
C
C*	Reset variables to 0, if grid subset values were invalid.
C
	IF  ( iret .ne. 0 )  THEN
	    ix1   = 0
	    iy1   = 0
	    ix2   = 0
	    iy2   = 0
	    ixinc = 0
	    iyinc = 0
	END IF
C*
	RETURN
	END
