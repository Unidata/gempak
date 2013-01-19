	SUBROUTINE GDBPNT  ( skip, kx, ky, ix1, iy1, ix2, iy2, ixstep, 
     +			     iystep, istag, latt, alatsk, iret )
C************************************************************************
C* GDBPNT								*
C*									*
C* This subroutine finds the grid points at which wind barbs or arrows	*
C* should be plotted.  The variable SKIP contains the increment for	*
C* x and y to be used in skipping grid points.				*
C*									*
C* GDBPNT  ( SKIP, KX, KY, IX1, IY1, IX2, IY2, IXSTEP, IYSTEP,		*
C*	     ISTAG, LATT, ALATSK, IRET )				*
C*									*
C* Input parameters:							*
C*	SKIP		CHAR*		Value for point input by user	*
C*	KX		INTEGER		Number of points in x dir	*
C*	KY		INTEGER		Number of points in y dir	*
C*									*
C* Output parameters:							*
C*	IX1		INTEGER		First point in x dir		*
C*	IY1		INTEGER		First point in y dir		*
C*	IX2		INTEGER		Last point in x dir		*
C*	IY2		INTEGER		Last point in y dir		*
C*	IXSTEP		INTEGER		Increment in x dir		*
C*	IYSTEP		INTEGER		Increment in y dir		*
C*	ISTAG		INTEGER		Increment for stagger		*
C*	LATT		LOGICAL		Flag for lat thinning		*
C*	ALATSK(*)	REAL 		Array for lat thinning		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
c*					 -9 = invalid grid subset area	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	2/85						*
C* K. Brill/NMC		1/92	Replaced POINTS with SKIP		*
C* K. Brill/NMC		2/92	Add 1 to ISKPWN				*
C* L. Sager/NMC		6/93	Add stagger to skip increments		*
C* S. Jacobs/NCEP	5/96	Added istag to calling sequence		*
C* T. Lee/GSC		7/99	Added latitudinal dependant skip	*
C************************************************************************
	CHARACTER*(*)	skip
	REAL		alatsk (*)
C*
	INTEGER		iskpwn (2)
	LOGICAL		latt
C------------------------------------------------------------------------
	iret = 0
C
C*	Break the value for point into three values.
C
	CALL IN_SKIP ( skip, iskpxy, iskpwn, ier )
	ixinc = iskpwn (1)
	iyinc = iskpwn (2)
	IF  ( iyinc .ge. 0 ) THEN
C
C*	    Normal skip processing.
C
	    latt = .false.
C
C*	    Check for stagger ( ixinc .lt. 0 ).
C
	    IF ( ixinc .ge. 0 )  THEN
		ixstep = ixinc + 1
		istag  = 0
	      ELSE
		ixstep = - ixinc + 1
		istag  = ixstep / 2
	    END IF
	    iystep = iyinc + 1
C
	  ELSE IF ( iyinc .lt. 0 )  THEN
C
C*	    Latitudinal thinning processing ( iyinc .lt. 0 ).
C*
C*	    For each latitude degree, figure out the number of
C*	    wind barbs that would fit around a latitude circle,
C*	    given the currect size of a wind barb (rszwb) and the
C*	    graphical distance along the latitude line (dd).
C*	    The variable ixinc is not used.
C
	    latt = .true.
	    istag = 0
	    ixstep = 1
	    iystep = - iyinc
C
	    CALL GQSYSZ ( rxszmk, ryszmk, rxsztx, rysztx,
     +			  rxszwb, ryszwb, iret )
	    rszwb = SQRT ( rxszwb * rxszwb + ryszwb * ryszwb )
	    CALL GQBND ( 'M', rlatmn, rlonmn,
     +			 dlatmx, dlonmx, iret )
C
C*	    Loop through each latitude.
C
	    DO  ialt = -90, 90
		alt1 = ialt + 91.0
		CALL GTRANS ( 'M', 'N', 1, ialt, rlonmn,
     +			      xout1, yout1, iret )
		CALL GTRANS ( 'M', 'N', 1, ialt, rlonmn+1,
     +			      xout2, yout2, iret )
		dd = SQRT ( ( xout1-xout2 )**2 + ( yout1-yout2 )**2 )
		dd = dd * 360.0
		alatsk ( INT (alt1) ) = dd / rszwb
	    END DO
C
C*	    Set north and south pole values such that only
C*	    one wind barb gets plotted there.
C
	    alatsk (  1) = 0.0
	    alatsk (181) = 0.0
C
	END IF
C
C*	Check which points are in graphics area. 
C
	CALL GR_GALM  ( kx, ky, ix1, iy1, ix2, iy2, ier )
C
C*	Reset variables to 0, if grid subset values were invalid.
C
	IF  ( ier .ne. 0 ) THEN
	    ix1   = 0
	    iy1   = 0
	    ix2   = 0
	    iy2   = 0
	    ixinc = 0
	    iyinc = 0
	    iret  = ier
	END IF
C*
	RETURN
	END
