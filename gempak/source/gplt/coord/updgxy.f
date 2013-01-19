	SUBROUTINE UPDGXY  ( xl, yb, xr, yt, iret )
C************************************************************************
C* UPDGXY								*
C* 									*
C* This subroutine checks the grid coordinates to find the range	*
C* which will be visible in the plot area.				*
C* 									*
C* UPDGXY ( XL, YB, XR, YT, IRET )					*
C*									*
C* Output parameters:							*
C*	XL		REAL		Lower left x grid point		*
C*	YB		REAL		Lower left y grid point		*
C*	XR		REAL		Upper right x grid point	*
C*	YT		REAL		Upper right y grid point	*
C*	IRET		INTEGER		Status				*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88						*
C* K. Brill/NMC		 1/91		Changed YR to YT in ( KXKY .gt.	*
C*   					10000 ) block IF		*
C* K. Brill/NMC		08/91		Finished changing YR to YT and  *
C*					changed 10000 to LLMXGD		*
C* K. Brill/HPC		08/02		Change LLMXGD to MXBFR and	*
C*					arrange code for bigger grids	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
	INCLUDE		'DEVCHR.CMN'
	PARAMETER	( MXBFR = 50000 )
C*
	LOGICAL		vis (MXBFR)
	REAL		xarr (MXBFR), yarr (MXBFR)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Get number of grid points.
C
	IF  ( ( igmode .eq. 1 ) .and. mset .and. mgset  )  THEN
	    kx = gpxrm
	    ky = gpytm
	  ELSE IF  ( ( igmode .eq. 2 ) .and. gset .and. ggset )  THEN
	    kx = gpxr
	    ky = gpyt
	  ELSE
	    iret = NIPROJ
	    RETURN
	END IF
C
C*	Compute kxky and compute number of iterations required.
C
	kxky = kx * ky
	numit = kxky / MXBFR
	IF ( MOD ( kxky, MXBFR ) .ne. 0 ) numit = numit + 1
C
C*	Execute NUMIT iterations to cover entire grid.
C
	imin = kx
	imax = 1
	jmin = ky
	jmax = 1
	istrt = 1
	DO it = 1, numit
	    istop = istrt + MXBFR - 1
	    IF ( istop .gt. kxky ) istop = kxky
	    knt = 0
	    DO indx = istrt, istop
C
C*		Now load grid points into arrays.
C
		i = MOD ( indx, kx )
		j = indx / kx + 1
		IF ( i .eq. 0 ) THEN
		    i = kx
		    j = j - 1
		END IF
		knt = knt + 1
		xarr ( knt ) = FLOAT ( i )
		yarr ( knt ) = FLOAT ( j )
	    END DO
C
C*	    Check points for visibility.
C
	    CALL GPTVIS  ( 'G', knt, xarr, yarr, vis, ier )
	    knt = 0
	    DO indx = istrt, istop
C
C*		Now check for max/min.
C
		knt = knt + 1
		i = NINT ( xarr (knt) )
		j = NINT ( yarr (knt) )
		IF  ( vis (knt) )  THEN
		    IF  ( j .lt. jmin )  jmin = j
		    IF  ( j .gt. jmax )  jmax = j
		    IF  ( i .lt. imin )  imin = i
		    IF  ( i .gt. imax )  imax = i
		END IF
	    END DO
	    istrt = istrt + MXBFR
	END DO
C
C*	Include an extra grid point and check that area is correct.
C
	IF  ( ( jmin .eq. ky ) .or. ( jmax .eq. 1 ) .or. 
     +	      ( imin .eq. kx ) .or. ( imax .eq. 1 ) )  THEN
	    iret = NOBNDS
	    xl   = 0.
	    yb   = 0.
	    xr   = 0.
	    yt   = 0.
	    RETURN
	END IF
C*
	ixl = FLOAT ( imin - 1 )
	IF  ( ixl .lt. 1 )  ixl = 1
	xl  = ixl
	ixr = FLOAT ( imax + 1 )
	IF  ( ixr .gt. kx )  ixr = kx
	xr  = ixr
	jyb = FLOAT ( jmin - 1 )
	IF  ( jyb .lt. 1 )  jyb = 1
	yb  = jyb
	jyt = FLOAT ( jmax + 1 )
	IF  ( jyt .gt. ky )  jyt = ky
	yt  = jyt
C*
	RETURN
	END
