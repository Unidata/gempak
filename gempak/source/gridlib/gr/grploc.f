	SUBROUTINE GR_PLOC  ( gpoint, rgx, rgy, rlat, rlon, iret )
C************************************************************************
C* GR_PLOC								*
C*									*
C* This subroutine translates the user input for a grid point into an	*
C* actual grid point, x and y coordinates, and latitude and longitude.	*
C*									*
C* GR_PLOC  ( GPOINT, RGX, RGY, RLAT, RLON, IRET )			*
C*									*
C* Input parameters:							*
C*	GPOINT		CHAR*		User input for grid point	*
C*									*
C* Output parameters:							*
C*	RGX		REAL		X grid point			*
C*	RGY		REAL		Y grid point			*
C*	RLAT		REAL		Latitude			*
C*	RLON		REAL		Longitude			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-12 = invalid grid point	*
C*					-13 = proj not set in GEMPLT	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/85						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	 4/89	Added LC_FLOC to get location		*
C* K. Brill/GSC          3/90   Added DG_KXKY and check againt KX and KY*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C* R. Tian/SAIC		10/02	Replace DG_KXKY with GQGPRJ		*
C* K. Brill/HPC		 4/04	Allow .005 slop in chck against grd bnds*
C************************************************************************
	CHARACTER*(*)	gpoint
C*
	REAL		rarr (2)
	CHARACTER	sep*1, gprj*4
C------------------------------------------------------------------------
	iret = 0
	rgx  = 0.
	rgy  = 0.
C
C*	Query grid projection from GEMPLT
C
	CALL GQGPRJ ( gprj, ag1, ag2, ag3, kx, ky, aglt1, agln1,
     +                aglt2, agln2, ier )

	IF ( ier .ne. 0 ) THEN
          CALL ER_WMSG ( 'GEMPLT', ier, ' ', ierr )
	  iret = -13
	  RETURN
	END IF
C
C*	Get location if no @ is used.
C
	IF  ( gpoint (1:1) .ne. '@' )  THEN
	    CALL LC_FLOC  ( gpoint, rlat, rlon, iret )
	    IF  ( iret .ne. 0 )  THEN
		iret = -12
		RETURN
	    END IF
	    CALL GTRANS  ( 'M', 'G', 1, rlat, rlon, rgx, rgy, ier )
	    IF  ( ier .ne. 0 )  THEN
		CALL ER_WMSG ( 'GEMPLT', ier, ' ', ierr )
		iret = -12
		RETURN
	    END IF
	  ELSE
C
C*	    Check whether user used ; or / as a separator.
C
	    islash = INDEX ( gpoint, '/' )
	    isemic = INDEX ( gpoint, ';' )
	    IF  ( isemic .ne. 0 )  THEN
		sep = ';'
	      ELSE IF  ( islash .ne. 0 )  THEN
		sep = '/'
	      ELSE
		iret = -12
		RETURN
	    END IF
C
C*	Translate rest of input as grid point.
C
	    CALL ST_RLST  ( gpoint (2:), sep, 0, 2, rarr, n, ier )
	    rgx = rarr (1)
	    rgy = rarr (2)
	    CALL GTRANS ( 'G', 'M', 1, rgx, rgy, rlat, rlon, ier )
	END IF
C
C*	Check that grid point values are valid.
C
	IF ( ABS (1. - rgx) .le. .005 ) rgx = 1.00000
	IF ( ABS (1. - rgy) .le. .005 ) rgy = 1.00000
	IF ( ABS (FLOAT(kx) - rgx) .le. .005 ) rgx = FLOAT (kx)
	IF ( ABS (FLOAT(ky) - rgy) .le. .005 ) rgy = FLOAT (ky)
	IF  ( ( rgx .lt. 1. ) .or. ( rgy .lt. 1. )   .or.
     +        ( rgx .gt. FLOAT ( kx ) )              .or.
     +        ( rgy .gt. FLOAT ( ky ) )            ) THEN
	    iret = -12
	END IF
C*
	RETURN
	END
