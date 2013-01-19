	SUBROUTINE GSATPX ( sys, x, y, offx, offy, imgfil, iarea, mode, 
     &			    ipix, dxo, dyo, iret )
C************************************************************************
C* GSATPX								*
C*									*
C* This subroutine gets a pixel value from an image file.		*
C*									*
C* GSATPX ( SYS, X, Y, OFFX, OFFY, IMGFIL, IAREA, MODE, 		*
C*          IPIX, DXO, DYO, IRET )					*
C*									*
C* Input parameters:							*
C*	SYS		CHAR*		Coordinate system		*
C*					  'S' = screen coordinates      *
C*					  'D' = device coordinates      *
C*					  'N' = normalized coordinates  *
C*					  'V' = view coordinates        *
C*					  'P' = plot coordinates        *
C*					  'M' = map coordinates         *
C*					  'G' = grid coordinates        *
C*	X		REAL		X coordinates / latitudes	*
C*	Y		REAL		Y coordinates / longitudes	*
C*	OFFX            INTEGER         Image pixel offset, x		*
C*	OFFY            INTEGER         Image pixel offset, y		*
C*	IMGFIL		CHAR*		Image file name 		*
C*	IAREA		INTEGER		Pixel area indicator		*
C*	MODE		INTEGER		Pixel mode indicator		*
C*									*
C* Output parameters:							*
C*	IPIX		INTEGER		Pixel value			*
C*	DXO             FLOAT		Device coordinate X at offset	*
C*	DYO             FLOAT		Device coordinate Y at offset	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* T. Lee/GSC		 9/99	Created					*
C* T. Lee/GSC		12/99	Added pixel area and mode retrieval	*
C* D.W.Plummer/NCEP	 5/03	Apply nint function to D coords		*
C* D.W.Plummer/NCEP	 6/04	Added offx,offy and (dxo,dyo)		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C*
	CHARACTER*(*) 	sys, imgfil
C*
	CHARACTER	sysuc*1
	REAL		xin (2), yin (2), xout (2), yout (2)
C------------------------------------------------------------------------
C*	Check that a device has been set.
C
	IF  (  ddev .ne. ' ' ) THEN
	    iret = NORMAL
	  ELSE
	    iret = NDVICE
	    RETURN
	END IF
C
C*	Convert input to device coordinates. 
C
	CALL ST_LCUC ( sys, sysuc, ier )
	isys = INDEX ( sysup, sysuc )
	IF  ( isys .eq. 0 )  THEN
	    iret = NOCORD
	    RETURN
	  ELSE
	    CALL GTRANS  ( sysuc, 'D', 1, x, y, dx, dy, ier )
	END IF
C
C*	Get bounding values of image display region and convert to
C*	device coordinates.
C
	CALL GQBND ( 'P', xl, yb, xr, yt, iret )
	IF ( iret .ne. NORMAL )  THEN
	    RETURN
	  ELSE
	    xin (1) = xl
	    yin (1) = yb
	    xin (2) = xr
	    yin (2) = yt
	END IF
C
	CALL GTRANS ( 'P', 'D', 2, xin, yin, xout, yout, iret )
	IF ( iret .ne. NORMAL )  THEN
	    RETURN
	  ELSE
	    ixout0 = nint ( xout (1) )
	    iyout0 = nint ( yout (1) )
	    ixout1 = nint ( xout (2) )
	    iyout1 = nint ( yout (2) )
	END IF
C
C*	Get the pixel values.
C
	CALL DSATPX ( dx, dy, offx, offy, imgfil, 
     +                ixout0, iyout0, ixout1, iyout1, 
     +		      iarea, mode, ipix, dxo, dyo, iret )
C
	RETURN
	END
