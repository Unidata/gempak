	SUBROUTINE GQBND  ( sys, xl, yb, xr, yt, iret )	
C************************************************************************
C* GQBND								*
C*									*
C* This subroutine returns the boundaries of the specified 		*
C* coordinate system.  For the linear coordinate systems (D,N,V,P),	*
C* the lower left and upper right corners are returned.  For M		*
C* coordinates, an estimate of the minimum and maximum range of		*
C* latitude and longitude in the plot are returned.  For G		*
C* coordinates, the minimum and maximum grid points which will		*
C* be displayed in the plot area are returned.				*
C*									*
C* GQBND  ( SYS, XL, YB, XR, YT, IRET )					*
C*									*
C* Input parameters:							*
C*	SYS		CHAR*		Coordinate system		*
C*					  'S' = screen coordinates	*
C*					  'D' = device coordinates	*
C*					  'N' = normalized coordinates	*
C*					  'V' = view coordinates	*
C*					  'P' = plot coordinates	*
C*					  'M' = map coordinates		*
C*					  'G' = grid coordinates	*
C*									*
C* Output parameters:							*
C*	XL		REAL		Lower left x / latitude		*
C*	YB		REAL		Lower left y / longitude	*
C*	XR		REAL		Upper right x / latitude	*
C*	YT		REAL		Upper right y / longitude	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 8/86	Fixed error in computing grid bounds	*
C* M. desJardins/GSFC	 6/88	Fixed G bounds				*
C* C. Lin/EAI	 	 6/97	Added S bounds				*
C************************************************************************
	CHARACTER*(*)	sys
	CHARACTER	csys*1
C*
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Convert sys to upper case.
C
	CALL ST_LCUC  ( sys, csys, ier )
C
C*	Branch on coordinate system.  Retrieve boundaries from /XYDEF/.
C
	IF  ( csys .eq. 'S' )  THEN
	    xl  =  xbndls
	    yb  =  ybndbs
	    xr  =  xbndrs
	    yt  =  ybndts
C*
	  ELSE IF  ( csys .eq. 'D' )  THEN
	    xl  =  xbndld
	    yb  =  ybndbd
	    xr  =  xbndrd
	    yt  =  ybndtd
C*
	  ELSE IF  ( csys .eq. 'N' )  THEN
	    xl  =  xbndln
	    yb  =  ybndbn
	    xr  =  xbndrn
	    yt  =  ybndtn
C*
	  ELSE IF  ( csys .eq. 'V' )  THEN
	    xl  =  xbndlv
	    yb  =  ybndbv
	    xr  =  xbndrv
	    yt  =  ybndtv
C*
	  ELSE IF  ( csys .eq. 'P' ) THEN
	    xl  =  xbndlp
	    yb  =  ybndbp
	    xr  =  xbndrp
	    yt  =  ybndtp
C*
	  ELSE IF ( csys .eq. 'L' ) THEN
	    IF  ( ( igmode .eq. 1 ) .and. mset )  THEN
		xl = xmbndl
		yb = ymbndb
		xr = xmbndr
		yt = ymbndt
	      ELSE IF  ( ( igmode .eq. 2 ) .and. gset )  THEN
		xl = xgbndl
		yb = ygbndb
		xr = xgbndr
		yt = ygbndt
	      ELSE
		iret = NOBNDS
	    END IF
C*
	  ELSE IF  ( csys .eq. 'M' )  THEN
	    IF  ( ( igmode .eq. 1 ) .and. mset )  THEN
		xl = blats
		yb = blonw
		xr = blatn
		yt = blone
	      ELSE IF  ( ( igmode .eq. 2 ) .and.  gset )  THEN
		xl = xlmg
		yb = ybmg
		xr = xrmg
		yt = ytmg
	      ELSE
		iret = NOBNDS
	    END IF
C*
	  ELSE IF  ( csys .eq. 'G' )  THEN
	    IF  ( ( ( igmode .eq. 1 ) .and. mset .and. mgset ) .or.
     +		  ( ( igmode .eq. 2 ) .and. gset .and. ggset ) ) THEN
		CALL UPDGXY  ( xl, yb, xr, yt, iret )
	      ELSE
		iret = NOBNDS
	    END IF
C*
	  ELSE
	    iret = NOCORD
	END IF
C*
	RETURN
	END
