	SUBROUTINE UPDGDG  ( iret )
C************************************************************************
C* UPDGDG								*
C* 									*
C* This subroutine updates the common areas for graphs in the G 	*
C* coordinate system.							*
C* 									*
C* UPDGDG ( IRET )							*
C* 									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	9/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	6/86	Corrected computations of all scalings	*
C* M. desJardins/GSFC	10/86	Added polar coordinate system		*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'XYDEF.CMN'
	INCLUDE		'GEMPRM.PRM'
C-------------------------------------------------------------------------
C*	Do computations for graph mode.
C
	IF  ( ( igmode .eq. 2 ) .and. ggset ) THEN
C*
	    IF  ( ( jgytyp .eq. 1 ) .or. ( jgytyp .eq. 5 ) ) THEN
		yb = gybmg
		yt = gytmg
	      ELSE IF  ( jgytyp .eq. 2 )  THEN
		yb = ALOG ( gybmg )
		yt = ALOG ( gytmg )
	      ELSE IF  ( jgytyp .eq. 3 )  THEN
		yb = gybmg ** RKAPPA
		yt = gytmg ** RKAPPA
	    END IF
C
	    IF  ( ( jgxtyp .eq. 1 ) .or. ( jgxtyp .eq. 5 ) ) THEN
		xl = gxlmg
		xr = gxrmg
	      ELSE IF  ( jgxtyp .eq. 2 )  THEN
		xl = ALOG ( gxlmg )
		xr = ALOG ( gxrmg )
	      ELSE IF  ( jgxtyp .eq. 3 )  THEN
		xl = gxlmg ** RKAPPA
		xr = gxrmg ** RKAPPA
	    END IF
C
C*	    Save grid to linear ( 0. - 1. ) scaling terms for grid coord sys.
C
	    gxx1 = 1.0 / ( gpxr - gpxl )
	    gxx0 =   - ( gpxl * gxx1 )
	    gyy1 = 1.0 / ( gpyt - gpyb )
	    gyy0 =   - ( gpyb * gyy1 )
C
C*	    Compute scaling from (0.-1.) grid sys to values at points.
C
	    gglx1 = 1.0 / ( xr - xl )
	    gglx0 = - (gglx1 * xl)
	    ggly1 = 1.0 / ( yt - yb )
	    ggly0 = - ( ggly1 * yb )
	END IF
C*
	iret = 0
C*
	RETURN
	END
