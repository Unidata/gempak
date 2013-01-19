	SUBROUTINE UPDGRF  ( iret )
C************************************************************************
C* UPDGRF								*
C* 									*
C* This subroutine updates the coordinate systems for graphs. 		*
C* 									*
C* UPDGRF ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 7/84						*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	10/86	Added polar coordinates			*
C* T. Lee/GSC		 9/97	Included ERROR.PRM			*
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'XYDEF.CMN'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
C-------------------------------------------------------------------------
	iret = NORMAL
C
C*	Do computations for graph mode.
C
	IF  ( ( igmode .eq. 2 ) .and. gset ) THEN
C*
	    IF  ( jytyp .eq. 1 ) THEN
		yb = ybmg
		yt = ytmg
	      ELSE IF ( jytyp .eq. 2 ) THEN
		yb = ALOG ( ybmg )
		yt = ALOG ( ytmg )
	      ELSE IF ( jytyp .eq. 3 ) THEN
		yb = ybmg ** RKAPPA
		yt = ytmg ** RKAPPA
	      ELSE IF ( jytyp .eq. 5 ) THEN
		yb = xlmg * SIN ( ybmg * DTR )
		yt = xrmg * SIN ( ytmg * DTR )
	    END IF
C
	    IF  ( jxtyp .eq. 1 ) THEN
		xl = xlmg
		xr = xrmg
	      ELSE IF ( jxtyp .eq. 2 ) THEN
		xl = ALOG ( xlmg )
		xr = ALOG ( xrmg )
	      ELSE IF ( jxtyp .eq. 3 ) THEN
		xl = xlmg ** RKAPPA
		xr = xrmg ** RKAPPA
	      ELSE IF ( jxtyp .eq. 4 ) THEN
		xl = xlmg
		xr = xrmg
	      ELSE IF ( jxtyp .eq. 5 ) THEN
		xl = xlmg * COS ( ybmg * DTR )
		xr = xrmg * COS ( ytmg * DTR )
	    END IF
C
C*	    Save graph linear bounds.
C
	    xgbndl = 0.0
	    ygbndb = 0.0
	    xgbndr = 1.0
	    ygbndt = 1.0
C
C*	    Compute M to L scaling factors.  Always scale  0.--->1.
C
	    amlx1 = 1.0 / ( xr - xl )
	    amlx0 = - ( amlx1 * xl )
	    amly1 = 1.0 / ( yt - yb )
	    amly0 = - ( amly1 * yb )
C
C*	    For polar graphs, set y to x scaling ratio.
C
	    IF ( jxtyp .eq. 5 ) THEN
		yxgraf = ( yt - yb ) / ( xr - xl )
	    END IF
	  ELSE
	    iret = NIPROJ
	END IF
C*
	RETURN
	END
