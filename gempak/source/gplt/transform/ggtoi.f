	SUBROUTINE GGTOI  ( np, xg, yg, xm, ym, iret )
C************************************************************************
C* GGTOI								*
C*									*
C* This subroutine converts points in the grid coordinate system into	*
C* values in the grid linear intermediate coordinate system.		*
C*									*
C* GGTOI  ( NP, XG, YG, XM, YM, IRET )					*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	XG (NP)		REAL		X input coordinates		*
C*	YG (NP)		REAL		Y input coordinates		*
C*									*
C* Output parameters:							*
C*	XM (NP)		REAL		X output coordinates		*
C*	YM (NP)		REAL		Y output coordinates		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	10/86	Added polar coordinates			*
C* M. desJardins/GSFC	 2/87	Fixed polar coords when graph not set	*
C* M. desJardins/GSFC	 1/88	Fixed missing data			*
C* M. desJardins/GSFC	 6/88	Adapted from GGTOM			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	REAL		xg (*), yg (*), xm (*), ym (*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Transform grid to linear intermediate coordinates for map mode.
C
	IF  ( igmode .eq. 1 )  THEN
C*
	    IF  ( .not. mgset )  THEN
		iret = NIPROJ
		RETURN
	    END IF
C*
	    iret = NORMAL
	    DO  i = 1, np
		IF  ( ( .not. ERMISS ( xg (i) ) ) .and.
     +		      ( .not. ERMISS ( yg (i) ) ) )  THEN
		    xm (i) = aglx1 * xg (i) + aglx0
		    ym (i) = agly1 * yg (i) + agly0
		  ELSE
		    xm (i) = RMISSD
		    ym (i) = RMISSD
		END IF
	    END DO
C
C*	    Convert to linear coordinates for graph mode.
C
	  ELSE IF  ( igmode .eq. 2 )  THEN
C*
	    IF  ( .not. ggset )  THEN
		iret = NIPROJ
		RETURN
	    END IF
C*
	    iret = NORMAL
	    DO  i = 1, np
		IF  ( ( .not. ERMISS ( xg (i) ) ) .and.
     +		      ( .not. ERMISS ( yg (i) ) ) )  THEN
		    xm (i) = gxx1 * xg (i) + gxx0
		    ym (i) = gyy1 * yg (i) + gyy0
		  ELSE
		    xm (i) = RMISSD
		    ym (i) = RMISSD
		END IF
	    END DO
	END IF
C*
	RETURN
	END
