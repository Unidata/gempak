	SUBROUTINE GROTTL  ( ctyp, invrs, np, x, y, iret )
C************************************************************************
C* GROTTL								*
C*									*
C* This subroutine rotates the linear coordinates on projection plane.	*
C*									*
C* GROTTL  ( CTYP, INVRS, NP, X, Y, IRET )				*
C*									*
C* Input parameters:							*
C*	CTYP		CHAR*1		Type of intermediate coord	*
C*					'G' = grid			*
C*					'M' = map			*
C*	INVRS		INTEGER		Indicator of inverse rotation	*
C*					 < 0 inverse rotation		*
C*					 = 0 forward rotation		*
C*					 > 0 forward rotation		*
C*	NP		INTEGER		Number of points		*
C*									*
C* Input and output parameters:						*
C*	X (NP)		REAL		X coordinates			*
C*	Y (NP)		REAL		Y coordinates			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Brill/EMC		 6/98	Rotate L coordinates for WAFS maps	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	CHARACTER*(*)	ctyp
	REAL		x (*), y (*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
	IF ( ctyp .eq. 'G' ) THEN
	    rcos = rcosgl
	    rsin = rsingl
	ELSE
	    rcos = rcosml
	    rsin = rsinml
	END IF
C
	IF ( invrs .ge. 0 ) THEN
	    a = rcos
	    b = rsin
	    c = -rsin
	    d = rcos
	ELSE
	    a = rcos
	    b = -rsin
	    c = rsin
	    d = rcos
	END IF
C
	DO i = 1, np
	    IF ( .not. ERMISS ( x(i) ) .and.
     +		 .not. ERMISS ( y(i) ) ) THEN
		xl = x (i)
		yl = y (i)
		x (i) = a * xl + b * yl
		y (i) = c * xl + d * yl
	    END IF
	END DO
C*
	RETURN
	END
