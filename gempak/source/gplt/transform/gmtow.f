	SUBROUTINE GMTOW  ( np, xm, ym, xw, yw, iret )
C************************************************************************
C* GMTOW								*
C*									*
C* This subroutine converts points in the map lat/lon sytem to		*
C* rotated lat/lon map coordinates via a coordinate rotation.		*
C*									*
C* GMTOW  ( NP, XM, YM, XW, YW, IRET )					*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	XM (NP)		REAL		X input coordinates		*
C*	YM (NP)		REAL		Y input coordinates		*
C*									*
C* Output parameters:							*
C*	XW (NP)		REAL		X output coordinates		*
C*	YW (NP)		REAL		Y output coordinates		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Brill/EMC		 3/96						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	REAL		xm (*), ym (*), xw (*), yw (*)
C*
C------------------------------------------------------------------------
	iret = 0
C
	IF  ( igmode .eq. 2 )  THEN
	    IF ( .not. gset ) THEN
	    	iret = NIPROJ
	    ELSE
C
C*	    	For graph mode just transfer input to output values.
C
	    	DO i = 1, np
		    xw (i) = xm (i)
		    yw (i) = ym (i)
	    	END DO
	    END IF
	 ELSE IF  ( igmode .eq. 1 )  THEN
C
C*	    Map mode.
C

	    IF  ( .not. mset ) THEN
	        iret = NIPROJ
C*
	      ELSE
	        CALL GMTOWQ ( am2w, .false., anglr2,
     +			      np, xm, ym, xw, yw, iret )

	    END IF
	  ELSE
	    iret = NIMODE
	END IF
C*
	RETURN
	END
