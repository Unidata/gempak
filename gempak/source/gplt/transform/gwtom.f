	SUBROUTINE GWTOM  ( np, xw, yw, xm, ym, iret )
C************************************************************************
C* GWTOM								*
C*									*
C* This subroutine converts points in the rotated map lat/lon sytem to	*
C* actual lat/lon map coordinates via a coordinate rotation.		*
C*									*
C* GWTOM  ( NP, XW, YW, XM, YM, IRET )					*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	XW (NP)		REAL		X input coordinates		*
C*	YW (NP)		REAL		Y input coordinates		*
C*									*
C* Output parameters:							*
C*	XM (NP)		REAL		X output coordinates		*
C*	YM (NP)		REAL		Y output coordinates		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Brill/EMC		 3/96						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	REAL		xw (*), yw (*), xm (*), ym (*)
C*
C------------------------------------------------------------------------
	iret = NORMAL
C
	IF  ( igmode .eq. 2 )  THEN
	    IF ( .not. gset ) THEN
		iret = NIPROJ
	    ELSE
C
C*	        For graph mode just transfer input to output values.
C
	        DO i = 1, np
		    xm (i) = xw (i)
		    ym (i) = yw (i)
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
	        CALL GMTOWQ ( am2w, .true., anglr2,
     +			      np, xw, yw, xm, ym, iret )

	    END IF
	  ELSE
	    iret = NIMODE
	END IF
C*
	RETURN
	END
