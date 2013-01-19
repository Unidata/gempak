	SUBROUTINE GQTOM  ( np, xq, yq, xm, ym, iret )
C************************************************************************
C* GQTOM								*
C*									*
C* This subroutine converts points in the rotated grid lat/lon sytem to	*
C* actual lat/lon coordinates via a coordinate rotation.		*
C*									*
C* GQTOM  ( NP, XQ, YQ, XM, YM, IRET )					*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	XQ (NP)		REAL		X input coordinates		*
C*	YQ (NP)		REAL		Y input coordinates		*
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
	REAL		xq (*), yq (*), xm (*), ym (*)
C*
C------------------------------------------------------------------------
	iret = NORMAL
C
	IF  ( igmode .eq. 2 )  THEN
	    IF ( .not. ggset ) THEN
		iret = NIPROJ
	    ELSE
C
C*	    	For graph mode just transfer input to output values.
C
	    	DO i = 1, np
		    xm (i) = xq (i)
		    ym (i) = yq (i)
	    	END DO
	    END IF
C
C*	  Map mode.
C
	 ELSE IF  ( igmode .eq. 1 )  THEN
C
	    IF  ( .not. mgset ) THEN
	        iret = NIPROJ
C*
	      ELSE
	        CALL GMTOWQ ( gm2q, .true., gangr2,
     +			      np, xq, yq, xm, ym, iret )

	    END IF
	  ELSE
	    iret = NIMODE
	END IF
C*
	RETURN
	END
