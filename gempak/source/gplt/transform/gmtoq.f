	SUBROUTINE GMTOQ  ( np, xm, ym, xq, yq, iret )
C************************************************************************
C* GMTOQ								*
C*									*
C* This subroutine converts lat/lon points to rotated lat/lon grid	*
C* coordinates.								*
C*									*
C* GMTOQ  ( NP, XM, YM, XQ, YQ, IRET )					*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	XM (NP)		REAL		X input coordinates		*
C*	YM (NP)		REAL		Y input coordinates		*
C*									*
C* Output parameters:							*
C*	XQ (NP)		REAL		X output coordinates		*
C*	YQ (NP)		REAL		Y output coordinates		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Brill/EMC		 3/96						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	REAL		xm (*), ym (*), xq (*), yq (*)
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
		    xq (i) = xm (i)
		    yq (i) = ym (i)
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
	        CALL GMTOWQ ( gm2q, .false., gangr2,
     +			      np, xm, ym, xq, yq, iret )

	    END IF
	  ELSE
	    iret = NIMODE
	END IF
C*
	RETURN
	END
