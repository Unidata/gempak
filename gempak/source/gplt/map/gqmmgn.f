	SUBROUTINE GQMMGN  ( xl, yb, xr, yt, iret )
C************************************************************************
C* GQMMGN								*
C*									*
C* This subroutine returns the current margins used in map mode.  The	*
C* value returned is that originally specified as a fraction of view	*
C* region or text size multiple; subsequent changes in view region or	*
C* text size do not affect the margin size.				*
C*									*
C* GQMMGN  ( XL, YB, XR, YT, IRET )					*
C*									*
C* Output parameters:							*
C*	XL		REAL		Left margin size		*
C*	YB		REAL		Bottom margin size		*
C*	XR		REAL		Right margin size		*
C*	YT		REAL		Top margin size			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	 9/84	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'XYDEF.CMN'
C------------------------------------------------------------------------
C*	Check for graph mode
C
	IF ( igmode .ne. 1 ) THEN
	    iret = NIMODE
	  ELSE
C
C*	    Retrieve the values from the common block.
C
	    iret = NORMAL
	    xl   = xlmmgn
	    yb   = ybmmgn
	    xr   = xrmmgn
	    yt   = ytmmgn
	END IF
C*
	RETURN
	END
