	SUBROUTINE GQGMGN  ( xl, yb, xr, yt, iret )
C************************************************************************
C* GQGMGN								*
C* 									*
C* This subroutine returns the current margins for the graph mode of	*
C* the map/graph coordinate system.  The value returned is that		*
C* originally specified as a fraction of the view region or a multiple  *
C* of the text size; subsequent changes in view region or text size do 	*
C* not affect the value.						*
C* 									*
C* GQGMGN  ( XL, YB, XR, YT, IRET )					*
C*									*
C* Output parameters:							*
C* 	XL		REAL		Left margin size		*
C* 	YB		REAL		Bottom margin size		*
C* 	XR		REAL		Right margin size		*
C* 	YT		REAL		Top margin size 		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	 9/84	GEMPLT Version 3.0			*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'XYDEF.CMN'
C------------------------------------------------------------------------
C*	Check for graph mode.
C
	IF  ( igmode .ne. 2 )  THEN
	    iret = NIMODE
	  ELSE
C
C*	    Retrieve the values from the common block.
C
	    iret = NORMAL
	    xl   = xlgmgn
	    yb   = ybgmgn
	    xr   = xrgmgn
	    yt   = ytgmgn
	END IF
C*
	RETURN
	END
