	SUBROUTINE GQGGRF ( ixtyp, iytyp, kx, ky, xll, yll, xur, yur,
     +			    iret )
C************************************************************************
C* GQGGRF								*
C* 									*
C* This subroutine returns the current coordinate system definition	*
C* for a grid which is evenly spaced in a graph coordinate system.	*
C* The grid coordinate system is defined by GSGGRF.			*
C*									*
C* GQGGRF  ( IXTYP, IYTYP, KX, KY, XLL, YLL, XUR, YUR, IRET )		*
C*									*
C* Output parameters:							*
C*	IXTYP		INTEGER		X coordinate type 		*
C*					  1 = linear			*
C*					  2 = logarithmic		*
C*					  3 = ** KAPPA (2/7)		*
C*					  5 = polar (R)			*
C*	IYTYP		INTEGER		Y coordinate type		*
C*					  1 = linear			*
C*					  2 = logarithmic		*
C*					  3 = ** KAPPA (2/7)		*
C*					  5 = polar (THETA)		*
C*	KX		INTEGER		Number of x grid points		*
C*	KY		INTEGER		Number of y grid points		*
C*	XLL 		REAL		Lower left X value		*
C*	YLL		REAL		Lower left Y value		*
C*	XUR		REAL		Upper right X value		*
C*	YUR		REAL		Upper right Y value		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Changed calling sequence to have kx,ky	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C------------------------------------------------------------------------
C*	Check for graph mode
C
	IF ( igmode .ne. 2 ) THEN
	    iret = NIMODE
C
C*	  Check for graph defined.
C
	  ELSE IF ( .not. ggset ) THEN
	    iret = NOGRAF
	  ELSE IF  ( ( gpxl .ne. 1. ) .or. ( gpyb .ne. 1. ) )  THEN
	    iret = NIPBND
	  ELSE
	    iret   = NORMAL
	    ixtyp  = jgxtyp
	    iytyp  = jgytyp
	    kx     = gpxr
	    ky     = gpyt
	    xll    = gxlmg
	    yll    = gybmg
	    xur    = gxrmg
	    yur    = gytmg
	END IF
C*
	RETURN
	END
