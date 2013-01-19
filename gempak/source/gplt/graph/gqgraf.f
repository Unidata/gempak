	SUBROUTINE GQGRAF ( ixtyp, iytyp, yszxsz, xlm, ybm, xrm, ytm,
     +	                    iret )
C************************************************************************
C* GQGRAF								*
C*									*
C* This subroutine returns the current coordinate system definition 	*
C* for the graph plotting mode of the map/graph coordinate system.	*
C*									*
C* GQGRAF  ( IXTYP, IYTYP, YSZXSZ, XLM, YBM, XRM, YTM, IRET )		*
C* 									*
C* Output parameters:							*
C* 	IXTYP		INTEGER		X coordinate type		*
C*					   1 = linear			*
C*					   2 = logarithmic		*
C*					   3 = ** kappa (2 / 7)		*
C*					   4 = skew			*
C*					   5 = polar (R)		*
C* 	IYTYP		INTEGER		Y coordinate type 		*
C*					   1 = linear			*
C*					   2 = logarithmic		*
C*					   3 = ** kappa (2 / 7)		*
C*					   5 = polar (THETA)		*
C* 	YSZXSZ		REAL		Height to width ratio of plot	*
C* 	XLM		REAL		Left limit of X axis 		*
C* 	YBM		REAL		Bottom limit of Y axis 		*
C* 	XRM		REAL		Right limit of X axis 		*
C* 	YTM		REAL		Top limit of Y axis 		*
C* 	IRET 		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	 9/84	GEMPLT Version 3.0			*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	10/86	Cleaned up				*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'XYDEF.CMN'
C------------------------------------------------------------------------
C*	Check for graph mode.
C
	IF  ( igmode .ne. 2 )  THEN
	    iret = NIMODE
C
C*	  Check for graph defined.
C
	  ELSE IF  ( .not. gset )  THEN
	    iret = NOGRAF
C
C*	   Return values from common.
C
	  ELSE
	    iret   = NORMAL
	    ixtyp  = jxtyp
	    iytyp  = jytyp
	    yszxsz = yxgraf
	    xlm    = xlmg
	    ybm    = ybmg
	    xrm    = xrmg
	    ytm    = ytmg
	END IF
C*
	RETURN
	END
