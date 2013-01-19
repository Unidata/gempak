	SUBROUTINE GGRFML  ( ixtyp, iytyp, np, xm, ym, bmlx1, bmlx0,
     +			     bmly1, bmly0, xl, yl, iret )
C************************************************************************
C* GGRFML								*
C* 									*
C* This subroutine converts points in graph coordinates into linear	*
C* intermediate coordinates.						*
C*									*
C* GGRFML  ( IXTYP, IYTYP, NP, XM, YM, BMLX1, BMLX0, BMLY1, BMLY0,	*
C*           XL, YL, IRET )						*
C*									*
C* Input parameters:							*
C*	IXTYP		INTEGER		X axis type			*
C*	IYTYP		INTEGER		Y axis type			*
C*	NP		INTEGER		Number of points		*
C*	XM (NP)		REAL		X coordinates			*
C*	YM (NP)		REAL		Y coordinates			*
C*	BMLX1		REAL		X scaling factor		*
C*	BMLX0		REAL		X offset			*
C*	BMLY1		REAL		Y scaling factor		*
C*	BMLY0		REAL		Y offset			*
C*									*
C* Output parameters:							*
C*	XL (NP)		REAL		X coordinates			*
C*	YL (NP)		REAL		Y coordinates 			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	10/86	Added polar coordinates			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* K. Brill/NMC		 7/90	SkewT offset multiplied by aspect ratio *
C* M. desJardins/GSFC	 9/90	Use constants in GEMPRM.PRM		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	REAL		xl (*), yl (*), xm (*), ym (*)
C-------------------------------------------------------------------------
	iret = NORMAL
C
C*	Do transform for polar coordinates.  These are done separately
C*	from the other coordinate types since the x and y values cannot
C*	be computed independently.
C
	IF  ( ( ixtyp .eq. 5 ) .and. ( iytyp .eq. 5 ) )  THEN
	    DO  i = 1, np
		CALL RTTOXY  ( xm (i), ym (i), x, y, ier )
		xl (i) = bmlx1 * x + bmlx0
		yl (i) = bmly1 * y + bmly0
	    END DO
	    RETURN
	END IF
C
C*	Do transform for Y axis then for X axis.
C*	Order is important for skew-T plots.
C
	IF  ( iytyp .eq. 1 )  THEN
	    DO i = 1, np
	    	yl (i)  = bmly1 * ym (i) + bmly0
	    END DO
C
	  ELSE IF  ( iytyp .eq. 2 )  THEN
	    DO i = 1, np
		IF  ( ym (i) .gt. 0.0 )  THEN
		    yl (i)  =  bmly1 * ALOG ( ym (i) ) + bmly0
		  ELSE
		    yl (i)  =  0.0
		END IF
	    END DO
C
	  ELSE IF  ( iytyp .eq. 3 )  THEN
	    DO i = 1, np
		IF  ( ym (i) .ge. 0.0 )  THEN
		    yl (i)  =  bmly1 * ym (i) ** RKAPPA + bmly0
		  ELSE
		    yl (i)   = 0.0
		END IF
	    END DO
	END IF
C
	IF  ( ixtyp .eq. 1 )  THEN
	    DO i = 1, np
		xl (i)  =  bmlx1 * xm (i) + bmlx0
	    END DO
C
	  ELSE IF  ( ixtyp .eq. 2 )  THEN
	    DO i = 1, np
		IF  ( xm (i) .gt. 0. )  THEN
		    xl (i) = bmlx1 * ALOG ( xm (i) ) + bmlx0
		  ELSE
		    xl (i) = 0.0
		END IF
	    END DO
C
	  ELSE IF ( ixtyp .eq. 3 )  THEN
	    DO i = 1, np
		IF  ( xm (i) .ge. 0.0 )  THEN
		    xl (i)  = bmlx1 * xm (i) ** RKAPPA + bmlx0
		  ELSE
		    xl (i) = 0.0
		END IF
	    END DO
C
	  ELSE IF  ( ixtyp .eq. 4 )  THEN
	    DO i = 1, np
		xl (i) = bmlx1 * xm (i) + bmlx0 + yl (i) * yxgraf
	    END DO
C
	END IF
C*
	RETURN
	END
