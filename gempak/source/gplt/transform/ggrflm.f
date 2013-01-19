	SUBROUTINE GGRFLM  ( ixtyp, iytyp, np, xl, yl, bmlx1, bmlx0,
     +			     bmly1, bmly0, xm, ym, iret )
C************************************************************************
C* GGRFLM								*
C* 									*
C* This subroutine converts graph linear intermediate coordinates	*
C* into map/graph coordinates.						*
C* 									*
C* GGRFLM  ( IXTYP, IYTYP, NP, XL, YL, BMLX1, BMLX0, BMLY1, BMLY0,	*
C*           XM, YM, IRET )						*
C*									*
C* Input parameters:							*
C*	IXTYP		INTEGER		X axis type			*
C*	IYTYP		INTEGER		Y axis type			*
C*	NP		INTEGER		Number of points		*
C* 	XL (NP)		REAL		X coordinates 			*
C* 	YL (NP)		REAL		Y coordinates			*
C*	BMLX1		REAL		X axis graph scaling factor	*
C*	BMLX0		REAL		X axis graph offset 		*
C*	BMLY1		REAL		Y axis graph scaling factor	*
C*	BMLY0		REAL		Y axis graph scaling factor	*
C* 									*
C* Output parameters: 							*
C*	XM (NP)		REAL		X coordinates			*
C*	YM (NP)		REAL		Y coordinates			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 7/84	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	10/86	Added polar coordinates			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* K. Brill/NMC          7/90   SkewT offset multiplied by aspect ratio	*
C* M. desJardins/GSFC	 9/90	Use constants in GEMPRM.PRM		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE         'XYDEF.CMN'
C*
	REAL		xl (*), yl (*), xm (*), ym (*)
C--------------------------------------------------------------------------
	iret = NORMAL
C
C*	Do transforms for polar coordinates.
C
	IF  ( ( ixtyp .eq. 5 ) .and. ( iytyp .eq. 5 ) )  THEN
	    DO  i = 1, np
		x = ( xl (i) - bmlx0 ) / bmlx1
		y = ( yl (i) - bmly0 ) / bmly1
		CALL XYTORT  ( x, y, r, theta, ier )
		xm (i) = r
		ym (i) = theta
	    END DO
	    RETURN
	END IF
C
C*	Do transform for x axis, then for y axis.
C
	IF  ( ixtyp .eq. 1 )  THEN
	    DO  i = 1, np
		xm(i) =  ( xl(i) - bmlx0 ) / bmlx1
	    END DO
C*
	  ELSE IF  ( ixtyp .eq. 2 )  THEN
	    DO  i = 1, np
		xm (i)  =  EXP  ( ( xl(i) - bmlx0 ) / bmlx1 )
	    END DO
C*
	  ELSE IF  ( ixtyp .eq. 3 ) THEN
	    DO  i = 1, np
		xm (i)  =  ( ( xl(i) - bmlx0 ) / bmlx1 ) ** AKAPPA
	    END DO
C*
	  ELSE IF  ( ixtyp .eq. 4 ) THEN
	    DO  i = 1, np
	        yoffst  =  yl (i) * yxgraf
		xm (i)  =  ( ( xl(i) - yoffst ) - bmlx0 ) / bmlx1
	    END DO
	END IF
C*
	IF  ( iytyp .eq. 1 )  THEN
	    DO  i = 1, np
		ym (i)  =  ( yl(i) - bmly0 ) / bmly1
	    END DO
C*
	  ELSE IF  ( iytyp .eq. 2  ) THEN
	    DO  i = 1, np
		ym (i)  =  EXP  ( ( yl(i) - bmly0 ) / bmly1 )
	    END DO
C*
	  ELSE IF  ( iytyp .eq. 3  ) THEN
	    DO  i = 1, np
		ym (i)  =  ( ( yl(i) - bmly0 ) / bmly1 ) ** AKAPPA
	    END DO
	END IF
C*
	RETURN
	END
