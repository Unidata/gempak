 	SUBROUTINE GMERLM  ( mproj, np, xl, yl, anglr1, xm, ym, iret ) 
C************************************************************************
C* GMERLM								*
C* 									*
C* This subroutine converts a point from x, y coordinates to		*
C* map coordinates for the general Mercator.  See Cylindrical class	*
C* for the normal Mercator projection.					*
C* 									*
C* GMERLM  ( MPROJ, NP, XL, YL, ANGLR1, XM, YM, IRET )			*
C*									*
C* Input parameters:							*
C*	MPROJ		INTEGER		Projection type ( not used )	*
C*	NP		INTEGER		Number of points		*
C* 	XL(*) 		REAL		X coordinates			*
C* 	YL(*) 		REAL		Y coordinates			*
C*	ANGLR1		REAL		Center longitude		*
C* 									*
C* Output parameters:							*
C* 	XM(*)		REAL		Latitudes			*
C* 	YM(*)		REAL		Longitudes			*
C*	IRET		INTEGER		Return code			*
C** 									*
C* Log:									*
C* B. Doty/RDS		 4/87	GEMPLT Version 4			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* K. Brill/NMC		 4/91	Check for ATAN ( 0, 0 )			*
C* K. Brill/NMC		10/92	Make it so input & output can be same	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		xm (*), ym (*), xl (*), yl (*)
C------------------------------------------------------------------------
	iret = NORMAL
C*
	DO  i = 1, np
	    xmm    = ASIN ( SIN ( yl (i) ) / COSH ( xl (i) ) ) 
	    xxx    = SINH ( xl (i) )
	    yyy    = COS  ( yl (i) )
	    IF ( xxx .eq. 0.0 .and. yyy .eq. 0.0 ) THEN
		ym (i) = anglr1
	    ELSE
	        ym (i) = anglr1 + ATAN2 ( xxx, yyy )
	    END IF
	    xm (i) = xmm    * RTD
	    ym (i) = ym (i) * RTD
	END DO
C*
	RETURN
	END
